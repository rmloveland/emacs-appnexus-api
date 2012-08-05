(eval-when-compile (require 'cl))
(require 'json)
(require 'url)

(defgroup appnexus nil
  "An Emacs interface to the AppNexus Console API."
  :group 'processes
  :prefix "an-"
  :link '(url-link :tag "AppNexus API entry point."
		   "http://api.appnexus.com")
  :link '(url-link :tag "AppNexus Sandbox API entry point."
		   "http://api.sand-08.adnxs.net")
  :link '(url-link :tag "AppNexus API Documentation."
		   "https://wiki.appnexus.com/display/api/Home"))

(defcustom an-username nil
  "Your AppNexus username."
  :group 'appnexus
  :type '(string))

(defcustom an-password nil
  "Your AppNexus password."
  :group 'appnexus
  :type '(string))

(defvar *an-auth*
  `(:auth
    (:username ,an-username
	       :password ,an-password)))

(defvar *an-production-url* "http://api.appnexus.com")
(defvar *an-sandbox-url* "http://api.sand-08.adnxs.net")
(defvar *an-current-url* *an-sandbox-url*)

(defvar *an-debug* nil) ;; no-op right now, maybe just learn to use the debugger?

;; TODO: learn what the heck `put' does
(put 'an-api-error 'error-message "AppNexus API error")
(put 'an-api-error 'error-conditions '(appnexus-api-error error))

;; TODO: think about whether you'd like to use this code
(defun an-check-error (response)
  "Check to see if RESPONSE is an API error. If so, signal the error."
  (let ((status (gethash "status" response)))
    (unless (string-equal "OK" (gethash "response" status))
      (let ((id (gethash "error_id" status))
	    (type (gethash "error" status))
	    (description (gethash "error_description" status)))
	(signal 'an-api-error (list id type description))))))

(defun an-response (buffer)
  "Convert the JSON response in BUFFER into a hash table."
  (unwind-protect
      (with-current-buffer buffer
	(save-excursion
	  (goto-char url-http-end-of-headers)
	  (let ((json-key-type 'hash-table)
		(response (json-read-from-string (buffer-substring (point) (point-max)))))
	    response)))))

(defun an-request (verb path &optional payload)
  "Send a request to the AppNexus API service at PATH using a RESTful VERB.
The PAYLOAD will be a Lisp data structure that we convert into JSON. The
URL is a string."
  (if payload
      (let ((url-request-method verb)
	    (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
	    (url-request-data
	     (json-encode payload)))
	(an-response
	 (url-retrieve-synchronously
	  (concat *an-current-url* "/" path))))
    (let ((url-request-method verb)
	  (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (an-response
       (url-retrieve-synchronously
	(concat *an-current-url* "/" path))))))

(defun alist-to-query-params (alist)
  "This function is not being used right now."
  (mapconcat (lambda (x) x)
	     (mapcar (lambda (x) (concat (symbol-name (car x)) "="
					 (symbol-name (cadr x))))
		     alist) "&"))

(defun an-json-fields (filename)
  "This function is not finished."
  ;; In progress...
  (let* ((possible-values '(meta fields))
	(response (let ((json-object-type 'alist))
		    (assoc 'response (json-read-file filename))))
	(fields (car (remove-if #'null
				(mapcar (lambda (val)
					  (assoc val response))
					possible-values)))))
    fields))

(defun an-auth (&optional payload)
  "Authenticate with the API, and open the response in a temporary buffer."
  (interactive)
  (print-buf "*an-auth*"
	     (an-request "POST"
			 "auth"
			 (or payload
			     `(:auth (:username ,an-username :password ,an-password))))))

(defun print-buf (bufname thing)
  "Print THING to a temporary buffer BUFNAME."
  (with-output-to-temp-buffer bufname
    (print thing)))

(defun smart-print-buf (bufname thing mode)
  "Not finished yet. I need to finish it so I can have results buffers that retain undo information."
  (interactive)
  (let ((buf (generate-new-buffer bufname))
	(other-frame t))
    ;; setting mode is done before showing the new frame
    ;; because otherwise, we get a nasty animation effect
    (set-buffer buf)
    (funcall mode)
  (if other-frame
      (switch-to-buffer-other-window buf)
    (let ((one-buffer-one-frame-force one-buffer-one-frame-mode))
      ;; change window in case its unsuitable (dedicated or special display)
      (select-window (get-window-for-other-buffer))
      ;; force new frame
      (switch-to-buffer buf)))
  ;;(select-frame-set-input-focus (window-frame (selected-window)))))
  (setq buffer-offer-save t)
  (put 'buffer-offer-save 'permanent-local t)
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (print thing buf)))

(defun buf2json ()
  "Convert the current buffer to JSON, and open in a temp buffer."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat "*json-" (number-to-string (random 1000)) "*"))
	(mode 'js-mode))
    (smart-print-buf bufname (json-encode it) mode)))

(defun dirty-json ()
  "puts the backslashed quotes back in, for re-conversion to lisp"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "\"")
    (goto-char (point-max))
    (insert "\"")
    (goto-char (+ 1 (point-min)))
    ;; replace backslash quote with double backslash quote
    (while (re-search-forward "\\\\\"" (- (point-max) 1) t)
      (replace-match "\\\\\"" nil t))
    (goto-char (+ 1 (point-min)))
    ;; replace quote with backslash quote
    (while (re-search-forward "\"" (- (point-max) 1) t)
      (replace-match "\\\"" nil t))))

(defun clean-json ()
  "strip json.el-generated json string of its backslashed double quotes for other json parsers to use"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\"" nil t)
    (replace-match "" nil t)
    (goto-char (point-max))
    (re-search-backward "\"" nil t)
    (replace-match "" nil t)
    (goto-char (point-min))
    ;; first, replace the single backslashed quote with quote
    (while (re-search-forward "\\\\\"" nil t)
      (replace-match "\"" nil t))
    ;; then, on second pass, replace double backslashes with a single backslash
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\\\" nil t)
      (replace-match "\\" nil t))))

(defun buf2lsp ()
  "Convert the current buffer to Lisp, and open in a temp buffer."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat "*jlsp-" (number-to-string (random 1000)) "*"))
	(mode 'lisp-interaction-mode))
    (smart-print-buf bufname (json-read-from-string it) mode)
    ;; think about using `elisp-format-buffer'
    (switch-to-buffer bufname)))

(defun buf-do (verb service+params)
  "Send the HTTP request via VERB, with SERVICE+PARAMS."
  (interactive "sverb: \nsservice+params: ")
  (let ((payload (read (buffer-string))))
    (print-buf (concat "*" service+params "*")
	       (an-request verb
			   service+params
			   	   payload))))

(defun an-get (service+params)
  "Send a GET request to the specified SERVICE+PARAMS."
  (interactive "sservice+params: ")
  (print-buf (concat "*" service+params "*")
	     (an-request "GET"
			 service+params)))

(defun an-switchto (user-id)
  "Switch to another AppNexus API user. Only works if you're an admin."
  (interactive "suser-id: ")
  (print-buf "*an-switchto*"
	     (an-request "POST"
			 "auth"
			 `(:auth (:switch_to_user ,user-id)))))

(defun an-who ()
  "Find out what user you are; open in new buffer."
  (interactive)
  (print-buf "*an-who*" (an-request "GET" "user?current")))

(defun an-confluence-doc ()
  "Browse confluence 3.5 docs for symbol at point."
  (interactive)
  (browse-url-default-macosx-browser
   (concat "https://confluence.atlassian.com/dosearchsite.action?&searchQuery.spaceKey=CONF35&searchQuery.queryString=ancestorIds%3A252347565+AND+"
	   (symbol-name-before-point))))

(defun an-api-doc ()
  "search appnexus api docs for symbol at point"
  (interactive)
  (browse-url-default-macosx-browser
   (concat "https://wiki.appnexus.com/dosearchsite.action?searchQuery.spaceKey=api&searchQuery.queryString=ancestorIds%3A27984339+AND+" (symbol-name-before-point))))

(defun an-auth-credentials (username)
  (interactive "susername: ")
  (setq an-username username)
  (setq an-password (read-passwd "password: ")))

;; (defun an-generate-curl-string (service+params &optional verb payload-file)
;;   (interactive))

(defun an-print-current-url ()
  (interactive)
  (message "current api url is %s" *an-current-url*))

(defun an-toggle-sand-or-prod-url ()
  "toggle sand or prod url; todo: authenticate, too?"
  (interactive)
  (if (string-equal *an-current-url* *an-sandbox-url*)
      (setq *an-current-url* *an-production-url*)
    (setq *an-current-url* *an-sandbox-url*)))

(global-set-key (kbd "C-x C-A A") 'an-auth)
(global-set-key (kbd "C-x C-A a") 'an-auth-credentials)
(global-set-key (kbd "C-x C-A S") 'an-switchto)

(global-set-key (kbd "C-x C-A W") 'an-who)
(global-set-key (kbd "C-x C-A w") 'an-print-current-url)
(global-set-key (kbd "C-x C-A T") 'an-toggle-sand-or-prod-url)

(global-set-key (kbd "C-x C-A J") 'buf2json)
(global-set-key (kbd "C-x C-A L") 'buf2lsp)

(global-set-key (kbd "C-x C-A P") 'buf-do)
(global-set-key (kbd "C-x C-A G") 'an-get)

(global-set-key (kbd "C-x C-A C") 'an-confluence-doc)
(global-set-key (kbd "C-x C-A D") 'an-api-doc)

(global-set-key (kbd "C-x C-A E") 'clean-json)
(global-set-key (kbd "C-x C-A I") 'dirty-json)

(provide 'appnexus)
