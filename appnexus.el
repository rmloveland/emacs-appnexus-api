(eval-when-compile (require 'cl))
(require 'json)
(require 'url)

(defgroup appnexus nil
  "Functions that allow for easy interaction with AppNexus' Console API."
  :group 'processes
  :prefix "an-"
  :link '(url-link :tag "AppNexus API entry point."
		   "http://api.appnexus.com")
  :link '(url-link :tag "AppNexus Sandbox API entry point."
		   "http://api.sand-08.adnxs.net")
  :link '(url-link :tag "AppNexus API Documentation."
		   "https://wiki.appnexus.com/display/api/Home"))

(defcustom an-username nil
  "Your AppNexus API username."
  :group 'appnexus
  :type '(string))

(defcustom an-password nil
  "Your AppNexus API password."
  :group 'appnexus
  :type '(string))

(defvar *an-auth*
  `(:auth
    (:username ,an-username
	       :password ,an-password)))

(defvar *an-production-url* "http://api.appnexus.com")
(defvar *an-sandbox-url* "http://api.sand-08.adnxs.net")
(defvar *an-current-url* *an-sandbox-url*)

;; FIXME: Applying `url.el' settings here is hacky
(setq url-cookie-trusted-urls '(".*adnxs\.net" ".*appnexus\.com"))

;; FIXME: learn what the heck `put' does
(put 'an-api-error 'error-message "AppNexus API error")
(put 'an-api-error 'error-conditions '(appnexus-api-error error))

;; FIXME: think about whether you'd like to use this code
(defun an-check-error (response)
  "Check to see if RESPONSE is an API error. If so, signal the error."
  (let ((status (gethash "status" response)))
    (unless (string-equal "OK" (gethash "response" status))
      (let ((id (gethash "error_id" status))
	    (type (gethash "error" status))
	    (description (gethash "error_description" status)))
	(signal 'an-api-error (list id type description))))))

(defun an-response (buffer)
  "Convert the JSON response left by `an-request' in BUFFER into a hash table.
Then, return that hash table."
  (unwind-protect
      (with-current-buffer buffer
	(save-excursion
	  (goto-char url-http-end-of-headers)
	  (let ((json-key-type 'hash-table)
		(response (json-read-from-string (buffer-substring (point) (point-max)))))
	    response)))))

(defun an-request (verb path &optional payload)
  "Send an HTTP request, VERB, to the service at PATH with PAYLOAD in tow.
If it exists, PAYLOAD will be a Lisp data structure that we convert into
JSON on the fly before making the request."
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
  "Convert an alist of URL query parameters into a query string.
This function is not currently being used anywhere, and may be
removed."
  (mapconcat (lambda (x) x)
	     (mapcar (lambda (x) (concat (symbol-name (car x)) "="
					 (symbol-name (cadr x))))
		     alist) "&"))

(defun an-json-fields (filename)
  "Gather a list of JSON fields available through the API `meta' service.
This function is unfinished. Currently takes a filename argument, but should
accept a URL in future."
  ;; FIXME: Finish writing this function
  (let* ((possible-values '(meta fields))
	(response (let ((json-object-type 'alist))
		    (assoc 'response (json-read-file filename))))
	(fields (car (remove-if #'null
				(mapcar (lambda (val)
					  (assoc val response))
					possible-values)))))
    fields))

(defun an-auth (&optional payload)
  "Authenticate with the API and open the response in a temporary buffer."
  (interactive)
  (smart-print-buf "*an-auth*"
	     (an-request "POST"
			 "auth"
			 (or payload
			     `(:auth (:username ,an-username :password ,an-password))))
	     'emacs-lisp-mode))

(defun print-buf (bufname thing)
  "Print THING to a temporary buffer named BUFNAME.
This function is obsoleted by `smart-print-buf', and will be removed."
  (with-output-to-temp-buffer bufname
    (print thing)))

(defun smart-print-buf (bufname thing mode)
  "Pop open a buffer BUFNAME containing THING in your Emacs MODE of choice."
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
  "Convert the current buffer from Elisp to the heavily escaped JSON string
format preferred by `json.el'. Open the results in a new buffer."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat "*json-" (number-to-string (random 1000)) "*"))
	(mode 'js-mode))
    (smart-print-buf bufname (json-encode it) mode)))

(defun dirty-json ()
  "Convert a buffer of standard JSON to the escaped JSON string format
preferred by `json.el', and pop open a new buffer with the contents. This
is an intermediate step for re-conversion to Elisp."
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
      (replace-match "\\\"" nil t))
    (goto-char (+ 1 (point-min)))
    ;; remove newlines and space characters, since our json parser
    ;; can't understand them and throws an error
    (while (re-search-forward "\n +" (- (point-max) 1) t)
      (replace-match "" nil t))))

(defun clean-json ()
  "Convert a buffer of the escaped JSON strings preferred by `json.el' into
standard  JSON, and pop open a new buffer with the contents."
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
  "Convert the `json.el' escaped JSON string in the current buffer to Elisp,
and open the results in a new temp buffer."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat "*jlsp-" (number-to-string (random 1000)) "*"))
	(mode 'emacs-lisp-mode))
    (smart-print-buf bufname (json-read-from-string it) mode)
    ;; think about using `elisp-format-buffer'
    (switch-to-buffer bufname)))

(defun buf-do (verb service+params)
  "Send a request, VERB, to SERVICE+PARAMS."
  (interactive "sverb: \nsservice+params: ")
  (let ((payload (read (buffer-string))))
    (smart-print-buf (concat "*" service+params "*")
	       (an-request verb
			   service+params
			   	   payload)
	       'emacs-lisp-mode)))

(defun an-get (service+params)
  "Send a standard GET request to SERVICE+PARAMS."
  (interactive "sservice+params: ")
  (smart-print-buf (concat "*" service+params "*")
	     (an-request "GET"
			 service+params)
	     'emacs-lisp-mode))

(defun an-switchto (user-id)
  "Switch to another API user. This function will only work if you're an
admin user."
  (interactive "suser-id: ")
  (smart-print-buf "*an-switchto*"
	     (an-request "POST"
			 "auth"
			 `(:auth (:switch_to_user ,user-id)))
	     'emacs-lisp-mode))

(defun an-who ()
  "Find out what user you are; open in new buffer."
  (interactive)
  (smart-print-buf "*an-who*"
		   (an-request
		    "GET"
		    "user?current")
		   'emacs-lisp-mode))

(defun an-confluence-doc ()
  "Browse confluence 3.5 docs for symbol at point."
  (interactive)
  (let ((browse-url-generic-program "open"))
  (browse-url-generic
   (concat "https://confluence.atlassian.com/dosearchsite.action?"
	   "&searchQuery.spaceKey=CONF35"
	   "&searchQuery.queryString=ancestorIds%3A252347565+AND+"
	   (symbol-name (symbol-at-point))))))

(defun an-api-doc ()
  "search appnexus api docs for symbol at point"
  (interactive)
  (let ((browse-url-generic-program "open"))
    (browse-url-generic
     (concat "https://wiki.appnexus.com/dosearchsite.action?"
	     "searchQuery.spaceKey=api"
	     "&searchQuery.queryString=ancestorIds%3A27984339+AND+"
	     (symbol-name (symbol-at-point))))))

(defun an-auth-credentials (username)
  (interactive "susername: ")
  (setq an-username username)
  (setq an-password (read-passwd "password: ")))

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
