(eval-when-compile (require 'cl))
(require 'json)
(require 'url)


;;; group and customization information

(defgroup appnexus nil
  "functions that allow for easy interaction with appnexus apis."
  :group 'processes
  :prefix "an-"
  :link '(url-link :tag "appnexus api entry point."
		   "http://api.appnexus.com")
  :link '(url-link :tag "appnexus sandbox api entry point."
		   "http://api.sand-08.adnxs.net")
  :link '(url-link :tag "appnexus api documentation."
		   "https://wiki.appnexus.com/display/api/home"))


(defcustom an-username nil
  "appnexus api username."
  :group 'appnexus
  :type '(string))


(defcustom an-password nil
  "appnexus api password."
  :group 'appnexus
  :type '(string))


;;; variables

;; this is used by `an-auth' when you log in; you can set it using 
;; `an-auth-credentials'

(defvar *an-auth*
  `(:auth
    (:username ,an-username
	       :password ,an-password)))


;; console api urls

(defvar *an-production-url* "http://api.appnexus.com")
(defvar *an-sandbox-url* "http://hb.sand-08.adnxs.net")


;; impbus api urls

(defvar *an-ib-production-url* "http://api.adnxs.com")
(defvar *an-ib-sandbox-url* "http://api.sand-08.adnxs.net")


;; this is used by `an-print-current-url'

(defvar *an-current-url* *an-sandbox-url*)


(setq url-cookie-trusted-urls '(".*adnxs\.net"
				".*adnxs\.com"
				".*appnexus\.com"))


;;; functions

(defun an-response (buffer)
  "convert the json response left by `an-request' in buffer into a hash table.
then, return that hash table."
  (unwind-protect
      (with-current-buffer buffer
	(save-excursion
	  (goto-char url-http-end-of-headers)
	  (let ((json-key-type 'hash-table)
		(response (json-read-from-string (buffer-substring (point) (point-max)))))
	    response)))))


(defun an-request (verb path &optional payload)
  "send an http request, verb, to the service at path with payload in tow.
if it exists, payload will be a lisp data structure that we convert into
json on the fly before making the request."
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


;; fixme: finish writing the below function; make it generic enough to handle all
;; the weird cases. maybe use a translation table?

(defun an-json-fields (filename)
  "gather a list of json fields available through the api `meta' service.
this function is unfinished. currently takes a filename argument, but should
accept a url in future."
  (let* ((possible-values '(meta fields))
	(response (let ((json-object-type 'alist))
		    (assoc 'response (json-read-file filename))))
	(fields (car (remove-if #'null
				(mapcar (lambda (val)
					  (assoc val response))
					possible-values)))))
    fields))


(defun an-auth (&optional payload)
  "authenticate with the api and open the response in a temporary buffer."
  (interactive)
  (smart-print-buf "*an-auth*"
	     (an-request "POST"
			 "auth"
			 (or payload
			     `(:auth (:username ,an-username :password ,an-password))))
	     'emacs-lisp-mode))


(defun smart-print-buf (bufname thing mode)
  "pop open a buffer bufname containing thing in your emacs mode of choice."
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
  "convert the current buffer from elisp to the heavily escaped json string
format preferred by `json.el'. open the results in a new buffer."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat "*json-" (number-to-string (random 1000)) "*"))
	(mode 'js-mode))
    (smart-print-buf bufname (json-encode it) mode)))


(defun dirty-json ()
  "convert a buffer of standard json to the escaped json string format
preferred by `json.el', and pop open a new buffer with the contents. this
is an intermediate step for re-conversion to elisp."
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
  "convert a buffer of the escaped json strings preferred by `json.el' into
standard  json, and pop open a new buffer with the contents."
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
  "convert the `json.el' escaped json string in the current buffer to elisp,
and open the results in a new temp buffer."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat "*jlsp-" (number-to-string (random 1000)) "*"))
	(mode 'emacs-lisp-mode))
    (smart-print-buf bufname (json-read-from-string it) mode)
    ;; think about using `elisp-format-buffer'
    (switch-to-buffer bufname)))


(defun buf-do (verb service+params)
  "send a request, verb, to service+params."
  (interactive "sverb: \nsservice+params: ")
  (let ((payload (read (buffer-string))))
    (smart-print-buf (concat "*" service+params "*")
	       (an-request verb
			   service+params
			   	   payload)
	       'emacs-lisp-mode)))


(defun an-get (service+params)
  "send a standard get request to service+params."
  (interactive "sservice+params: ")
  (smart-print-buf (concat "*" service+params "*")
	     (an-request "GET"
			 service+params)
	     'emacs-lisp-mode))


(defun an-switchto (user-id)
  "switch to another api user. this function will only work if you're an
admin user."
  (interactive "suser-id: ")
  (smart-print-buf "*an-switchto*"
	     (an-request "POST"
			 "auth"
			 `(:auth (:switch_to_user ,user-id)))
	     'emacs-lisp-mode))


(defun an-who ()
  "find out what user you are; open in new buffer."
  (interactive)
  (smart-print-buf "*an-who*"
		   (an-request
		    "GET"
		    "user?current")
		   'emacs-lisp-mode))


;; FIXME: move this code to .emacs and add a hook to confluence-mode

(defun an-confluence-doc ()
  "search confluence 3.5 docs for symbol at point"
  (interactive)
  (let ((browse-url-generic-program "open"))
  (browse-url-generic
   (concat "https://confluence.atlassian.com/dosearchsite.action?"
	   "&searchQuery.spaceKey=CONF35"
	   "&searchQuery.queryString=ancestorIds%3A252347565+AND+"
	   (symbol-name (symbol-at-point))))))


(defun js-search-documentation ()
  "search mozilla developer network documentation for the symbol at point"
  (interactive)
  (let ((browse-url-generic-program "open"))
    (browse-url-generic
     (concat "https://developer.mozilla.org/en-US/search?q="
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
  "toggle sand or prod url"
  (interactive)
  (if (string-equal *an-current-url* *an-sandbox-url*)
      (setq *an-current-url* *an-production-url*)
    (setq *an-current-url* *an-sandbox-url*)))


;;; keybindings
;; move these into `.emacs' and document them

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

;; bye
