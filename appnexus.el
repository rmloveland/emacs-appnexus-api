(eval-when-compile (require 'cl))
(require 'json)
(require 'url)

;; Group and customization information.

(defgroup appnexus nil
  "Functions that allow for easy interaction with AppNexus APIs."
  :group 'processes
  :prefix "appnexus-"
  :link '(url-link :tag "Appnexus Console API entry point."
		   "http://api.appnexus.com")
  :link '(url-link :tag "Appnexus sandbox Console API entry point."
		   "http://sand.api.appnexus.com")
  :link '(url-link :tag "Appnexus Console API documentation."
		   "https://wiki.appnexus.com/display/api/home"))

(defcustom an-username nil
  "Appnexus API username."
  :group 'appnexus
  :type '(string))

(defcustom an-password nil
  "Appnexus API password."
  :group 'appnexus
  :type '(string))

;; Variables.

(defvar *an-auth*
  `(:auth
    (:username ,an-username
	       :password ,an-password))
  "The username and password stored here are used by `an-auth' when you log
in. Set your credentials using the `an-auth-credentials' command.")

(defvar *an-production-url* "http://api.appnexus.com"
  "Production Console API entry point.")

(defvar *an-sandbox-url* "http://sand.api.appnexus.com"
  "Sandbox Console API entry point.")

(defvar *an-current-url* *an-sandbox-url*
  "This variable holds the value of the current API entry point. Toggle
the value of this variable with the `an-toggle-sand-or-prod-url' command.")

;; The variable `url-cookie-trusted-urls' is from the built-in `url'
;; package.  We don't want to clobber the global value, so we set a
;; value for this buffer only.

(set (make-local-variable 'url-cookie-trusted-urls)
     '(".*adnxs\.net" ".*adnxs\.com" ".*appnexus\.com"))

;; Functions.

(defun an-response (buffer)
  "Converts the JSON response left in BUFFER by `an-request' into a
Lisp hash table."
  (unwind-protect
      (with-current-buffer buffer
	(save-excursion
	  (goto-char url-http-end-of-headers)
	  (let ((json-key-type 'hash-table)
		(response (json-read-from-string
			   (buffer-substring (point) (point-max)))))
	    response)))))

(defun an-request (verb path &optional payload)
  "Sends an HTTP VERB to the API service at PATH with an optional PAYLOAD.
If PAYLOAD exists, it will be a Lisp data structure that is converted into
JSON before attaching it to the request."
  (if payload
      (let ((url-request-method verb)
	    (url-request-extra-headers
	     '(("Content-Type" . "application/x-www-form-urlencoded")))
	    (url-request-data
	     (json-encode payload)))
	(an-response
	 (url-retrieve-synchronously
	  (concat *an-current-url* "/" path))))
    (let ((url-request-method verb)
	  (url-request-extra-headers
	   '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (an-response
       (url-retrieve-synchronously
	(concat *an-current-url* "/" path))))))

(defun an-extract-meta-fields ()
  "Given the Lisp response from an API service's `meta' call, create a
new buffer with just the `fields' list."
  (interactive)
  (let* ((it (read (buffer-string)))
	 (response (let ((json-object-type 'alist))
		     (assoc 'response it)))
	 (fields (cdr (assoc 'fields response)))
	 (bufname (concat "*meta-" (number-to-string (random 1000)) "*"))
	(mode 'emacs-lisp-mode))
    (smart-print-buf bufname fields mode)))

(defun an-auth (&optional payload)
  "Authenticates with the API entry point currently in use and opens the
response in a new Lisp buffer. Takes an optional Lisp PAYLOAD defining
your authentication credentials."
  (interactive)
  (smart-print-buf
   "*an-auth*"
   (an-request "POST"
	       "auth"
	       (or payload
		   `(:auth (:username ,an-username :password ,an-password))))
   'emacs-lisp-mode))

(defun smart-print-buf (bufname stuff mode)
  "Opens a new buffer BUFNAME and prints STUFF into it, using the Emacs
MODE of your choice."
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
  (setq buffer-offer-save t)
  (put 'buffer-offer-save 'permanent-local t)
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (print stuff buf)))

(defun buf2json ()
  "Converts the current buffer from Lisp to a string containing escaped JSON.
This escaped string is preferred by the `json' package. Opens the results in
a new buffer."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat "*json-" (number-to-string (random 1000)) "*"))
	(mode 'js-mode))
    (smart-print-buf bufname (json-encode it) mode)))

(defun dirty-json ()
  "Converts the current buffer from raw JSON to a string containing
escaped JSON, as preferred by the `json' package. Opens the results
in a new buffer.

This is currently an intermediate step in the conversion from JSON to
Lisp, which involves invoking the interactive commands `dirty-json'
and `buf2lsp' in sequence. This opens new buffers and performs other
unnecessary stateful operations, and should be rewritten."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "\"")
    (goto-char (point-max))
    (insert "\"")
    (goto-char (+ 1 (point-min)))
    ;; Replace all instances of backslash quote with double backslash
    ;; quote.
    (while (re-search-forward "\\\\\"" (- (point-max) 1) t)
      (replace-match "\\\\\"" nil t))
    (goto-char (+ 1 (point-min)))
    ;; Replace all instances of quote with backslash quote.
    (while (re-search-forward "\"" (- (point-max) 1) t)
      (replace-match "\\\"" nil t))
    (goto-char (+ 1 (point-min)))
    ;; Finally, remove all newlines and space characters, since our
    ;; the `json' package's parser can't understand them and throws an
    ;; error.
    (while (re-search-forward "\n +" (- (point-max) 1) t)
      (replace-match "" nil t))))

(defun clean-json ()
  "Converts the current buffer from a string containing escaped JSON into
raw JSON. Opens the results in a new buffer.

This is currently an intermediate step in the conversion from Lisp to JSON,
which involves invoking the commands `buf2json' and `clean-json' in sequence.
This opens new buffers and performs other unnecessary stateful operations,
and should be rewritten."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\"" nil t)
    (replace-match "" nil t)
    (goto-char (point-max))
    (re-search-backward "\"" nil t)
    (replace-match "" nil t)
    (goto-char (point-min))
    ;; First, replace all instances of the single backslashed quote
    ;; with quote.
    (while (re-search-forward "\\\\\"" nil t)
      (replace-match "\"" nil t))
    ;; Make a second pass, replacing all instances of double
    ;; backslashes with a single backslash.
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\\\" nil t)
      (replace-match "\\" nil t))))

(defun buf2lsp ()
  "Converts the current buffer from a string containing escaped JSON into
Lisp. Opens the results in a new buffer.

This is currently an intermediate step in the conversion from JSON to Lisp,
which involves invoking the commands `dirty-json' and `buf2lsp' in sequence.
This opens new buffers and performs other unnecessary stateful operations,
and should be rewritten."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat "*lsp-" (number-to-string (random 1000)) "*"))
	(mode 'emacs-lisp-mode))
    (smart-print-buf bufname (json-read-from-string it) mode)
    (switch-to-buffer bufname)))

(defun buf-do (VERB SERVICE+PARAMS)
  "Sends an HTTP VERB to SERVICE+PARAMS, with the current buffer's
contents in the header. VERB should be either PUT or POST; for GET
calls, use `an-get'.

Prompts for SERVICE+PARAMS in the minibuffer. When the call to
SERVICE+PARAMS returns, the JSON is converted to Lisp and displayed in
a new buffer.

Note that you should be in a buffer containing Lisp as understood by
the `json' package when you invoke this command. The easiest way to
get to this state is using the `buf2lsp' command in a sequence like
the following:

1. From a JSON buffer, invoke the command `buf2lsp', which will open a
new buffer containing the Lisp equivalent. Note that this step is
optional, since you may write your JSON in Lisp directly and skip to
step 2.

2. Invoke the `buf-do' command inside the Lisp buffer. The Lisp in
this buffer will be sent to the API entry point as JSON; you will
receive a JSON response that is converted to Lisp and opened in (yet
another) new buffer.

This workflow is likely to be redesigned in the future, as there are
many inefficiencies that can be removed; it was never actually
designed in the first place, but grown."
  (interactive "sverb: \nsservice+params: ")
  (let ((payload (read (buffer-string))))
    (smart-print-buf
     (concat "*" service+params "*")
     (an-request verb
		 service+params
		 payload)
     'emacs-lisp-mode)))

(defun an-get (service+params)
  "Sends a GET request to SERVICE+PARAMS. Prompts for SERVICE+PARAMS
in the minibuffer. Opens the response in a new Lisp buffer."
  (interactive "sservice+params: ")
  (smart-print-buf (concat "*" service+params "*")
	     (an-request "GET"
			 service+params)
	     'emacs-lisp-mode))

(defun an-switchto (user-id)
  "Switches to the API user denoted by USER-ID. Opens the response in
a new Lisp buffer."
  (interactive "suser-id: ")
  (smart-print-buf "*an-switchto*"
	     (an-request "POST"
			 "auth"
			 `(:auth (:switch_to_user ,user-id)))
	     'emacs-lisp-mode))

(defun an-who ()
  "Displays what user you are currently operating as. Opens the
response in a new Lisp buffer."
  (interactive)
  (smart-print-buf "*an-who*"
		   (an-request
		    "GET"
		    "user?current")
		   'emacs-lisp-mode))

(defun an-api-doc ()
  "Searches the API documentation for the symbol at point. Opens the
results in a web browser.

Note that this function is currently only working on Mac OS X, and
should be rewritten ASAP. Please contact rloveland@appnexus.com and
nag him about it."
  (interactive)
  (let ((browse-url-generic-program "open"))
    (browse-url-generic
     (concat "https://wiki.appnexus.com/dosearchsite.action?"
	     "searchQuery.spaceKey=api"
	     "&searchQuery.queryString=ancestorIds%3A27984339+AND+"
	     (symbol-name (symbol-at-point))))))

(defun an-auth-credentials (username)
  "Prompts for your API username and password. You can also set them
by typing `M-x customize-group RET appnexus'."
  (interactive "susername: ")
  (setq an-username username)
  (setq an-password (read-passwd "password: ")))

(defun an-print-current-url ()
  "Prints the current API URL in the minibuffer."
  (interactive)
  (message "current api url is %s" *an-current-url*))

;; FIXME: Rewrite this to use a list.

(defun an-toggle-sand-or-prod-url ()
  "Switches between ``sand'' and ``prod'' Console APIs."
  (interactive)
  (if (string-equal *an-current-url* *an-sandbox-url*)
      (setq *an-current-url* *an-production-url*)
    (setq *an-current-url* *an-sandbox-url*)))

;; FIXME: move these functions to `.emacs'

(defun an-confluence-doc ()
  "Search Confluence 3.5 documentation for the symbol at point."
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

;; keybindings
;; FIXME: move these into `.emacs' and document them

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

;; appnexus.el ends here.
