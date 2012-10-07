(eval-when-compile (require 'cl))
(require 'json)
(require 'url)

;; Group and customization information.

(defgroup appnexus nil
  "functions that allow for easy interaction with appnexus apis."
  :group 'processes
  :prefix "appnexus-"
  :link '(url-link :tag "Appnexus Console API entry point."
		   "http://api.appnexus.com")
  :link '(url-link :tag "Appnexus sandbox API entry point."
		   "http://sand.api.appnexus.com")
  :link '(url-link :tag "Appnexus API documentation."
		   "https://wiki.appnexus.com/display/api/home"))

(defcustom an-username nil
  "Your Appnexus API username."
  :group 'appnexus
  :type '(string))

(defcustom an-password nil
  "Your Appnexus API password."
  :group 'appnexus
  :type '(string))

;; Variables.

(defvar *an-auth*
  `(:auth
    (:username ,an-username
	       :password ,an-password))
  "The username and password stored here are used by `an-auth' when you log in. Set your credentials using the `an-auth-credentials' command.")

(defvar *an-production-url* "http://api.appnexus.com"
  "Production Console API entry point.")

(defvar *an-sandbox-url* "http://sand.api.appnexus.com"
  "Sandbox Console API entry point.")

(defvar *an-current-url* *an-sandbox-url*
  "This variable holds the value of the current API entry point. Toggle the value of this variable with the `an-toggle-sand-or-prod-url' command.")

;; The variable `url-cookie-trusted-urls' is from the built-in `url'
;; package.  We don't want to clobber the global value, so we set a
;; value for this buffer only.

(set (make-local-variable 'url-cookie-trusted-urls)
     '(".*adnxs\.net" ".*adnxs\.com" ".*appnexus\.com"))

;; Functions.

(defun an-response (buffer)
  "Converts the JSON response left in BUFFER by `an-request' into a Lisp hash table."
  (unwind-protect
      (with-current-buffer buffer
	(save-excursion
	  (goto-char url-http-end-of-headers)
	  (let ((json-key-type 'hash-table)
		(response (json-read-from-string
			   (buffer-substring (point) (point-max)))))
	    response)))))

(defun an-request (verb path &optional payload)
  "Sends VERB (an HTTP request) to the API service at PATH with an optional PAYLOAD attached.
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

;; FIXME: Finish writing the below function. Make it generic enough to handle all
;; the weird non-standard `meta' responses. Maybe use a translation table?

(defun an-json-fields (filename)
  "Given a JSON response from an API service's `meta' command, return the fields as a Lisp structure.
Note: This function is currently unfinished."
  (let* ((possible-values '(meta fields))
	(response (let ((json-object-type 'alist))
		    (assoc 'response (json-read-file filename))))
	(fields (car (remove-if #'null
				(mapcar (lambda (val)
					  (assoc val response))
					possible-values)))))
    fields))

(defun an-auth (&optional payload)
  "Authenticates with the API entry point currently in use and opens the response in a new Lisp buffer. Takes an optional Lisp PAYLOAD defining your authentication credentials."
  (interactive)
  (smart-print-buf
   "*an-auth*"
   (an-request "POST"
	       "auth"
	       (or payload
		   `(:auth (:username ,an-username :password ,an-password))))
   'emacs-lisp-mode))

(defun smart-print-buf (bufname stuff mode)
  "Opens a new buffer BUFNAME and prints STUFF into it, using the Emacs MODE of your choice."
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
  "Converts the current buffer from regular Lisp structures (lists, &c.) to a Lisp string containing escaped JSON. This escaped string is preferred by the `json' package. After the conversion, opens the resulting escaped string in a new buffer using `smart-print-buf'."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat "*json-" (number-to-string (random 1000)) "*"))
	(mode 'js-mode))
    (smart-print-buf bufname (json-encode it) mode)))

(defun dirty-json ()
  "Converts the current buffer from JSON to an Lisp string containing
escaped JSON, as preferred by the `json' package. After the
conversion, opens the resulting escaped string in a new buffer.

This is currently an intermediate step in the conversion from JSON to Lisp, which involves invoking the commands `dirty-json' and `buf2lsp' in sequence. This opens new buffers and performs other unnecessary stateful operations, and should be rewritten."
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
  "Converts the current buffer from a Lisp string of escaped JSON as preferred by the `json' package into JSON. After the conversion, opens the resulting JSON in a new buffer.

This is currently an intermediate step in the conversion from Lisp to JSON, which involves invoking the commands `buf2json' and `clean-json' in sequence. This opens new buffers and performs other unnecessary stateful operations, and should be rewritten."
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
  "Converts the current buffer from a Lisp string containing escaped JSON into Lisp. After the conversion, opens the resulting Lisp in a new buffer.

This is currently an intermediate step in the conversion from JSON to Lisp, which involves invoking the commands `dirty-json' and `buf2lsp' in sequence. This opens new buffers and performs other unnecessary stateful operations, and should be rewritten."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat "*lsp-" (number-to-string (random 1000)) "*"))
	(mode 'emacs-lisp-mode))
    (smart-print-buf bufname (json-read-from-string it) mode)
    (switch-to-buffer bufname)))

(defun buf-do (VERB SERVICE+PARAMS)
  "Sends an HTTP request, VERB, to SERVICE+PARAMS, with the current buffer's contents attached as a payload. Since we send a payload to the API entry point at SERVICE+PARAMS, VERB should be either PUT or POST; otherwise, use `an-get'.

Prompts for SERVICE+PARAMS interactively in the minibuffer. When the call to SERVICE+PARAMS returns, the JSON response is converted to Lisp and opened in a new buffer.

Note that you should be in a buffer containing Lisp data understood by the `json' package when you invoke this command.

The easiest way to get to this state is using the `buf2lsp' command in a sequence like the following:
1. Navigate to a buffer containing valid JSON, as determined by an AppNexus API service. Note that this step is optional, since you may write your JSON in Lisp first and then convert it to JSON.
2. Invoke the command `buf2lsp' from a JSON buffer, which will open a new buffer containing the JSON buffer's data formatted in Lisp. Again, this is not necessary if you prefer to compose your JSON in Lisp.
3. Invoke this command, `buf-do', inside a Lisp buffer formatted as understood by the `json' package. The Lisp data in this buffer will be sent to the API entry point as JSON, and you will receive a JSON response that is converted to Lisp and opened in (yet another) new buffer.

This workflow is likely to be redesigned in the future, as there are many inefficiencies that can be removed; this is because it was never designed in the first place, but grown."
  (interactive "sverb: \nsservice+params: ")
  (let ((payload (read (buffer-string))))
    (smart-print-buf
     (concat "*" service+params "*")
     (an-request verb
		 service+params
		 payload)
     'emacs-lisp-mode)))

(defun an-get (service+params)
  "Send a standard HTTP GET request to SERVICE+PARAMS. Prompts for SERVICE+PARAMS in the minibuffer. When the call to SERVICE+PARAMS returns, the JSON response is converted to Lisp and opened in a new buffer."
  (interactive "sservice+params: ")
  (smart-print-buf (concat "*" service+params "*")
	     (an-request "GET"
			 service+params)
	     'emacs-lisp-mode))

(defun an-switchto (user-id)
  "Switches your current user to the API user denoted by USER-ID. Prompts for USER-ID in the minibuffer. When the call to the API returns, the JSON response is converted to Lisp and opened in a new buffer."
  (interactive "suser-id: ")
  (smart-print-buf "*an-switchto*"
	     (an-request "POST"
			 "auth"
			 `(:auth (:switch_to_user ,user-id)))
	     'emacs-lisp-mode))

(defun an-who ()
  "Finds out from the API what user you are currently operating as. When the call to the API returns, the JSON response is converted to Lisp and opened in new buffer."
  (interactive)
  (smart-print-buf "*an-who*"
		   (an-request
		    "GET"
		    "user?current")
		   'emacs-lisp-mode))

(defun an-api-doc ()
  "Searches the Appnexus API documentation for the symbol at point. Opens the results in a web browser.

Note that this function is currently only working on Mac OS X, and should be rewritten ASAP. Please contact rloveland@appnexus.com and nag him about it."
  (interactive)
  (let ((browse-url-generic-program "open"))
    (browse-url-generic
     (concat "https://wiki.appnexus.com/dosearchsite.action?"
	     "searchQuery.spaceKey=api"
	     "&searchQuery.queryString=ancestorIds%3A27984339+AND+"
	     (symbol-name (symbol-at-point))))))

(defun an-auth-credentials (username)
  "Interactively prompts for the values of your API username and password. If you prefer to set your username and password using Emacs' `customize' command, type `M-x customize-group RET appnexus'."
  (interactive "susername: ")
  (setq an-username username)
  (setq an-password (read-passwd "password: ")))

(defun an-print-current-url ()
  "Prints the value of the current API entry point to the minibuffer."
  (interactive)
  (message "current api url is %s" *an-current-url*))

(defun an-toggle-sand-or-prod-url ()
  "Switches between the sandbox and production Console API entry points."
  (interactive)
  (if (string-equal *an-current-url* *an-sandbox-url*)
      (setq *an-current-url* *an-production-url*)
    (setq *an-current-url* *an-sandbox-url*)))

;; FIXME: move these functions to `.emacs'

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
