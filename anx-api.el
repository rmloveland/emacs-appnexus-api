;;; anx-api.el --- Interact with the AppNexus API from Emacs.

;; Copyright (C) 2013 Rich Loveland

;; Author: Rich Loveland
;; Version: 0.1
;; Keywords: Convenience, JSON, REST, API, AppNexus

;; This file is NOT part of GNU Emacs.

;; This code is written by Richard M. Loveland and placed in the
;; Public Domain. All warranties are disclaimed.

;;; Commentary:

;; Provides a number of functions for interacting with the AppNexus
;; API from Emacs.  These functions are bound to key chords for
;; convenience and speed of access.

;; To get started, call `M-x anx-get-user-authentication-credentials',
;; bound to `C-x C-a a'.  See the `Keybindings' section below for a
;; complete list of functions and their keys.

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)
(require 'url)

;; Group and customization information

(defgroup anx nil
  "Convenient interaction with AppNexus APIs from Emacs."
  :group 'processes
  :prefix "anx-"
  :link '(url-link :tag "Appnexus Console API documentation."
		   "https://wiki.appnexus.com/display/api/home"))

(defcustom anx-username nil
  "Appnexus API username."
  :group 'anx
  :type '(string))

(defcustom anx-password nil
  "Appnexus API password."
  :group 'anx
  :type '(string))

;;------------------------------------------------------------------
;; FIXME: Incorporate this into the code:
(defvar *anx-buffer-save-dir* nil)

(setq *anx-buffer-save-dir* "/Users/rloveland/work/code/api/elisp/")

(defun anx-save-buffer-contents ()
  (interactive)
  (write-file
   (expand-file-name
    (concat *anx-buffer-save-dir*
	    (replace-regexp-in-string "/" "_"
				      (buffer-name))
	    "_"
	    (replace-regexp-in-string " " "_"
				      (current-time-string))))))

(global-set-key (kbd "C-x C-a s") 'anx-save-buffer-contents)
;;------------------------------------------------------------------

;; Variables

(defvar *anx-authentication-credentials*
  `(:auth
    (:username ,anx-username
	       :password ,anx-password))
  "The username and password used by `anx-authenticate'.
Set your credentials using the `anx-get-user-authentication-credentials' command.")

(defvar *anx-production-url* "http://api.appnexus.com"
  "Production Console API entry point.")

(defvar *anx-sandbox-url* "http://sand.api.appnexus.com"
  "Sandbox Console API entry point.")

(defvar *anx-current-url* *anx-sandbox-url*
  "This variable holds the value of the current API entry point.
Toggle the value of this variable with the `anx-toggle-current-api-url' command.")

;; The variable `url-cookie-trusted-urls' is from the built-in `url'
;; package.  We don't want to clobber the global value, so we set a
;; value for this buffer only.
(set (make-local-variable 'url-cookie-trusted-urls)
     '(".*adnxs\.net" ".*adnxs\.com" ".*appnexus\.com"))

;; Functions

(defun anx--parse-response (buffer)
  "Given a BUFFER with an HTTP response, extracts the JSON payload.
Converts it to Lisp and returns it."
  (unwind-protect
      (with-current-buffer buffer
	(save-excursion
	  (if (boundp 'url-http-end-of-headers)
	      (progn
		(goto-char url-http-end-of-headers)
		(let ((json-key-type 'hash-table)
		      (response (json-read)))
		  response)))))))

(defun anx--send-request (verb path &optional payload)
  "HTTP VERB the service at PATH with an optional PAYLOAD.
If PAYLOAD is , it will be a Lisp data structure that is converted into
JSON before attaching it to the request."
  (let ((url-request-method verb)
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (if payload
	     (json-encode payload)
	   "")))
      (anx--parse-response
       (url-retrieve-synchronously
	(concat *anx-current-url* "/" path)))))

(defun anx--pop-up-buffer (bufname stuff mode)
  "Create a BUFNAME with STUFF in it, using your preferred MODE."
  (let ((buf (generate-new-buffer bufname))
	(other-frame t))
    ;; setting mode is done before showing the new frame
    ;; because otherwise, we get a nasty animation effect
    (set-buffer buf)
    (funcall mode)
    (if other-frame
	(switch-to-buffer-other-window buf))
    (setq buffer-offer-save t)
    (put 'buffer-offer-save 'permanent-local t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (print stuff buf)))

(defun anx-authenticate ()
  "Authenticate with the current API endpoint."
  (interactive)
  (anx--pop-up-buffer
   (concat *anx-current-url* "/auth")
   (anx--send-request "POST"
		      "auth"
		      `(:auth (:username
			       ,anx-username
			       :password
			       ,anx-password)))
   'emacs-lisp-mode))

(defun anx-lisp-to-json ()
  "Convert the current buffer from Lisp to a string containing escaped JSON.

This escaped string is preferred by the 'json' package.  The results open in
a new buffer."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat (buffer-name) ".json"))
	(mode 'js-mode))
    (anx--pop-up-buffer bufname (json-encode it) mode)))

(defun anx-escape-json ()
  "Escape the contents of the current buffer for use by the Lisp reader.

This is currently an intermediate step in the conversion from
JSON to Lisp, which involves invoking the interactive commands
`anx-escape-json' and `buf2lsp' in sequence.  This opens new
buffers and performs other unnecessary stateful operations, and
should be rewritten."
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

(defun anx-unescape-json ()
  "Unescape the contents of the buffer by removing quotes and backslashes.

This is currently an intermediate step in the conversion from
Lisp to JSON, which involves invoking the commands
`anx-lisp-to-json' and `anx-unescape-json' in sequence.  This
opens new buffers and performs other unnecessary stateful
operations, and should be rewritten."
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

(defun anx-json-to-lisp ()
  "Convert the current buffer's contents from an escaped JSON string into Lisp.

This is currently an intermediate step in the conversion from
JSON to Lisp, which involves invoking the commands
`anx-escape-json' and `anx-json-to-lisp' in sequence.  This opens
new buffers and performs other unnecessary stateful operations,
and should be rewritten."
  (interactive)
  (let ((it (read (buffer-string)))
	(bufname (concat (buffer-name) ".el"))
	(mode 'emacs-lisp-mode))
    (anx--pop-up-buffer bufname (json-read-from-string it) mode)
    (switch-to-buffer bufname)))

(defun anx-send-buffer (verb service-and-params)
  "Send the current buffer's contents using VERB to SERVICE-AND-PARAMS.

VERB is an HTTP command, and should be either 'PUT' or 'POST'; to
make 'GET' calls use `anx-get'.

Prompts for SERVICE-AND-PARAMS in the minibuffer.  When the call
returns, the JSON is converted to Lisp and displayed in a new
buffer.

Note that you should be in a buffer containing Lisp as understood
by the `json' package when you invoke this command.  The easiest
way to get to this state is using the `anx-json-to-lisp' command
in a sequence like the following:

1. From a JSON buffer, invoke the command `anx-json-to-lisp',
which will open a new buffer containing the Lisp equivalent.
Note that this step is optional, since you may write your JSON in
Lisp directly and skip to step 2.

2. Invoke the `anx-send-buffer' command inside the Lisp buffer.
The Lisp in this buffer will be sent to the API entry point as
JSON; you will receive a JSON response that is converted to Lisp
and opened in (yet another) new buffer.

This workflow is likely to be redesigned in the future, as there
are many inefficiencies that can be removed; it was never
actually designed in the first place, but grown."
  (interactive "sverb: \nsservice+params: ")
  (let ((payload (read (buffer-string))))
    (anx--pop-up-buffer
     (concat *anx-current-url* "/" service-and-params "[" verb "]")
     (anx--send-request verb
		 service-and-params
		 payload)
     'emacs-lisp-mode)))

(defun anx-get (service-and-params)
  "Send a 'GET' request to SERVICE-AND-PARAMS.

Prompts for SERVICE-AND-PARAMS in the minibuffer and opens the
response in a new Lisp buffer."
  (interactive "sservice+params: ")
  (anx--pop-up-buffer (concat *anx-current-url* "/" service-and-params)
	     (anx--send-request "GET"
			 service-and-params)
	     'emacs-lisp-mode))

;; Generic GET request -- this should probably live somewhere else
;; eventually, when this code grows up and gets refactored properly...

(defun rml/get (url)
  "Send a 'GET' request to URL.

Prompts for URL in the minibuffer and opens the
response in a new buffer."
  (interactive "sURL: ")
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(response-buf (url-retrieve-synchronously url)))
    (anx--pop-up-buffer url
			(unwind-protect
			    (with-current-buffer response-buf
			      (save-excursion
				(let ((response (buffer-string)))
				  response))))
			'fundamental-mode)))

(defun anx-delete (service-and-params)
  "Send a 'DELETE' request to SERVICE-AND-PARAMS.

Prompts for SERVICE-AND-PARAMS in the minibuffer and opens the
response in a new Lisp buffer."
  (interactive "sservice+params: ")
  (anx--pop-up-buffer (concat *anx-current-url* "/" service-and-params)
	     (anx--send-request "DELETE"
			 service-and-params)
	     'emacs-lisp-mode))

(defun anx-switch-users (user-id)
  "Switch to the user denoted by USER-ID.

Opens the response in a new buffer."
  (interactive "suser-id: ")
  (anx--pop-up-buffer "*anx-switch-users*"
	     (anx--send-request "POST"
			 "auth"
			 `(:auth (:switch_to_user ,user-id)))
	     'emacs-lisp-mode))

(defun anx-who-am-i ()
  "Ask the API which user you're operating as.

Opens the response in a new buffer."
  (interactive)
  (anx--pop-up-buffer "*anx-who-am-i*"
		   (anx--send-request
		    "GET"
		    "user?current")
		   'emacs-lisp-mode))

(defun anx-browse-api-docs ()
  "Search the AppNexus Console API documentation for the symbol at point.
Opens the results in whatever web browser is preferred by `browse-url'."
  (interactive)
  (browse-url
   (concat "https://wiki.appnexus.com/dosearchsite.action?"
	   "searchQuery.spaceKey=api"
	   "&searchQuery.queryString=ancestorIds%3A27984339+AND+"
	   (symbol-name (symbol-at-point)))))

(defun anx-get-user-authentication-credentials (username)
  "Prompt for an API USERNAME (and password).

You can also set your login credentials using
`customize-group'.  The group name is 'anx'."
  (interactive "susername: ")
  (setq anx-username username)
  (setq anx-password (read-passwd "password: ")))

(defun anx-display-current-api-url ()
  "Prints the current API URL in the minibuffer."
  (interactive)
  (message "current api url is %s" *anx-current-url*))

(defun anx-toggle-current-api-url ()
  "Switch between 'sand' and 'prod' Console APIs."
  (interactive)
  (if (string-equal *anx-current-url* *anx-sandbox-url*)
      (setq *anx-current-url* *anx-production-url*)
    (setq *anx-current-url* *anx-sandbox-url*)))

;; Keybindings

(global-set-key (kbd "C-x C-a A") 'anx-authenticate)
(global-set-key (kbd "C-x C-a a") 'anx-get-user-authentication-credentials)
(global-set-key (kbd "C-x C-a S") 'anx-switch-users)

(global-set-key (kbd "C-x C-a W") 'anx-who-am-i)
(global-set-key (kbd "C-x C-a w") 'anx-display-current-api-url)
(global-set-key (kbd "C-x C-a T") 'anx-toggle-current-api-url)

(global-set-key (kbd "C-x C-a J") 'anx-lisp-to-json)
(global-set-key (kbd "C-x C-a L") 'anx-json-to-lisp)

(global-set-key (kbd "C-x C-a P") 'anx-send-buffer)
(global-set-key (kbd "C-x C-a G") 'anx-get)
(global-set-key (kbd "C-x C-a g") 'rml/get)
(global-set-key (kbd "C-x C-a D") 'anx-delete)

(global-set-key (kbd "C-x C-a U") 'anx-unescape-json)
(global-set-key (kbd "C-x C-a E") 'anx-escape-json)

(global-set-key (kbd "C-x C-a d") 'anx-browse-api-docs)

(provide 'anx-api)

;;; anx-api.el ends here
