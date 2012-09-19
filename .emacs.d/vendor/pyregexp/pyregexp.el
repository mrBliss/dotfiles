;;; pyregexp.el --- A Python regexp/replace command for Emacs with interactive visual feedback

;; Copyright (C) 2012 Marko Bencun

;; Author : Marko Bencun <mbencun@gmail.com>
;; URL : https://github.com/benma/pyregexp/
;; Version : 0.2
;; Keywords : regexp, replace, python

;; This file is part of pyregexp.

;; pyregexp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; pyregexp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with pyregexp.  If not, see <http://www.gnu.org/licenses/>.

;;; WHAT'S NEW
;; 0.2 added pyregexp isearch and pyregexp-query-replace (experimental)
;; 0.1 initial release

;;; INTRODUCTION
;;
;; What's This?
;;
;; It is a command for emacs which enables you to use Python regular expressions and either a Python string or a Python expression for doing replacements.
;; While constructing the regexp in the minibuffer, you get live visual feedback for the matches, including group matches.
;; While constructing the replacement in the minibuffer, you get live visual feedback for the replacements.

;; Where does pyregexp come from?
;;
;; I was not happy with the way I used emacs' replace-regexp before. Constructing the regular expression is error prone and emacs' regular expressions are limited
;; (for example, no lookaheads, named groups, etc.).
;; Using re-builder to interactively build regular expressions was a step into the right direction, but manually copying over the regexp
;; to the minibuffer is cumbersome.
;; Using the idea of interactive of of re-builder, this package makes it possible to use just the minibuffer to construct (with live visual feedback) the regexp and replacement,
;; using Python's regular expressions and, optionally, Python expressions for the replacement.
;;
;; So a thanks to Detlev Zundel for his re-builder.

;;; Installation
;;
;; Put pyregexp.el and pyregexp.py into the same directory.
;; Add the following code to your init file. Of course you can select
;; your own key bindings.
;; ----------------------------------------------------------
;; (add-to-list 'load-path "folder-in-which-pyregexp-files-are-in/")
;; (require 'pyregexp)
;; (define-key global-map (kbd "C-c r") 'pyregexp-replace)
;; (define-key global-map (kbd "C-c q") 'pyregexp-query-replace)
;; ;; to use pyregexp isearch instead of the built-in regexp isearch, also include the following lines:
;; (define-key esc-map (kbd "C-r") 'pyregexp-isearch-backward)
;; (define-key esc-map (kbd "C-s") 'pyregexp-isearch-forward)
;; ----------------------------------------------------------
;; To customize, use: M-x customize-group [RET] pyregexp.
;; You can specify how to invoke the Python interpreter by modifying the pyregexp-command-prefix variable. The default is "python /path/to/pyregexp.py".
;;
;; Execute C-h f "pyregexp-replace" to read more and see examples.

;; Code goes here

(unless (fboundp 'make-overlay)
  (require 'overlay))

;; cl is used for (loop ...) macro
(require 'cl)

;;; faces

(defface pyregexp-match-0
  '((((class color) (background light))
     :background "lightblue")
    (((class color) (background dark))
     :background "steelblue4")
    (t
     :inverse-video t))
  "First face for displaying a whole match."
  :group 'pyregexp)

(defface pyregexp-match-1
  '((((class color) (background light))
     :background "cadetblue")
    (((class color) (background dark))
     :background "slateblue4")
    (t
     :inverse-video t))
  "Second face for displaying a whole match."
  :group 'pyregexp)

(defface pyregexp-group-0
  '((((class color) (background light))
     :background "aquamarine")
    (((class color) (background dark))
     :background "blue3")
    (t
     :inverse-video t))
  "First face for displaying a matching group."
  :group 'pyregexp)

(defface pyregexp-group-1
  '((((class color) (background light))
     :background "springgreen")
    (((class color) (background dark))
     :background "chartreuse4")
    (t
     :inverse-video t))
  "Second face for displaying a matching group."
  :group 'pyregexp)

(defface pyregexp-group-2
  '((((min-colors 88) (class color) (background light))
     :background "yellow1")
    (((class color) (background light))
     :background "yellow")
    (((class color) (background dark))
     :background "sienna4")
    (t
     :inverse-video t))
  "Third face for displaying a matching group."
  :group 'pyregexp)

;;; variables

(defconst pyregexp-filename (expand-file-name "pyregexp.py" (file-name-directory load-file-name))
  "Path to pyregexp.py")

(defcustom pyregexp-command-prefix (format "python %s" pyregexp-filename)
  "External script to compute the replacements."
  :type 'string
  :group 'pyregexp)

(defcustom pyregexp-auto-show-help t
  "Show help message automatically when the minibuffer is entered."
  :type 'boolean
  :group 'pyregexp)

(defcustom pyregexp-default-feedback-limit 50
  "Limit number of matches shown in visual feedback. 
If nil, don't limit the number of matches shown in visual feedback."
  :type 'integer
  :group 'pyregexp)

(defcustom pyregexp-default-replace-preview nil
  "Preview of replacement activated by default? If activated, the original is not shown alongside the replacement."
  :type 'boolean
  :group 'pyregexp)

(defcustom pyregexp-default-regexp-modifiers '(:I nil :M t :S nil :U nil)
  "Modifiers that are applied by default. All modifiers are: '(I M S U).
See also: http://docs.python.org/library/re.html#re.I"
  ;;:type '(choice (const 10) (const 5))

  :type '(plist :key-type (choice
			   (const :tag "Enable the IGNORECASE modifier by default" :I) 
			   (const :tag "Enable the MULTILINE modifier by default (^ and $ match on every line)" :M)
			   (const :tag "Enable the DOTALL modifier by default (dot matches newline)" :S)
			   (const :tag "Enable the UNICODE modifier by default" :U))
		:value-type boolean)
  :group 'pyregexp
  )

;;; private variables

(defconst pyregexp-match-faces '(pyregexp-match-0 pyregexp-match-1)
  "Faces in list for convenience")

(defconst pyregexp-group-faces '(pyregexp-group-0 pyregexp-group-1 pyregexp-group-2)
  "Faces in list for convenience")

(defconst pyregexp-overlay-priority 1001
  "Starting priority of pyregexp overlays.")

(defvar pyregexp-in-minibuffer nil
  "Is pyregexp currently being used?")

(defvar pyregexp-last-minibuffer-contents nil
  "Keeping track of minibuffer changes")

(defvar pyregexp-target-buffer-start nil
  "Starting position in target buffer.")

(defvar pyregexp-target-buffer-end nil
  "Ending position in target buffer.")

(defvar pyregexp-regexp-string nil
  "Entered regexp.")

(defvar pyregexp-replace-string nil
  "Entered replacement.")

(defvar pyregexp-use-expression nil
  "Use expression instead of string in replacement.")

(defvar pyregexp-feedback-limit nil
  "Feedback limit currently in use.")

(defvar pyregexp-replace-preview nil
  "Preview of replacement activated?")

(defvar pyregexp-target-buffer nil
  "Buffer to which pyregexp is applied to.")

(defvar pyregexp-overlays (make-hash-table :test 'equal)
  "Overlays used in target buffer.")

(defvar pyregexp-visible-overlays (list)
  "Overlays currently visible.")

;; modifiers IMSU (see http://docs.python.org/library/re.html#re.I)
(defvar pyregexp-regexp-modifiers '()
  "Modifiers in use.")


;; (make-variable-buffer-local 'pyregexp-in-minibuffer)
;; (make-variable-buffer-local 'pyregexp-last-minibuffer-contents)
;; (make-variable-buffer-local 'pyregexp-target-buffer-start)
;; (make-variable-buffer-local 'pyregexp-target-buffer-end)
;; (make-variable-buffer-local 'pyregexp-regexp-string)
;; (make-variable-buffer-local 'pyregexp-replace-string)
;; (make-variable-buffer-local 'pyregexp-use-expression)
;; (make-variable-buffer-local 'pyregexp-feedback-limit)
;; (make-variable-buffer-local 'pyregexp-target-buffer)
;; (make-variable-buffer-local 'pyregexp-overlays)
;; (make-variable-buffer-local 'pyregexp-visible-overlays)
;; (make-variable-buffer-local 'pyregexp-modifiers)

;;; keymap

(defvar pyregexp-minibuffer-regexp-keymap 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c ?") (lambda () (interactive) (pyregexp-minibuffer-help)))

    ;; C-i is also <tab>. http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
    (setq map (delq '(kp-tab . [9]) map))
    ;;(keyboard-translate ?\C-i ?\H-i)
    (define-key map (kbd "C-c i") (lambda () (interactive) (pyregexp-toggle-regexp-modifier :I)))
    (define-key map (kbd "C-c m") (lambda () (interactive) (pyregexp-toggle-regexp-modifier :M)))
    (define-key map (kbd "C-c s") (lambda () (interactive) (pyregexp-toggle-regexp-modifier :S)))
    (define-key map (kbd "C-c u") (lambda () (interactive) (pyregexp-toggle-regexp-modifier :U)))

    (define-key map (kbd "C-c a") 'pyregexp-toggle-limit)
    map)
  "Keymap used while using pyregexp,")

(defvar pyregexp-minibuffer-replace-keymap 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c ?") (lambda () (interactive) (pyregexp-minibuffer-help)))
    (define-key map (kbd "C-c C-c") (lambda () (interactive) 
				      (when (equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
					(setq pyregexp-use-expression (not pyregexp-use-expression))
					(pyregexp-update-minibuffer-prompt)
					(pyregexp-do-replace-feedback))))
    (define-key map (kbd "C-c m") (lambda ()
				    (interactive)
				    (when (equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
				      (pyregexp-delete-overlay-displays)
				      ;; wait for any input to redisplay replacements
				      (sit-for 100000000 t)
				      (pyregexp-do-replace-feedback))))
    (define-key map (kbd "C-c p") (lambda ()
				    (interactive)
				    (when (equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
				      (setq pyregexp-replace-preview (not pyregexp-replace-preview))
				      (pyregexp-do-replace-feedback))))
    
    (define-key map (kbd "C-c a") 'pyregexp-toggle-limit)
    map)
  "Keymap used while using pyregexp,")


;;; helper functions

(defun pyregexp-toggle-limit ()
  "Toggle the limit of overlays shown (default limit / no limit)"
  (interactive)
  (if pyregexp-feedback-limit
      (setq pyregexp-feedback-limit nil)
    (setq pyregexp-feedback-limit pyregexp-default-feedback-limit))
  (cond ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	 (pyregexp-regexp-feedback))
	((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	 (pyregexp-regexp-feedback t) ;; update overlays
	 (pyregexp-do-replace-feedback))))

(defun pyregexp-toggle-regexp-modifier(modifier)
  "modifier should be one of :I, :M, :S, :U."
  (plist-put pyregexp-regexp-modifiers modifier 
	     (not (plist-get pyregexp-regexp-modifiers modifier)))
  (pyregexp-update-minibuffer-prompt)
  (pyregexp-regexp-feedback))

(defun pyregexp-get-regexp-string ()
  (concat (pyregexp-get-regexp-modifiers-prefix) 
	  (if (equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp) 
	      (minibuffer-contents-no-properties) 
	    pyregexp-regexp-string)))

(defun pyregexp-get-regexp-modifiers-prefix ()
  "Construct (?imsu) prefix based on selected modifiers."
  (let ((s (mapconcat 'identity 
		      (delq nil (mapcar (lambda (m)
					  (when (plist-get pyregexp-regexp-modifiers m)
					    (cond ((equal m :I) "i")
						  ((equal m :M) "m")
						  ((equal m :S) "s")
						  ((equal m :U) "u")
						  (t nil))))
					(list :I :M :S :U)))
		      "")))
    (if (string= "" s) "" (format "(?%s)" s))))

;;; minibuffer functions

(defun pyregexp-minibuffer-set-prompt (prompt)
  "Updates minibuffer prompt. Call when minibuffer is active."
  (let ((inhibit-read-only t)) 
    (put-text-property (point-min) (minibuffer-prompt-end) 'display prompt)))

(defun pyregexp-update-minibuffer-prompt ()
  (when (and pyregexp-in-minibuffer (minibufferp))
    (pyregexp-minibuffer-set-prompt
     (cond ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	    (format "Regexp: %s" (pyregexp-get-regexp-modifiers-prefix)))
	   ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	    (concat
	     "Replace"
	     (when pyregexp-use-expression " (using expression)")
	     (format " (%s)" (pyregexp-get-regexp-string))
	     ": "))))))

(defun pyregexp-minibuffer-message (message)
  "Minibuffer message without timeout"
  (let ((minibuffer-message-timeout nil))
    (minibuffer-message message)))

(defun pyregexp-minibuffer-help ()
  (cond ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	 (pyregexp-minibuffer-message "C-c ?: help, C-c i: toggle case, C-c m: toggle multiline match of ^ and $, C-c s: toggle dot matches newline, C-c a: toggle show all"))
	((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	 (pyregexp-minibuffer-message "C-c ?: help, C-c C-c: toggle expression, C-c m: show matches/groups, C-c p: toggle preview, C-c a: toggle show all"))))

;;; overlay functions

(defun pyregexp-get-overlay (i j)
  "i: match index, j: submatch index"
  (let (overlay)
    (setq overlay (gethash (list i j) pyregexp-overlays))
    (unless overlay ;; create new one if overlay does not exist yet
      (progn 
	(setq overlay (make-overlay 0 0))
	(if (= 0 j)
	    (overlay-put overlay 'face (nth (mod i (length pyregexp-match-faces)) pyregexp-match-faces))
	  (overlay-put overlay 'face (nth (mod j (length pyregexp-group-faces)) pyregexp-group-faces)))
	(overlay-put overlay 'priority (+ pyregexp-overlay-priority (if (= j 0) 0 1)))
	(overlay-put overlay 'pyregexp-ij (list i j))
	(when (= j 0)
	  (overlay-put overlay 'intangible t))
	(puthash (list i j) overlay pyregexp-overlays)
	))
    overlay))

(defun pyregexp-delete-overlays ()
  "Delete all visible overlays."
  (mapc (lambda (overlay)
	     (delete-overlay overlay)) 
	   pyregexp-visible-overlays)
  (setq pyregexp-visible-overlays (list)))

(defun pyregexp-delete-overlay-displays ()
  "Delete the display of all visible overlays. Call before pyregexp-delete-overlays."
  (mapc (lambda (overlay)
	  (multiple-value-bind (i j) (overlay-get overlay 'pyregexp-ij)
	    (when (= 0 j)
	      (overlay-put overlay 'display nil)
	      (overlay-put overlay 'priority pyregexp-overlay-priority))))
	pyregexp-visible-overlays))

;;; hooks

(defun pyregexp-update (beg end len)
  (when (and pyregexp-in-minibuffer (minibufferp))
    ;; minibuffer-up temporarily deletes minibuffer contents before inserting new one.
    ;; don't do anything then as the messages shown my pyregexp are irritating while browsing the history.
    (unless (and (string= "" (minibuffer-contents-no-properties))
		 (equal last-command 'previous-history-element))
      ;; do something when minibuffer contents changes
      (unless (string= pyregexp-last-minibuffer-contents (minibuffer-contents-no-properties))
	(setq pyregexp-last-minibuffer-contents (minibuffer-contents-no-properties))
	;; minibuffer contents has changed, update visual feedback.
	;; not using after-change-hook because this hook applies to the whole minibuffer, including minibuffer-messages
	;; that disappear after a while.
	(cond ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	       (pyregexp-regexp-feedback))
	      ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	       (pyregexp-do-replace-feedback)))))))
(add-hook 'after-change-functions 'pyregexp-update)

(defun pyregexp-minibuffer-setup ()
  "Setup prompt and help when entering minibuffer."
  (when pyregexp-in-minibuffer
    (progn
      (pyregexp-update-minibuffer-prompt)
      (when pyregexp-auto-show-help (pyregexp-minibuffer-help)))))
(add-hook 'minibuffer-setup-hook 'pyregexp-minibuffer-setup)

;;; shell command / parsing functions
(defun pyregexp-command (command)
  (let ((stdout-buffer (generate-new-buffer (generate-new-buffer-name " *pyregex stdout*")))
	output
	exit-code)
    (with-current-buffer pyregexp-target-buffer
      (setq exit-code (call-process-region
		       pyregexp-target-buffer-start
		       pyregexp-target-buffer-end
		       shell-file-name
		       nil ;; don't delete region
		       stdout-buffer
		       nil ;; don't redisplay buffer
		       shell-command-switch
		       command)))
    (with-current-buffer stdout-buffer
      (setq output (buffer-string))
      (kill-buffer))
    (list output exit-code)))

(defun pyregexp-run-command (args success)
  (multiple-value-bind (output exit-code) (pyregexp-command args)
    (cond ((equal exit-code 0) 
	   (funcall success output))
	  ((equal exit-code 1)
	   (message "script failed:%s\n" output)))))

(defun pyregexp-not-last-line () 
  "Output of external script ends in one line of message and one empty line.
Return t if current line is not the line with the message."
  (save-excursion (= 0 (forward-line 2))))

(defun pyregexp-current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun pyregexp-unescape (s)
  "Replacement strings returned by external script have escaped newlines and backslashes (so that there can be one replacement per line). Unescape to get back original.
Escaped newlines are only unescaped if newline is not nil."
  (setq s (replace-regexp-in-string (regexp-quote "\\n") (regexp-quote "\n") s))
  (replace-regexp-in-string (regexp-quote "\\\\") (regexp-quote "\\") s))

(defun pyregexp-parse-matches (s callback)
  "Parse string s with positions of matches and groups as returned by external script. For each position, callback is called with arguments (i j begin end),
i being the match and j the group index and begin/end being the span of the match.
The message line is returned.
"
  (let (message-line) ;; store message line (last non-empty line of output)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (let ((offset pyregexp-target-buffer-start))
	(loop while (and (pyregexp-not-last-line) (/= (line-beginning-position) (line-end-position))) ;; loop until empty line is reached
	      for i from 0 do
	      (loop while (re-search-forward "\\([0-9]+\\) \\([0-9]+\\)" (line-end-position) t) ;; loop integer pairs in line
		    for j from 0 do
		    (let ((begin (+ offset (string-to-number (match-string 1))))
			  (end (+ offset (string-to-number (match-string 2)))))
		      (funcall callback i j begin end)))
	      (forward-line 1)))
      (setq message-line (pyregexp-unescape (pyregexp-current-line))))
    message-line))

(defun pyregexp-parse-replace (s)
  "Parse string s with positions of matches and replacements as returned by external script.
Returns a list, in reverse order, of (replacement begin end i) (i = index of match = index of corresponding overlay)
and the message line."
  (let ((replacements (list)) ;; store replacements (lines of output) in list
	message-line) ;; store message line (last non-empty line of output)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (loop while (and (pyregexp-not-last-line) (/= (line-beginning-position) (line-end-position))) ;; loop until empty line is reached
	    for i from 0 do 
	    (re-search-forward "\\([0-9]+\\) \\([0-9]+\\) " (line-end-position) t)
	    (let ((replacement (buffer-substring-no-properties (point) (line-end-position)))
		  (begin (+ pyregexp-target-buffer-start (string-to-number (match-string 1))))
		  (end (+ pyregexp-target-buffer-start (string-to-number (match-string 2)))))
	      (setq replacements (cons (list replacement begin end i) replacements)))
	    (forward-line 1))
      (setq message-line (pyregexp-unescape (pyregexp-current-line))))
    (list replacements message-line)))

;;; helper functions

(defun pyregexp-target-window ()
  (if pyregexp-target-buffer
      (get-buffer-window pyregexp-target-buffer)
    nil))

(defun pyregexp-compose-messages(&rest msgs)
  (mapconcat 'identity (delq nil (mapcar (lambda (msg) (if (or (not msg) (string= "" msg)) nil msg)) msgs)) " - "))

;;; show feedback functions

(defun pyregexp-regexp-feedback (&optional inhibit-message)
  "Show visual feedback for matches."
  (pyregexp-delete-overlays)
  (let ((limit-reached nil) 
	message-line)
    (setq message-line 
	  (pyregexp-run-command 
	   (format "%s matches --regexp %s %s" pyregexp-command-prefix (shell-quote-argument (pyregexp-get-regexp-string)) (when pyregexp-feedback-limit (format "--feedback-limit %s" pyregexp-feedback-limit)))
	   (lambda (output)
	     (pyregexp-parse-matches
	      output 
	      (lambda (i j begin end) 
		(when (= 0 i) ;; first match: if invisible, make it visible.
		  (with-selected-window (pyregexp-target-window)
		    (if (>= begin (window-end nil t))
			(goto-char begin))))
		(let ((overlay (pyregexp-get-overlay i j)))
		  (move-overlay overlay begin end pyregexp-target-buffer)
		  (setq pyregexp-visible-overlays (cons overlay pyregexp-visible-overlays)))
		;; mark if we have reached the specified feedback limit	  
		(when (and pyregexp-feedback-limit (= pyregexp-feedback-limit (1+ i)) )
		  (setq limit-reached t)))))))
    (unless inhibit-message
      (let ((msg (pyregexp-compose-messages message-line (when limit-reached (format "%s matches shown, hit C-c a to show all" pyregexp-default-feedback-limit)))))
	(unless (string= "" msg)
	  (pyregexp-minibuffer-message msg))))))

(defun pyregexp-format-replace-feedback (original replacement)
  (if pyregexp-replace-preview
      replacement
    (format "%s => %s" original replacement)))

(defun pyregexp-do-replace-feedback ()
  "Show visual feedback for replacements."
  (pyregexp-delete-overlay-displays)
  (let ((replace-string (minibuffer-contents-no-properties)))
    (pyregexp-run-command 
     (format "%s replace %s --feedback %s --regexp %s --replace %s" pyregexp-command-prefix (if pyregexp-use-expression "--eval" "") (when pyregexp-feedback-limit (format "--feedback-limit %s" pyregexp-feedback-limit)) (shell-quote-argument (pyregexp-get-regexp-string)) (shell-quote-argument replace-string))
     (lambda (output)
       (multiple-value-bind (replacements message-line) (pyregexp-parse-replace output)
	 ;; visual feedback for matches
	 (loop for replacement-info in replacements do 
	       (multiple-value-bind (replacement begin end i) replacement-info
		 (let* ((overlay (pyregexp-get-overlay i 0))
			(empty-match (equal (overlay-start overlay) (overlay-end overlay))))
		   (unless empty-match
		     (let ((original (with-current-buffer pyregexp-target-buffer (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))))
		       (overlay-put overlay 'display (pyregexp-format-replace-feedback original (pyregexp-unescape replacement)))
		       (overlay-put overlay 'priority (+ pyregexp-overlay-priority 2)))))))
	 
	 (unless (string= "" message-line)
	   (pyregexp-minibuffer-message message-line)))))))

;;; replace / pyregexp-replace

(defun pyregexp-do-replace (&optional silent)
  "Replace matches."
  (pyregexp-delete-overlay-displays)
  (pyregexp-delete-overlays)
  (let ((replace-string pyregexp-replace-string))
    (pyregexp-run-command 
     (format "%s replace %s --regexp %s --replace %s" pyregexp-command-prefix (if pyregexp-use-expression "--eval" "") (shell-quote-argument (pyregexp-get-regexp-string)) (shell-quote-argument replace-string))
     (lambda (output)
       (let ((replace-count 0)
	     (cumulative-offset 0)
	     match-data)
	 (multiple-value-bind (replacements message-line) (pyregexp-parse-replace output)
	   ;; replace in target buffer
	   (loop for replacement-info in replacements 
		 for counter from 0 do 
		 (setq replace-count (1+ replace-count))
		 (multiple-value-bind (replacement begin end i) replacement-info
		   ;; replace match
		   (let ((replacement (pyregexp-unescape replacement)))
		     (with-current-buffer pyregexp-target-buffer
		       (save-excursion
			 ;; first insert, then delete
			 ;; this ensures that if we had an active region before, the replaced match is still part of the region
			 (goto-char begin)
			 (insert replacement)
			 (setq cumulative-offset (+ cumulative-offset (- (point) end)))
			 (delete-char (- end begin)))))
		   (when (= 0 counter)
		       (setq match-data (list begin end)))))
	   (unless (or silent (string= "" message-line))
	     (pyregexp-minibuffer-message message-line)))
	 (set-match-data (list (+ cumulative-offset (nth 0 match-data)) (+ cumulative-offset (nth 1 match-data))))
	 replace-count)))))

(defun pyregexp-interactive-get-args ()
  "Get interactive args for the pyregexp-replace function."
  (unwind-protect 
      (progn
	(let ((buffer-read-only t)) ;; make target buffer
	  (when pyregexp-in-minibuffer (error "pyregexp already in use."))
	  (setq pyregexp-target-buffer (current-buffer))
	  (setq pyregexp-use-expression current-prefix-arg)
	  (setq pyregexp-target-buffer-start (if (and transient-mark-mode mark-active) 
						 (region-beginning)
					       (point)))
	  (setq pyregexp-target-buffer-end (if (and transient-mark-mode mark-active) 
					       (region-end)
					     (point-max)))

	  (setq pyregexp-feedback-limit pyregexp-default-feedback-limit)
	  (setq pyregexp-regexp-modifiers (copy-sequence pyregexp-default-regexp-modifiers))
	  (setq pyregexp-replace-preview pyregexp-default-replace-preview)

	  (save-excursion
	    ;; deactivate mark so that we can see our faces instead of region-face.
	    (deactivate-mark)
	    (progn 
	      (setq pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	      (setq pyregexp-last-minibuffer-contents "")
	      (setq pyregexp-regexp-string 
		    (read-from-minibuffer 
		     " " ;; prompt will be  set in pyregexp-minibuffer-setup
		     nil pyregexp-minibuffer-regexp-keymap))
	      ;;(setq pyregexp-regexp-string (format "%s%s" (pyregexp-get-regexp-modifiers-prefix) pyregexp-regexp-string))
	      
	      (setq pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	      (setq pyregexp-last-minibuffer-contents "")
	      (setq pyregexp-replace-string
		    (read-from-minibuffer 
		     " " ;; prompt will be  set in pyregexp-minibuffer-setup 
		     nil pyregexp-minibuffer-replace-keymap))))
	  ;; Successfully got the args, deactivate mark now. If the command was aborted (C-g), the mark (region) would remain active.
	  (deactivate-mark)
	  (list pyregexp-regexp-string 
		pyregexp-replace-string
		pyregexp-target-buffer-start
		pyregexp-target-buffer-end
		pyregexp-regexp-modifiers
		pyregexp-use-expression)))
    (progn ;; execute on finish
      (setq pyregexp-in-minibuffer nil)
      (pyregexp-delete-overlay-displays)
      (pyregexp-delete-overlays))))

(defun pyregexp-replace (regexp replace start end &optional modifiers use-expression)
  "Regexp-replace with interactive feedback, using Python regular expressions. 
When used interactively with prefix arg, the replacement string is a Python expression. The Python expression has access to the following variables:
- i: the index of the match
- m: the match object
- \\0, \\1, ...: captured groups (those are aliases for m.group(0), m.group(1), ...).

Example 1: 
regexp: abcd(.)(.)
replace: abc\\2\\1

Example 2: capitalize every word (use prefix arg to use a Python expression)
regexp: \\b\\w
replace: \\0.upper()

Example 3: enumerate all words and put them on new lines (use prefix arg to use a Python expression)
regexp: \\w+
replace: \"\\n{}. {}\".format(i+1, \\0)
"
  (interactive 
   (pyregexp-interactive-get-args))
  (unwind-protect 
      (progn 
	(when pyregexp-in-minibuffer (error "pyregexp already in use."))
	(setq pyregexp-target-buffer (current-buffer)
	      pyregexp-target-buffer-start start
	      pyregexp-target-buffer-end end
	      pyregexp-regexp-modifiers (if modifiers modifiers (copy-sequence pyregexp-default-regexp-modifiers))
	      pyregexp-use-expression use-expression
	      pyregexp-regexp-string regexp
	      pyregexp-replace-string replace)
	;; do replacement
	(pyregexp-do-replace))
    ;; execute on finish
    (setq pyregexp-in-minibuffer nil)))


;; isearch starts here

(defun pyregexp-isearch-forward()
  "Like isearch-forward, but using Python regular expressions."
  (interactive)
  (let ((isearch-search-fun-function 'pyregexp-isearch-search-fun-function))
    (isearch-forward-regexp)))

(defun pyregexp-isearch-backward()
  "Like isearch-backward, but using Python regular expressions."
  (interactive)
  (let ((isearch-search-fun-function 'pyregexp-isearch-search-fun-function))
    (isearch-backward-regexp)))

(defvar pyregexp--isearch-cache-key nil)
(defvar pyregexp--isearch-cache-val nil)

(defun pyregexp--isearch-forward (string &optional bound noerror count)
  (pyregexp--isearch t string bound noerror count))

(defun pyregexp--isearch-backward (string &optional bound noerror count)
  (pyregexp--isearch nil string bound noerror count))

(defun pyregexp--isearch-find-match (matches start)
  (let ((i (pyregexp--isearch-find-match-bsearch matches start 0 (- (length matches) 1))))
    (unless (eq i -1)
      (aref matches i))))

(defun pyregexp--isearch-find-match-bsearch (matches start left right)
  (if (= 0 (length matches))
      -1
    (let ((mid (/ (+ left right) 2))
	  (el (if forward 0 1)) ;; 0 => beginning of match; 1 => end of match
	  (cmp (if forward '<= '>=)))
      (cond ((eq left right)
	     (if (funcall cmp start (nth el (aref matches mid)))
		 left
	       -1)
	     )
	    ((funcall cmp start (nth el (aref matches mid)))
	     (pyregexp--isearch-find-match-bsearch matches start left mid))
	    (t
	     (pyregexp--isearch-find-match-bsearch matches start (1+ mid) right))))))

(defun pyregexp--isearch (forward string &optional bound noerror count)
  ;; This is be called from isearch. In the first call, bound will be nil to find the next match.
  ;; Afterwards, lazy highlighting kicks in, which calls this function many times, for different values of (point), always with the same bound (window-end (selected-window)).
  ;; Calling a process repeatedly is noticeably  slow. To speed the lazy highlighting up, we fetch all matches in the visible window at once and cache them for subsequent calls.
  (let* ((is-called-from-lazy-highlighting bound) ;; we assume only lazy highlighting sets a bound. isearch does not, and neither does our own pyregexp-query-replace.
	 matches-vec ;; stores matches from pyregexp.py
	 message-line ;; message from pyregexp.py
	 (regexp (if case-fold-search (concat "(?i)" string) string))
	 (start
	  (if forward 
	      (if is-called-from-lazy-highlighting (window-start (selected-window)) (point))
	    (if is-called-from-lazy-highlighting bound (point-min))))
	 (end
	  (if forward
	      (if is-called-from-lazy-highlighting bound (point-max))
	    (if is-called-from-lazy-highlighting (window-end (selected-window)) (point))))
	 (cache-key (list regexp start end)))
    (if (and is-called-from-lazy-highlighting (equal pyregexp--isearch-cache-key cache-key))
	(setq matches-vec pyregexp--isearch-cache-val) ;; cache hit
      (progn ;; no cache hit, populate matches-vec
	(setq pyregexp-target-buffer-start start
	      pyregexp-target-buffer-end end
	      pyregexp-target-buffer (current-buffer))
	(pyregexp-run-command 
	 (format "%s matches --regexp %s %s %s" 
		 pyregexp-command-prefix (shell-quote-argument regexp) 
		 (if count
		     (format "--feedback-limit %s" count)
		   ;; if no bound, the rest of the buffer is searched for the first match -> need only one match
		   (if bound "" "--feedback-limit 1")) 
		 (if forward "" "--backwards"))
	 (lambda (output)
	   ;; initialize matches-vec
	   (setq matches-vec (make-vector 
			      (with-temp-buffer (insert output) (- (line-number-at-pos (point-max)) 2)) ;; number of matches
			      nil))
	   (let ((cur-match (list)))
	     (setq message-line
		   ;; populate matches-vec 
		   (pyregexp-parse-matches
		    output 
		    (lambda (i j begin end) 
		      (progn
			(when (and (= j 0) (> i 0))
			  (aset matches-vec (- i 1) (nreverse cur-match))
			  (setq cur-match (list)))
			(setq cur-match (cons end (cons begin cur-match)))))))
	     (when cur-match
	       (aset matches-vec (- (length matches-vec) 1) (nreverse cur-match))))
	   (when is-called-from-lazy-highlighting ;; store in cache
	     (setq pyregexp--isearch-cache-key cache-key
		   pyregexp--isearch-cache-val matches-vec))))))
    (let ((match (pyregexp--isearch-find-match matches-vec (point))))
      (if match
	  (progn
	    (set-match-data (mapcar 'copy-marker match)) ;; needed for isearch 
	    (if forward
		(goto-char (nth 1 match)) ;; move to end of match
	      (goto-char (nth 0 match)) ;; move to beginning of match
	      ))
	(progn 
	  (set-match-data (list 0 0))
	  (when (string= "Invalid:" (substring message-line 0 8))
	    (signal 'invalid-regexp (list message-line))))))))

(defun pyregexp-isearch-search-fun-function ()
  "To enable pyregexp-isearch, set isearch-search-fun-function to pyregexp-isearch-search-fun-function, i.e. `(setq isearch-search-fun-function 'pyregexp-isearch-search-fun-function)`."
  ;; isearch-search-fun is a function that returns the function that does the search. it calls isearch-search-fun-function (if it exists) to do its job.
  (if isearch-regexp ;; let us handle regexp search
      (if isearch-forward 'pyregexp--isearch-forward 'pyregexp--isearch-backward)
    (let ((isearch-search-fun-function nil)) ;; fall back to the default implementation of isearch, which will handle regular search and word search.
      (isearch-search-fun))))

(add-hook 'isearch-mode-end-hook (lambda ()
				   (setq pyregexp--isearch-cache-key nil
					 pyregexp--isearch-cache-val nil)))


;; query-replace-regexp starts here

(defvar pyregexp--query-replacements nil)
;; we redefine the help text from replace.el to remove the commands we don't support.

(defconst pyregexp--query-replace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
RET or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r [not supported in pyregexp],
C-w to delete match and recursive edit,
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ [not supported in pyregexp],
E [not supported in pyregexp]"
  "Help message while in `pyregexp-query-replace'.")

(defvar pyregexp--query-replace-map 
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map query-replace-map)
    ;; the following replace.el commands are not supported by pyregexp.
    (define-key map "e" nil)
    (define-key map "E" nil)
    (define-key map "\C-r" nil)
    (define-key map "\C-w" nil)
    (define-key map "^" nil)
    map
    ))

;; (defun pyregexp-looking-at (regexp)
;;   (let ((p (point)))
;;     (save-excursion (when (pyregexp--isearch-forward regexp nil t 1)
;; 		      (= p (match-beginning 0))))))

(defun pyregexp-query-replace (regexp replace start end &optional modifiers use-expression)
  "Use pyregexp-query-replace like you would use query-replace-regexp."
  (interactive 
   (pyregexp-interactive-get-args))

  (unwind-protect 
      (progn 
	(when pyregexp-in-minibuffer (error "pyregexp already in use."))
	(setq pyregexp-target-buffer (current-buffer)
	      pyregexp-target-buffer-start start
	      pyregexp-target-buffer-end end
	      pyregexp-regexp-modifiers (if modifiers modifiers (copy-sequence pyregexp-default-regexp-modifiers))
	      pyregexp-use-expression use-expression
	      pyregexp-regexp-string regexp
	      pyregexp-replace-string replace)

	(pyregexp-run-command 
	 (format "%s replace %s --regexp %s --replace %s" pyregexp-command-prefix (if pyregexp-use-expression "--eval" "") (shell-quote-argument (pyregexp-get-regexp-string)) (shell-quote-argument pyregexp-replace-string))
	 (lambda (output)
	   (setq pyregexp--query-replacements (nreverse (car (pyregexp-parse-replace output))))))
	(pyregexp--perform-replace regexp nil))
    ;; execute on finish
    (setq pyregexp-in-minibuffer nil)))

(defun pyregexp--perform-replace (from-string &optional map)
  ;; This function is a heavily modified version of (perform-replace) from replace.el.
  ;; The original plan was to use the original perform-replace, but various issues stood in the way.
  (or map (setq map pyregexp--query-replace-map))
  (and minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let* (
	 ;; isearch is used for highlighting. plug in our search function such that highlighting works correctly.
	 (isearch-search-fun-function 'pyregexp-isearch-search-fun-function)
	 (search-function 'pyregexp--isearch-forward)
	 (query-replace-help pyregexp--query-replace-help)
	 (search-string from-string)
	 (real-match-data nil) ; The match data for the current match.
	 (next-replacement nil) ;; replacement string for current match
	 (keep-going t)
	 (replace-count 0)
	 (automatic nil)
	 ;; a match can be replaced by a longer/shorter replacement. cumulate the difference
	 (cumulative-offset 0)
	 (end pyregexp-target-buffer-end)
	 (recenter-last-op nil)	; Start cycling order with initial position.
	 (message
	  (apply 'propertize
		 (substitute-command-keys
		  "Query replacing %s with %s: (\\<pyregexp--query-replace-map>\\[help] for help) ")
		 minibuffer-prompt-properties)))

    (when query-replace-lazy-highlight
      (setq isearch-lazy-highlight-last-string nil))

    (goto-char pyregexp-target-buffer-start)
    (push-mark)
    (undo-boundary)
    (unwind-protect
	;; Loop finding occurrences that perhaps should be replaced.
	(while (and keep-going pyregexp--query-replacements)
	  ;; Advance replacement list
	  (multiple-value-bind (replacement begin end) (car pyregexp--query-replacements)
	    (set-match-data (mapcar 'copy-marker (list (+ cumulative-offset begin) (+ cumulative-offset end))))
	    (setq next-replacement replacement))
	  (goto-char (match-end 0))
	  (setq real-match-data (replace-match-data t real-match-data))
	  (setq pyregexp--query-replacements (cdr pyregexp--query-replacements))

	  (undo-boundary)
	  (let (done replaced key def)
	    ;; Loop reading commands until one of them sets done,
	    ;; which means it has finished handling this
	    ;; occurrence. 
	    ;; Commands not setting `done' need to adjust
	    ;; `real-match-data'.
	    (while (not done)
	      (replace-highlight (match-beginning 0) (match-end 0) start end search-string t nil)
	      ;; Bind message-log-max so we don't fill up the message log
	      ;; with a bunch of identical messages.
	      (let ((message-log-max nil))
		(message message
			 (query-replace-descr from-string)
			 (query-replace-descr next-replacement)))
	      (setq key (read-event))
	      ;; Necessary in case something happens during read-event
	      ;; that clobbers the match data.
	      (set-match-data real-match-data)
	      (setq key (vector key))
	      (setq def (lookup-key map key))
	      ;; Restore the match data while we process the command.
	      (set-match-data real-match-data)
	      (cond ((eq def 'help)
		     (with-output-to-temp-buffer "*Help*"
		       (princ
			(concat "Query replacing pyregexp "
				from-string " with "
				next-replacement ".\n\n"
				(substitute-command-keys
				 query-replace-help)))
		       (with-current-buffer standard-output
			 (help-mode))))
		    ((eq def 'exit)
		     (setq keep-going nil
			   done t))
		    ((eq def 'act)
		     (unless replaced
		       (replace-match next-replacement t t)
		       (setq replace-count (1+ replace-count)))
		     (setq done t 
			   replaced t))
		    ((eq def 'act-and-exit)
		     (unless replaced
		       (replace-match next-replacement t t)
		       (setq replace-count (1+ replace-count)))
		     (setq keep-going nil
			   done t r
			   eplaced t))
		    ((eq def 'act-and-show)
		     (unless replaced
		       (replace-match next-replacement t t)
		       (setq replace-count (1+ replace-count)
			     real-match-data (replace-match-data t real-match-data))
		       (setq replaced t)))
		    ((eq def 'automatic)
		     (setq pyregexp-target-buffer-start (match-beginning 0)
			   pyregexp-target-buffer-end (+ cumulative-offset end))
		     (setq replace-count (+ replace-count (pyregexp-do-replace t)))
		     (setq done t 
			   replaced t 
			   keep-going nil))
		    ((eq def 'skip)
		     (setq done t))
		    ((eq def 'recenter)
		     ;; `this-command' has the value `query-replace',
		     ;; so we need to bind it to `recenter-top-bottom'
		     ;; to allow it to detect a sequence of `C-l'.
		     (let ((this-command 'recenter-top-bottom)
			   (last-command 'recenter-top-bottom))
		       (recenter-top-bottom)))
		    (t
		     (setq this-command 'mode-exited)
		     (setq keep-going nil)
		     (setq unread-command-events
			   (append (listify-key-sequence key)
				   unread-command-events))
		     (setq done t)))
	      (when replaced
		(setq cumulative-offset (+ cumulative-offset (- (length next-replacement) (- (nth 1 real-match-data) (nth 0 real-match-data))))))
	      (when query-replace-lazy-highlight
		;; Force lazy rehighlighting only after replacements.
		(if (not (memq def '(skip backup)))
		    (setq isearch-lazy-highlight-last-string nil)))
	      (unless (eq def 'recenter)
		;; Reset recenter cycling order to initial position.
		(setq recenter-last-op nil)))))
            
      (replace-dehighlight))
    (unless unread-command-events
      ;; point is set to the end of the last occurrence.
      (goto-char (match-end 0))
      (message "Replaced %d occurrence%s"
	       replace-count
	       (if (= replace-count 1) "" "s")))))


(provide 'pyregexp)

;;; pyregexp.el ends here

