;;; ggtags.el --- GNU Global source code tagging system -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.6.7
;; Keywords: tools, convenience
;; Created: 2013-01-29
;; URL: https://github.com/leoliu/ggtags

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A package to integrate GNU Global source code tagging system
;; (http://www.gnu.org/software/global) with Emacs.
;;
;; Usage:
;;
;; Type `M-x ggtags-mode' to enable the minor mode, or as usual enable
;; it in your desired major mode hooks. When the mode is on the symbol
;; at point is underlined if it is a valid (definition) tag.
;;
;; `M-.' finds definition or references according to the context at
;; point, i.e. if point is at a definition tag find references and
;; vice versa. `C-u M-.' is verbose and will ask you the name - with
;; completion - and the type of tag to search.
;;
;; If multiple matches are found, navigation mode is entered. In this
;; mode, `M-n' and `M-p' moves to next and previous match, `M-}' and
;; `M-{' to next and previous file respectively. `M-o' toggles between
;; full and abbreviated displays of file names in the auxiliary popup
;; window. When you locate the right match, press RET to finish which
;; hides the auxiliary window and exits navigation mode. You can
;; resume the search using `M-,'. To abort the search press `M-*'.
;;
;; Normally after a few searches a dozen buffers are created visiting
;; files tracked by GNU Global. `C-c M-k' helps clean them up.

;;; Code:

(eval-when-compile (require 'cl))
(require 'compile)

(if (not (fboundp 'comment-string-strip))
    (autoload 'comment-string-strip "newcomment"))

(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      (list 'set (list 'make-local-variable (list 'quote var)) val)))

  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var))))))

(eval-and-compile
  (unless (fboundp 'user-error)
    (defalias 'user-error 'error)))

(defgroup ggtags nil
  "GNU Global source code tagging system."
  :group 'tools)

(defface ggtags-highlight '((t (:underline t)))
  "Face used to highlight a valid tag at point.")

(defcustom ggtags-auto-jump-to-first-match t
  "Non-nil to automatically jump to the first match."
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-global-window-height 8 ; ggtags-global-mode
  "Number of lines for the 'global' popup window.
If nil, use Emacs default."
  :type '(choice (const :tag "Default" nil) integer)
  :group 'ggtags)

(defcustom ggtags-global-abbreviate-filename 35
  "Non-nil to display file names abbreviated such as '/u/b/env'."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Always" t)
                 integer)
  :group 'ggtags)

(defcustom ggtags-oversize-limit (* 50 1024 1024)
  "The over size limit for the  GTAGS file."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Always" t)
                 number)
  :group 'ggtags)

(defcustom ggtags-split-window-function split-window-preferred-function
  "A function to control how ggtags pops up the auxiliary window."
  :type 'function
  :group 'ggtags)

(defcustom ggtags-global-output-format 'grep
  "The output format for the 'global' command."
  :type '(choice (const path)
                 (const ctags)
                 (const ctags-x)
                 (const grep)
                 (const cscope))
  :group 'ggtags)

(defcustom ggtags-completing-read-function completing-read-function
  "Ggtags specific `completing-read-function' (which see)."
  :type 'function
  :group 'ggtags)

(defvar ggtags-cache nil)               ; (ROOT TABLE DIRTY TIMESTAMP)

(defvar ggtags-current-tag-name nil)

;; Used by ggtags-global-mode
(defvar ggtags-global-error "match"
  "Stem of message to print when no matches are found.")

;; http://thread.gmane.org/gmane.comp.gnu.global.bugs/1518
(defvar ggtags-global-has-path-style    ; introduced in global 6.2.8
  (with-demoted-errors                  ; in case `global' not found
    (and (string-match-p "^--path-style "
                         (shell-command-to-string "global --help"))
         t))
  "Non-nil if `global' supports --path-style switch.")

(defmacro ggtags-ensure-global-buffer (&rest body)
  (declare (indent 0))
  `(progn
     (or (and (buffer-live-p compilation-last-buffer)
              (with-current-buffer compilation-last-buffer
                (derived-mode-p 'ggtags-global-mode)))
         (error "No global buffer found"))
     (with-current-buffer compilation-last-buffer ,@body)))

(defun ggtags-oversize-p ()
  (pcase ggtags-oversize-limit
    (`nil nil)
    (`t t)
    (t (when (ggtags-root-directory)
         (> (or (nth 7 (file-attributes
                        (expand-file-name "GTAGS" (ggtags-root-directory))))
                0)
            ggtags-oversize-limit)))))

(defun ggtags-get-timestamp (root)
  "Get the timestamp (float) of file GTAGS in ROOT directory.
Return -1 if it does not exist."
  (let ((file (expand-file-name "GTAGS" root)))
    (if (file-exists-p file)
        (float-time (nth 5 (file-attributes file)))
      -1)))

(defun ggtags-get-libpath ()
  (split-string (or (getenv "GTAGSLIBPATH") "")
                (regexp-quote path-separator) t))

(defun ggtags-cache-get (key)
  (assoc key ggtags-cache))

(defun ggtags-cache-set (key val &optional dirty)
  (let ((c (ggtags-cache-get key)))
    (if c
        (setcdr c (list val dirty (float-time)))
      (push (list key val dirty (float-time)) ggtags-cache))))

(defun ggtags-cache-mark-dirty (key flag)
  "Return non-nil if operation is successful."
  (let ((cache (ggtags-cache-get key)))
    (when cache
      (setcar (cddr cache) flag))))

(defun ggtags-cache-dirty-p (key)
  "Value is non-nil if 'global -u' is needed."
  (third (ggtags-cache-get key)))

(defun ggtags-cache-stale-p (key)
  "Value is non-nil if tags in cache needs to be rebuilt."
  (> (ggtags-get-timestamp key)
     (or (fourth (ggtags-cache-get key)) 0)))

(defvar-local ggtags-root-directory nil
  "Internal; use function `ggtags-root-directory' instead.")

;;;###autoload
(defun ggtags-root-directory ()
  (or ggtags-root-directory
      (setq ggtags-root-directory
            (with-temp-buffer
              (when (zerop (call-process "global" nil (list t nil) nil "-pr"))
                (file-name-as-directory
                 (comment-string-strip (buffer-string) t t)))))))

(defun ggtags-check-root-directory ()
  (or (ggtags-root-directory) (error "File GTAGS not found")))

(defun ggtags-ensure-root-directory ()
  (or (ggtags-root-directory)
      (when (or (yes-or-no-p "File GTAGS not found; run gtags? ")
                (error "Aborted"))
        (let ((root (read-directory-name "Directory: " nil nil t)))
          (and (= (length root) 0) (error "No directory chosen"))
          (when (with-temp-buffer
                  (let ((default-directory
                          (file-name-as-directory root)))
                    (or (zerop (call-process "gtags" nil t))
                        (error "%s" (comment-string-strip
                                     (buffer-string) t t)))))
            (message "File GTAGS generated in `%s'"
                     (ggtags-root-directory)))))))

(defun ggtags-tag-names-1 (root &optional from-cache)
  (when root
    (if (and (not from-cache) (ggtags-cache-stale-p root))
        (let* ((default-directory (file-name-as-directory root))
               (tags (with-demoted-errors
                       (process-lines "global" "-c" ""))))
          (and tags (ggtags-cache-set root tags))
          tags)
      (cadr (ggtags-cache-get root)))))

;;;###autoload
(defun ggtags-tag-names (&optional from-cache)
  "Get a list of tag names."
  (let ((root (ggtags-root-directory)))
    (when (and root
               (not (ggtags-oversize-p))
               (not from-cache)
               (ggtags-cache-dirty-p root))
      (if (zerop (call-process "global" nil nil nil "-u"))
          (ggtags-cache-mark-dirty root nil)
        (message "ggtags: error running 'global -u'")))
    (apply 'append (mapcar (lambda (r)
                             (ggtags-tag-names-1 r from-cache))
                           (cons root (ggtags-get-libpath))))))

(defun ggtags-read-tag (quick)
  (ggtags-ensure-root-directory)
  (let ((default (thing-at-point 'symbol))
        (completing-read-function ggtags-completing-read-function))
    (setq ggtags-current-tag-name
          (if quick (or default (user-error "No tag at point"))
            (completing-read
             (format (if default "Tag (default %s): " "Tag: ") default)
             ;; XXX: build tag names more lazily such as using
             ;; `completion-table-dynamic'.
             (ggtags-tag-names)
             nil t nil nil default)))))

(defun ggtags-global-options ()
  (concat "-v --result="
          (symbol-name ggtags-global-output-format)
          (and ggtags-global-has-path-style " --path-style=shorter")))

;;;###autoload
(defun ggtags-find-tag (name &optional verbose)
  "Find definitions or references to tag NAME by context.
If point is at a definition tag, find references, and vice versa.
When called with prefix, ask the name and kind of tag."
  (interactive (list (ggtags-read-tag (not current-prefix-arg))
                     current-prefix-arg))
  (ggtags-check-root-directory)
  (let ((split-window-preferred-function ggtags-split-window-function)
        (default-directory (ggtags-root-directory))
        (help-char ??)
        (help-form "\
d: definitions          (-d)
r: references           (-r)
s: symbols              (-s)
?: show this help\n"))
    (compilation-start
     (if (or verbose (not buffer-file-name))
         (format "global %s -%s \"%s\""
                 (ggtags-global-options)
                 (char-to-string
                  (read-char-choice "Tag type? (d/r/s/?) " '(?d ?r ?s)))
                 name)
       (format "global %s --from-here=%d:%s \"%s\""
               (ggtags-global-options)
               (line-number-at-pos)
               (shell-quote-argument
                (expand-file-name (file-truename buffer-file-name)))
               name))
     'ggtags-global-mode))
  (eval-and-compile (require 'etags))
  (ring-insert find-tag-marker-ring (point-marker))
  (ggtags-navigation-mode +1))

(defun ggtags-find-tag-resume ()
  (interactive)
  (ggtags-ensure-global-buffer
    (ggtags-navigation-mode +1)
    (let ((split-window-preferred-function ggtags-split-window-function))
      (compile-goto-error))))

;; NOTE: Coloured output in grep requested: http://goo.gl/Y9IcX
(defun ggtags-list-tags (regexp file-or-directory)
  "List all tags matching REGEXP in FILE-OR-DIRECTORY."
  (interactive (list (read-string "POSIX regexp: ")
                     (read-file-name "Directory: "
                                     (if current-prefix-arg
                                         (ggtags-root-directory)
                                       default-directory)
                                     buffer-file-name t)))
  (let ((split-window-preferred-function ggtags-split-window-function)
        (default-directory (if (file-directory-p file-or-directory)
                               (file-name-as-directory file-or-directory)
                             (file-name-directory file-or-directory))))
    (ggtags-check-root-directory)
    (eval-and-compile (require 'etags))
    (ggtags-navigation-mode +1)
    (ring-insert find-tag-marker-ring (point-marker))
    (with-current-buffer
        (compilation-start (format "global %s -e %s %s"
                                   (ggtags-global-options)
                                   regexp
                                   (if (file-directory-p file-or-directory)
                                       "-l ."
                                     (concat "-f " (shell-quote-argument
                                                    (file-name-nondirectory
                                                     file-or-directory)))))
                           'ggtags-global-mode)
      (setq-local compilation-auto-jump-to-first-error nil)
      (remove-hook 'compilation-finish-functions 'ggtags-handle-single-match t))))

(defun ggtags-query-replace (from to &optional delimited directory)
  "Query replace FROM with TO on all files in DIRECTORY."
  (interactive
   (append (query-replace-read-args "Query replace (regexp)" t t)
           (list (read-directory-name "In directory: " nil nil t))))
  (let ((default-directory (file-name-as-directory directory)))
    (ggtags-check-root-directory)
    (dolist (file (process-lines "global" "-P" "-l" "."))
      (let ((file (expand-file-name file directory)))
        (when (file-exists-p file)
          (let* ((message-log-max nil)
                 (visited (get-file-buffer file))
                 (buffer (or visited
                             (with-demoted-errors
                               (find-file-noselect file)))))
            (when buffer
              (set-buffer buffer)
              (if (save-excursion
                    (goto-char (point))
                    (re-search-forward from nil t))
                  (progn
                    (switch-to-buffer (current-buffer))
                    (perform-replace from to t t delimited
                                     nil multi-query-replace-map))
                (message "Nothing to do for `%s'" file)
                (or visited (kill-buffer))))))))))

(defun ggtags-delete-tag-files ()
  "Delete the tag files generated by gtags."
  (interactive)
  (when (ggtags-root-directory)
    (let ((files (directory-files (ggtags-root-directory) t
                                  (regexp-opt '("GPATH" "GRTAGS" "GTAGS" "ID"))))
          (buffer "*GTags File List*"))
      (or files (user-error "No tag files found"))
      (with-output-to-temp-buffer buffer
        (dolist (file files)
          (princ file)
          (princ "\n")))
      (let ((win (get-buffer-window buffer)))
        (unwind-protect
            (progn
              (fit-window-to-buffer win)
              (when (yes-or-no-p "Remove GNU Global tag files? ")
                (mapc 'delete-file files)))
          (when (window-live-p win)
            (quit-window t win)))))))

(defvar ggtags-current-mark nil)

(defun ggtags-next-mark (&optional arg)
  "Move to the next mark in the tag marker ring."
  (interactive)
  (or (> (ring-length find-tag-marker-ring) 1)
      (user-error "No %s mark" (if arg "previous" "next")))
  (let ((mark (or (and ggtags-current-mark
                       (marker-buffer ggtags-current-mark)
                       (funcall (if arg #'ring-previous #'ring-next)
                                find-tag-marker-ring ggtags-current-mark))
                  (progn
                    (ring-insert find-tag-marker-ring (point-marker))
                    (ring-ref find-tag-marker-ring 0)))))
    (switch-to-buffer (marker-buffer mark))
    (goto-char mark)
    (setq ggtags-current-mark mark)))

(defun ggtags-prev-mark ()
  (interactive)
  (ggtags-next-mark 'previous))

(defvar-local ggtags-global-exit-status nil)

(defun ggtags-global-exit-message-function (_process-status exit-status msg)
  (setq ggtags-global-exit-status exit-status)
  (let ((count (save-excursion
                 (goto-char (point-max))
                 (if (re-search-backward "^\\([0-9]+\\) \\w+ located" nil t)
                     (string-to-number (match-string 1))
                   0))))
    (cons (if (> exit-status 0)
              msg
            (format "found %d %s" count (if (= count 1) "match" "matches")))
          exit-status)))

;;; NOTE: Must not match the 'Global started at Mon Jun 3 10:24:13'
;;; line or `compilation-auto-jump' will jump there and fail. See
;;; comments before the 'gnu' entry in
;;; `compilation-error-regexp-alist-alist'.
(defvar ggtags-global-error-regexp-alist-alist
  (append
   '((path "^\\(?:[^/\n]*/\\)?[^ )\t\n]+$" 0)
     ;; ACTIVE_ESCAPE	src/dialog.cc	172
     (ctags "^\\([^ \t\n]+\\)[ \t]+\\(.*?\\)[ \t]+\\([0-9]+\\)$"
            2 3 nil nil 2 (1 font-lock-function-name-face))
     ;; ACTIVE_ESCAPE     172 src/dialog.cc    #undef ACTIVE_ESCAPE
     (ctags-x "^\\([^ \t\n]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(\\(?:[^/\n]*/\\)?[^ \t\n]+\\)"
              3 2 nil nil 3 (1 font-lock-function-name-face))
     ;; src/dialog.cc:172:#undef ACTIVE_ESCAPE
     (grep "^\\(.+?\\):\\([0-9]+\\):\\(?:[^0-9\n]\\|[0-9][^0-9\n]\\|[0-9][0-9].\\)"
           1 2 nil nil 1)
     ;; src/dialog.cc ACTIVE_ESCAPE 172 #undef ACTIVE_ESCAPE
     (cscope "^\\(.+?\\)[ \t]+\\([^ \t\n]+\\)[ \t]+\\([0-9]+\\).*\\(?:[^0-9\n]\\|[^0-9\n][0-9]\\|[^:\n][0-9][0-9]\\)$"
             1 3 nil nil 1 (2 font-lock-function-name-face)))
   compilation-error-regexp-alist-alist))

(defun ggtags-abbreviate-file (start end)
  (let ((inhibit-read-only t)
        (amount (if (numberp ggtags-global-abbreviate-filename)
                    (- (- end start) ggtags-global-abbreviate-filename)
                  999))
        (advance-word (lambda ()
                        "Return the length of the text made invisible."
                        (let ((wend (min end (progn (forward-word 1) (point))))
                              (wbeg (max start (progn (backward-word 1) (point)))))
                          (goto-char wend)
                          (if (<= (- wend wbeg) 1)
                              0
                            (put-text-property (1+ wbeg) wend 'invisible t)
                            (1- (- wend wbeg)))))))
    (goto-char start)
    (while (and (> amount 0) (> end (point)))
      (decf amount (funcall advance-word)))))

(defun ggtags-abbreviate-files (start end)
  (goto-char start)
  (let* ((error-re (cdr (assq ggtags-global-output-format
                              ggtags-global-error-regexp-alist-alist)))
         (sub (cadr error-re)))
    (when (and ggtags-global-abbreviate-filename error-re)
      (while (re-search-forward (car error-re) end t)
        (when (and (or (not (numberp ggtags-global-abbreviate-filename))
                       (> (length (match-string sub))
                          ggtags-global-abbreviate-filename))
                   ;; Ignore bogus file lines such as:
                   ;;     Global found 2 matches at Thu Jan 31 13:45:19
                   (get-text-property (match-beginning sub) 'compilation-message))
          (ggtags-abbreviate-file (match-beginning sub) (match-end sub)))))))

(defun ggtags-handle-single-match (buf _how)
  (when (and ggtags-auto-jump-to-first-match
             ;; If exit abnormally keep the window for inspection.
             (zerop ggtags-global-exit-status)
             (save-excursion
               (goto-char (point-min))
               (not (ignore-errors
                      (goto-char (compilation-next-single-property-change
                                  (point) 'compilation-message))
                      (end-of-line)
                      (compilation-next-single-property-change
                       (point) 'compilation-message)))))
    (ggtags-navigation-mode -1)
    ;; 0.5s delay for `ggtags-auto-jump-to-first-match'
    (sit-for 0)                    ; See: http://debbugs.gnu.org/13829
    (ggtags-navigation-mode-cleanup buf 0.5)))

(defvar ggtags-global-mode-font-lock-keywords
  '(("^Global \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (1 'compilation-error)
     (2 'compilation-error nil t))
    ("^Global found \\([0-9]+\\)" (1 compilation-info-face))))

(define-compilation-mode ggtags-global-mode "Global"
  "A mode for showing outputs from gnu global."
  (setq-local compilation-error-regexp-alist
              (list ggtags-global-output-format))
  (setq-local compilation-auto-jump-to-first-error
              ggtags-auto-jump-to-first-match)
  (setq-local compilation-scroll-output 'first-error)
  (setq-local compilation-disable-input t)
  (setq-local compilation-always-kill t)
  (setq-local compilation-error-face 'compilation-info)
  (setq-local compilation-exit-message-function
              'ggtags-global-exit-message-function)
  (setq-local truncate-lines t)
  (jit-lock-register #'ggtags-abbreviate-files)
  (add-hook 'compilation-finish-functions 'ggtags-handle-single-match nil t)
  (define-key ggtags-global-mode-map "o" 'visible-mode))

(defvar ggtags-navigation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-n" 'next-error)
    (define-key map "\M-p" 'previous-error)
    (define-key map "\M-}" 'ggtags-navigation-next-file)
    (define-key map "\M-{" 'ggtags-navigation-previous-file)
    (define-key map "\M-o" 'ggtags-navigation-visible-mode)
    (define-key map "\r" 'ggtags-navigation-mode-done)
    ;; Intercept M-. and M-* keys
    (define-key map [remap pop-tag-mark] 'ggtags-navigation-mode-abort)
    (define-key map [remap ggtags-find-tag] 'undefined)
    map))

(defun ggtags-move-to-tag (&optional name)
  "Move to NAME tag in current line."
  (let ((orig (point))
        (tag (or name ggtags-current-tag-name)))
    (beginning-of-line)
    (if (and tag (re-search-forward
                  (concat "\\_<" (regexp-quote tag) "\\_>")
                  (line-end-position)
                  t))
        (goto-char (match-beginning 0))
      (goto-char orig))))

(defun ggtags-navigation-mode-cleanup (&optional buf time)
  (let ((buf (or buf compilation-last-buffer)))
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (when (get-buffer-process (current-buffer))
             (kill-compilation))
           (when (and (derived-mode-p 'ggtags-global-mode)
                      (get-buffer-window))
             (quit-window nil (get-buffer-window)))
           (and time (run-with-idle-timer time nil 'kill-buffer buf))))))

(defun ggtags-navigation-mode-done ()
  (interactive)
  (ggtags-navigation-mode -1)
  (ggtags-navigation-mode-cleanup))

(defun ggtags-navigation-mode-abort ()
  (interactive)
  (pop-tag-mark)
  (ggtags-navigation-mode -1)
  (ggtags-navigation-mode-cleanup nil 0))

(defun ggtags-navigation-next-file (n)
  (interactive "p")
  (ggtags-ensure-global-buffer
    (compilation-next-file n)
    (compile-goto-error)))

(defun ggtags-navigation-previous-file (n)
  (interactive "p")
  (ggtags-navigation-next-file (- n)))

(defun ggtags-navigation-visible-mode (&optional arg)
  (interactive (list (or current-prefix-arg 'toggle)))
  (ggtags-ensure-global-buffer
    (visible-mode arg)))

(define-minor-mode ggtags-navigation-mode nil
  :lighter (" GG[" (:propertize "n" face error) "]")
  :global t
  (if ggtags-navigation-mode
      (progn
        (add-hook 'next-error-hook 'ggtags-move-to-tag)
        (add-hook 'minibuffer-setup-hook 'ggtags-minibuffer-setup-function))
    (remove-hook 'next-error-hook 'ggtags-move-to-tag)
    (remove-hook 'minibuffer-setup-hook 'ggtags-minibuffer-setup-function)))

(defun ggtags-minibuffer-setup-function ()
  ;; Disable ggtags-navigation-mode in minibuffer.
  (setq-local ggtags-navigation-mode nil))

(defun ggtags-kill-file-buffers (&optional interactive)
  "Kill all buffers visiting files in the root directory."
  (interactive "p")
  (ggtags-check-root-directory)
  (let ((root (ggtags-root-directory))
        (count 0)
        (some (lambda (pred list)
                (loop for x in list when (funcall pred x) return it))))
    (dolist (buf (buffer-list))
      (let ((file (and (buffer-live-p buf)
                       (not (eq buf (current-buffer)))
                       (buffer-file-name buf))))
        (when (and file (funcall some (apply-partially #'file-in-directory-p
                                                       (file-truename file))
                                 (cons root (ggtags-get-libpath))))
          (and (kill-buffer buf)
               (incf count)))))
    (and interactive
         (message "%d %s killed" count (if (= count 1) "buffer" "buffers")))))

(defun ggtags-after-save-function ()
  (let ((root (with-demoted-errors (ggtags-root-directory))))
    (when root
      (ggtags-cache-mark-dirty root t)
      ;; When oversize update on a per-save basis.
      (when (and buffer-file-name (ggtags-oversize-p))
        (with-demoted-errors
          (call-process "global" nil 0 nil
                        "--single-update"
                        (file-truename buffer-file-name)))))))

(defvar ggtags-tag-overlay nil)
(defvar ggtags-highlight-tag-timer nil)

(defvar ggtags-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-." 'ggtags-find-tag)
    (define-key map "\M-," 'ggtags-find-tag-resume)
    (define-key map "\C-c\M-k" 'ggtags-kill-file-buffers)
    map))

;;;###autoload
(define-minor-mode ggtags-mode nil
  :lighter (:eval (if ggtags-navigation-mode "" " GG"))
  (if ggtags-mode
      (progn
        (add-hook 'after-save-hook 'ggtags-after-save-function nil t)
        (or (executable-find "global")
            (message "Failed to find GNU Global")))
    (remove-hook 'after-save-hook 'ggtags-after-save-function t)
    (and (overlayp ggtags-tag-overlay)
         (delete-overlay ggtags-tag-overlay))
    (setq ggtags-tag-overlay nil)))

(defun ggtags-highlight-tag-at-point ()
  (when ggtags-mode
    (unless (overlayp ggtags-tag-overlay)
      (setq ggtags-tag-overlay (make-overlay (point) (point)))
      (overlay-put ggtags-tag-overlay 'ggtags t))
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (valid-tag (when bounds
                        (member (buffer-substring (car bounds) (cdr bounds))
                                (ggtags-tag-names (ggtags-oversize-p)))))
           (o ggtags-tag-overlay)
           (done-p (lambda ()
                     (and (memq o (overlays-at (car bounds)))
                          (= (overlay-start o) (car bounds))
                          (= (overlay-end o) (cdr bounds))
                          (or (and valid-tag (overlay-get o 'face))
                              (and (not valid-tag) (not (overlay-get o 'face))))))))
      (cond
       ((not bounds)
        (overlay-put ggtags-tag-overlay 'face nil)
        (move-overlay ggtags-tag-overlay (point) (point) (current-buffer)))
       ((not (funcall done-p))
        (move-overlay o (car bounds) (cdr bounds) (current-buffer))
        (overlay-put o 'face (and valid-tag 'ggtags-highlight)))))))

;;; imenu

(defun ggtags-goto-imenu-index (name line &rest _args)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))
    (ggtags-move-to-tag name)))

;;;###autoload
(defun ggtags-build-imenu-index ()
  "A function suitable for `imenu-create-index-function'."
  (when buffer-file-name
    (let ((file (file-truename buffer-file-name)))
      (with-temp-buffer
        (when (with-demoted-errors
                (zerop (call-process "global" nil t nil "-f" file)))
          (goto-char (point-min))
          (loop while (re-search-forward
                       "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)" nil t)
                collect (list (match-string 1)
                              (string-to-number (match-string 2))
                              'ggtags-goto-imenu-index)))))))

;;; hippie-expand

;;;###autoload
(defun try-complete-ggtags-tag (old)
  "A function suitable for `hippie-expand-try-functions-list'."
  (with-no-warnings                     ; to avoid loading hippie-exp
    (unless old
      (he-init-string (if (looking-back "\\_<.*" (line-beginning-position))
                          (match-beginning 0)
                        (point))
                      (point))
      (setq he-expand-list
            (and (not (equal he-search-string ""))
                 (with-demoted-errors (ggtags-root-directory))
                 (sort (all-completions he-search-string
                                        (ggtags-tag-names))
                       'string-lessp))))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          nil)
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

;;; Finish up

(when ggtags-highlight-tag-timer
  (cancel-timer ggtags-highlight-tag-timer))

(setq ggtags-highlight-tag-timer
      (run-with-idle-timer 0.2 t 'ggtags-highlight-tag-at-point))

;; Higher priority for `ggtags-navigation-mode' to avoid being
;; hijacked by modes such as `view-mode'.
(defvar ggtags-mode-map-alist
  `((ggtags-navigation-mode . ,ggtags-navigation-mode-map)))

(add-to-list 'emulation-mode-map-alists 'ggtags-mode-map-alist)

(provide 'ggtags)
;;; ggtags.el ends here
