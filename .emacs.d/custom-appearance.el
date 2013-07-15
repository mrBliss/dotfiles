;;; custom-appearance.el --- Bring Emacs to the 21st century
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: color-themes, appearance, looks

;;##############################################################################

;; Load my themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Default font
(set-face-attribute 'default nil :family
                    (case system-type
                      ('gnu/linux "Quadraat Sans Mono")
                      ('darwin "Inconsolata")))

;; Fallback font for unicode
(set-fontset-font "fontset-default" 'unicode "Liberation Mono")

;; Powerline
(require 'powerline)

(defpowerline powerline-file-format
  (case (coding-system-eol-type buffer-file-coding-system)
    (0 "")
    (1 "^R")
    (2 "^M")))

(defun powerline-custom-theme ()
  "Setup my custom mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (face1 (if active 'powerline-active1
                                   'powerline-inactive1))
                          (face2 (if active 'powerline-active2
                                   'powerline-inactive2))
                          (lhs (list
                                (powerline-raw "%*" nil 'l)
                                (powerline-file-format nil 'l)
                                (powerline-buffer-size nil 'l)
                                (powerline-buffer-id nil 'l)

                                (powerline-raw " ")
                                (powerline-arrow-right nil face1)

                                (when (boundp 'erc-modified-channels-object)
                                  (powerline-raw erc-modified-channels-object
                                                 face1 'l))

                                (powerline-major-mode face1 'l)
                                (powerline-process face1)
                                (powerline-minor-modes face1 'l)
                                (powerline-narrow face1 'l)

                                (powerline-raw " " face1)
                                (powerline-arrow-right face1 face2)

                                (powerline-vc face2)))
                          (rhs (list
                                (powerline-raw global-mode-string face2 'r)

                                (powerline-arrow-left face2 face1)

                                (powerline-raw "%4l" face1 'r)
                                (powerline-raw ":" face1)
                                (powerline-raw "%3c" face1 'r)

                                (powerline-arrow-left face1 nil)
                                (powerline-raw " ")

                                (powerline-raw "%6p" nil 'r)

                                (powerline-hud face2 face1))))
                     (concat
                      (powerline-render lhs)
                      (powerline-fill face2 (powerline-width rhs))
                      (powerline-render rhs)))))))
(powerline-custom-theme)


(defvar mode-line-cleaner-alist
  '((yas/minor-mode . "")
    (highlight-parentheses-mode . "")
    (hi-lock-mode . "")
    (wrap-region-mode . "")
    (outline-minor-mode . "")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (projectile-mode . " Prj")
    (haskell-indentation-mode . "")
    (slime-mode . " Sli")
    (flyspell-mode . " FlyS")
    (num3-mode . "")
    (compilation-minor-mode . " Cmpl")
    ;; Major modes
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (lisp-interaction-mode . "EL")
    (haskell-mode . "Hs")
    (literate-haskell-mode . "LHs")
    (inferior-haskell-mode . "iHs")
    (clojure-mode . "Clj")
    (text-mode . "Txt")
    (markdown-mode . "Md")
    (sh-mode . "Sh")
    (js2-mode . "JS")
    )
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")


(defun clean-mode-line ()
  "Replace modeline lighters with new ones."
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let ((mode (car cleaner))
                 (mode-str (cdr cleaner)))
             (when (memq mode minor-mode-list)
               (setcar (cdr (assq mode minor-mode-alist)) mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; alias the new `flymake-report-status-slim' to
;;; `flymake-report-status'
(defalias 'flymake-report-status 'flymake-report-status-slim)
(defun flymake-report-status-slim (e-w &optional status)
  "Show \"slim\" flymake status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " FlyM"))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))

;; Pretty lambdas
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; Show column numbers
(column-number-mode 1)

;; Show tooltips in the minibuffer
(setq tooltip-use-echo-area t)

;; Set frame title
(setq frame-title-format
      '(("" invocation-name " | " mode-name " | " buffer-name "%b")))

;; Indicate empty lines by default (in the fringe)
(setq default-indicate-empty-lines t)

;; Enable winner-mode (restore the previous window layout)
(winner-mode 1)

;; Enable window numbering mode (select a window with M-1,2,3..)
(require 'window-numbering)
(window-numbering-mode t)

;; Moves the mouse pointer to the corner of the screen when typing
(mouse-avoidance-mode 'jump)

;; Close pop-up windows with C-g
(require 'popwin)

;; Escreen, screen for Emacs
(require 'escreen)
(escreen-install)
(setq escreen-prefix-char (kbd "C-z"))
(global-set-key escreen-prefix-char 'escreen-prefix)

;; C-z is now the prefix char for escreen, so to use its previous
;; function, suspending the frame, use C-z C-z.
(define-key escreen-map "\C-z" 'suspend-frame)

;; Modified version of Vinh Nguyen's
;; escreen-get-active-screen-numbers-with-emphasis.
(defun escreen-display-active-screen ()
  "Display the screens in the minibuffer.
Emphasize the active screen by displaying it in another face and
putting it in parentheses."
  (interactive)
  (let ((current escreen-current-screen-number)
        (screens (copy-list (escreen-configuration-screen-numbers)))
        (str ""))
    (dolist (s (sort screens '<))
      (setq str (concat str " "
                        (if (eq s current)
                            (propertize (format "(%d)" s)
                                        'face 'font-lock-builtin-face)
                          (number-to-string s)))))
    (message "Screen:%s" str)))

(add-hook 'escreen-goto-screen-hook 'escreen-display-active-screen)

;; Update the escreen number in the mode line
(add-hook 'escreen-goto-screen-hook 'escreen-enable-number-mode-if-more-than-one-screen)

;; Pretty display of `^L'
(require 'pp-c-l)
(pretty-control-l-mode 1)


;; Proper colors in tmux
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  (unless (fboundp 'xterm-register-default-colors)
    (load "term/xterm"))
  ;; Use the xterm color initialization code.
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))


(provide 'custom-appearance)
