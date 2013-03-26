;;; custom-appearance.el --- Bring Emacs to the 21st century
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: color-themes, appearance, looks

;;##############################################################################
;; Miscellaneous

(require 'color-theme)
(require 'custom-themes)

;; The dark and light color-themes
(setq dark-color-theme 'color-theme-dark-violet)
(setq light-color-theme 'color-theme-solarized-light)
;; The color-theme for the next frame
(setq next-color-theme dark-color-theme)

(defun make-frame-light-or-dark (arg)
  "Make a new frame like `make-frame' with a dark color-theme or
a light color-theme when passed a prefix argument."
  (interactive "P")
  (setq next-color-theme (if arg light-color-theme dark-color-theme))
  (make-frame))


;; Set font and theme depending on frame type. Also disable the tool
;; bar and the scroll bar
(defun appearance (f)
  "Applies the color-theme, ir-black in a terminal or dark-violet in a
   GUI. Disables the scroll and tool bar. Also sets the font."
  (with-selected-frame f
    (if (display-graphic-p f)
        (progn
          (set-frame-font
           (case system-type
             ('windows-nt "Envy Code R-8")
             ('gnu/linux "QuadraatSMono-Regular-9")
             ('darwin "Inconsolata-14")
             ('cygwin "m-9")))
          (tool-bar-mode -1)
          (set-scroll-bar-mode nil)
          (funcall next-color-theme))
      (color-theme-ir-black))))
(add-hook 'after-make-frame-functions 'appearance)

;; Not global, because terminal and graphical windows have different
;; themes.
(setq color-theme-is-global nil)


(require 'powerline)
(powerline-default)


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


;; Change flyspell faces
(eval-after-load "flyspell"
  ;; The faces in my color-themes are ignored :-(
  '(if (display-graphic-p)
       (progn (set-face-foreground 'flyspell-incorrect "#FA2573"
                                   (selected-frame))
              (set-face-attribute 'flyspell-incorrect (selected-frame)
                                  :underline t :bold t)
              (set-face-foreground 'flyspell-duplicate "#FF8844"
                                   (selected-frame))
              (set-face-attribute 'flyspell-duplicate (selected-frame)
                                  :underline t :bold t))
     (progn (set-face-foreground 'flyspell-incorrect "#FFA560"
                                 (selected-frame))
            (set-face-attribute 'flyspell-incorrect (selected-frame)
                                :underline nil :bold nil)
            (set-face-foreground 'flyspell-duplicate "#F1266F"
                                 (selected-frame))
            (set-face-attribute 'flyspell-duplicate (selected-frame)
                                :underline nil :bold nil))))

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
