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

;; Include the window-number
(defpowerline powerline-window-number
  (window-numbering-get-number-string))

(defun powerline-my-theme ()
  "Setup my mode-line."
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
                                (powerline-window-number nil 'l)
                                (powerline-raw "%*" nil 'l)
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

(powerline-my-theme)


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
;; Except when using ECB on the active frame
(defun popwin:display-buffer-ecb-compat (buffer &optional not-this-window)
  "Run `popwin:display-buffer' unless the active frame is using
ECB.  In that case, fallback to the original with
`popwin:original-display-buffer'."
  (interactive "BDisplay buffer:\n")
  (if (selected-frame-has-ecb)
      (popwin:original-display-buffer buffer not-this-window)
    (popwin:display-buffer buffer not-this-window)))
(setq display-buffer-function 'popwin:display-buffer-ecb-compat)


;; Escreen, screen for Emacs
(require 'escreen)
;; ECB support
(ecb-winman-escreen-enable-support)
(setq ecb-winman-escreen-number 9)
(escreen-install)
(setq escreen-prefix-char (kbd "C-z"))
(global-set-key escreen-prefix-char 'escreen-prefix)

(defun escreen-create-ecb-screen ()
  "Create a new screen for ECB and switch to it.
A new screen with `ecb-winman-escreen-number' as number will be
created, unless it already exists.  ECB will be enabled for that
screen."
  (interactive)
  (if (escreen-screen-defined ecb-winman-escreen-number)
      (message "ECB (%d) screen already created"
               ecb-winman-escreen-number)
    (flet ((escreen-first-unused-screen-number () ecb-winman-escreen-number))
      (call-interactively 'escreen-create-screen))))

;; C-z is now the prefix char for escreen, so to use its previous
;; function, suspending the frame, use C-z C-z.
(define-key escreen-map "\C-z" 'suspend-frame)
;; Create a new screen with ECB enabled with C-z e.
(define-key escreen-map (kbd "e") 'escreen-create-ecb-screen)

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


;; Show scrollbar like position indicator in mode line
;; (require 'sml-modeline)
;; (sml-modeline-mode 1)
;; (setq sml-modeline-len 8
;;       sml-modeline-borders nil)


;; Proper colors in tmux
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  (unless (fboundp 'xterm-register-default-colors)
    (load "term/xterm"))
  ;; Use the xterm color initialization code.
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))


(provide 'custom-appearance)