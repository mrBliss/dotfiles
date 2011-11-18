;;; custom-appearance.el --- Bring Emacs to the 21st century
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: color-themes, appearance, looks

;;##############################################################################
;; Miscellaneous

(require 'color-theme)
(require 'custom-themes)

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
             ('darwin "QuadraatSMono-Regular-12")
             ('cygwin "QuadraatSMono-Regular-9")))
          (tool-bar-mode -1)
          (set-scroll-bar-mode nil)
          (color-theme-dark-violet))
      (color-theme-ir-black))))
(add-hook 'after-make-frame-functions 'appearance)

;; Not global, because terminal and graphical windows have different
;; themes.
(setq color-theme-is-global nil)

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
(setq display-buffer-function 'popwin:display-buffer)

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



(provide 'custom-appearance)