;;; custom-appearance.el --- Bring Emacs to the 21st century
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: color-themes, appearance, looks

;;##############################################################################
;; Miscellaneous

(require 'color-theme)
(require 'custom-themes)

;;; Set font and theme depending on frame type. Also disable the tool
;;; bar and the scroll bar
(defun appearance (f)
  "Applies the color-theme, ir-black in a terminal or bespin in a
   GUI. Disables the scroll and tool bar. Also sets the font."
  (with-selected-frame f
    (if (window-system f)
        (progn
          (case system-type
            ('windows-nt (set-default-font "Envy Code R-8"))
            ('gnu/linux (if (string= system-name "gideon")
                            (set-default-font "QuadraatSMono-Regular-9")
                          (set-default-font "Inconsolata-9")))
            ('darwin (set-default-font "Inconsolata-12")))
          (tool-bar-mode -1)
          (set-scroll-bar-mode nil)
          (color-theme-bespin))
      (color-theme-ir-black))))
(add-hook 'after-make-frame-functions 'appearance)

;; Not global, because terminal and graphical windows have different
;; themes.
(setq color-theme-is-global nil)

;; Change flyspell faces
(eval-after-load "flyspell"
  ;; The faces in my color-themes are ignored :-(
  '(if window-system
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
(add-hook 'coding-hook 'pretty-lambdas)

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


(provide 'custom-appearance)