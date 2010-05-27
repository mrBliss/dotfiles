;; Custom Themes
(defun color-theme-twilighter ()
  "Color theme by Marcus Crafter, based off the TextMate Twilight theme, created 2008-04-18; modded bny Kieran Healy"
  (interactive)
  (color-theme-install
   '(color-theme-twilight
     ((background-color . "#1E1E1E")
      (background-mode . dark)
      (border-color . "#1E1E1E")
      (cursor-color . "#A7A7A7")
      (foreground-color . "#F8F8F8")
      (mouse-color . "sienna1"))
     (default ((t (:background "#1E1E1E" :foreground "#CACACA"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#1E1E1E" :foreground "#CACACA"))))
     (font-lock-builtin-face ((t (:foreground "#CACACA"))))
     (font-lock-comment-face ((t (:italic t :foreground "#5F5A60"))))
     (font-lock-constant-face ((t (:foreground "#CF6A4C"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "#9B703F"))))
     (font-lock-keyword-face ((t (:foreground "#CDA869"))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

     (font-lock-string-face ((t (:foreground "#8F9D6A"))))
     (font-lock-type-face ((t (:foreground "#9B703F"))))
     (font-lock-variable-name-face ((t (:foreground "#7587A6"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (gui-element ((t (:background "#D4D0C8" :foreground "#1E1E1E"))))
     (region ((t (:background "#27292A"))))
     (mode-line ((t (:background "grey75" :foreground "#1E1E1E"))))
     (highlight ((t (:background "#222222"))))
     (highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "#1E1E1E"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (zmacs-region ((t (:background "snow" :foreground "ble")))))))

(defun color-theme-tm ()
  "Color theme by dngpng, created 2007-09-11."
  (interactive)
  (color-theme-install
   '(color-theme-tm
     ((background-color . "#111")
      (background-mode . dark)
      (border-color . "#111")
      (cursor-color . "yellow")
      (foreground-color . "#ddd")
      (mouse-color . "sienna1"))
     (default ((t (:background "#111" :foreground "#ddd"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t :slant italic))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#111" :foreground "#ddd"))))
     (font-lock-builtin-face ((t (:foreground "#dd7b3b"))))
     (font-lock-comment-face ((t (:foreground "#666" ))))
     (font-lock-constant-face ((t (:foreground "#99cf50"))))
     (font-lock-doc-string-face ((t (:foreground "#9b859d"))))
     (font-lock-function-name-face ((t (:foreground "#e9c062" :bold t))))
     (font-lock-keyword-face ((t (:foreground "#cf6a4c" :bold t))))
     (font-lock-preprocessor-face ((t (:foreground "#aeaeae"))))
     (font-lock-reference-face ((t (:foreground "8b98ab"))))
     (font-lock-string-face ((t (:foreground "#65b042"))))
     (font-lock-type-face ((t (:foreground "#c5af75"))))
     (font-lock-variable-name-face ((t (:foreground "#3387cc"))))
     (font-lock-warning-face ((t (:bold t :background "#420e09" :foreground "#eeeeee"))))
     (erc-current-nick-face ((t (:foreground "#aeaeae"))))
     (erc-default-face ((t (:foreground "#ddd"))))
     (erc-keyword-face ((t (:foreground "#cf6a4c"))))
     (erc-notice-face ((t (:foreground "#666"))))
     (erc-timestamp-face ((t (:foreground "#65b042"))))
     (erc-underline-face ((t (:foreground "c5af75"))))
     (nxml-attribute-local-name-face ((t (:foreground "#3387cc"))))
     (nxml-attribute-colon-face ((t (:foreground "#e28964"))))
     (nxml-attribute-prefix-face ((t (:foreground "#cf6a4c"))))
     (nxml-attribute-value-face ((t (:foreground "#65b042"))))
     (nxml-attribute-value-delimiter-face ((t (:foreground "#99cf50"))))
     (nxml-namespace-attribute-prefix-face ((t (:foreground "#9b859d"))))
     (nxml-comment-content-face ((t (:foreground "#666"))))
     (nxml-comment-delimiter-face ((t (:foreground "#333"))))
     (nxml-element-local-name-face ((t (:foreground "#e9c062"))))
     (nxml-markup-declaration-delimiter-face ((t (:foreground "#aeaeae"))))
     (nxml-namespace-attribute-xmlns-face ((t (:foreground "#8b98ab"))))
     (nxml-prolog-keyword-face ((t (:foreground "#c5af75"))))
     (nxml-prolog-literal-content-face ((t (:foreground "#dad085"))))
     (nxml-tag-delimiter-face ((t (:foreground "#cda869"))))
     (nxml-tag-slash-face ((t (:foreground "#cda869"))))
     (nxml-text-face ((t (:foreground "#ddd"))))
     (gui-element ((t (:background "#0e2231" :foreground "black"))))
     (highlight ((t (:bold t :slant italic))))
     (highline-face ((t (:background "#4a410d"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (mmm-default-submode-face ((t (:background "#111"))))
     (mode-line ((t (:background "#e6e5e4" :foreground "black"))))
     (primary-selection ((t (:background "#222"))))
     (region ((t (:background "#4a410d"))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (underline ((nil (:underline nil)))))))

(defun color-theme-blackboard ()
  "Color theme by JD Huntington, based off the TextMate Blackboard theme, created 2008-11-27"
  (interactive)
  (color-theme-install
   '(color-theme-blackboard
     ((background-color . "#0C1021")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#A7A7A7")
      (foreground-color . "#F8F8F8")
      (mouse-color . "sienna1"))
     (default ((t (:background "#0C1021" :foreground "#F8F8F8"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))
     (font-lock-builtin-face ((t (:foreground "#F8F8F8"))))
     (font-lock-comment-face ((t (:italic t :foreground "#AEAEAE"))))
     (font-lock-constant-face ((t (:foreground "#D8FA3C"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "#FF6400"))))
     (font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

     (font-lock-string-face ((t (:foreground "#61CE3C"))))
     (font-lock-type-face ((t (:foreground "#8DA6CE"))))
     (font-lock-variable-name-face ((t (:foreground "#FF6400"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (region ((t (:background "#253B76"))))
     (mode-line ((t (:background "grey75" :foreground "black"))))
     (highlight ((t (:background "#222222"))))
     (highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (zmacs-region ((t (:background "snow" :foreground "ble")))))))

(defun color-theme-gentooish ()
  "Mostly green and purple color theme"
  (interactive)
  (color-theme-install
   '(color-theme-gentooish
     ((foreground-color . "#c0c0c0")
      (background-color . "#171717")
      (border-color . "black")
      (cursor-color . "green")
      (background-mode . dark))
     (bold ((t (:foreground "white" :weight normal))))
     (font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "#c476f1"))))
     (font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "grey30" :slant italic))))
     (font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#4cbbd1"))))
     (font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "#9a383a"))))
     (font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:background "#0f291a" :foreground "#5dff9e"))))
     (hi-blue ((((background dark)) (:background "grey20"))))
     (ido-first-match ((t (:background "#361d45" :foreground "#cf7dff" :weight bold))))
     (ido-only-match ((((class color)) (:background "#361d45" :foreground "#cf7dff" :weight bold))))
     (ido-subdir ((((min-colors 88) (class color)) (:foreground "#7dcfff"))))
     (linum ((t (:inherit shadow :background "grey12"))))
     (minibuffer-prompt ((((background dark)) (:foreground "#863335"))))
     (mode-line ((((class color) (min-colors 88)) (:background "#333333" :foreground "#ffffff" :box (:line-width -1 :color "#333333")))))
     (mode-line-highlight ((((class color) (min-colors 88)) nil)))
     (mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background dark)) (:foreground "#8b8b8b" :weight light))))
     (show-paren-match ((((class color) (background dark)) (:background "#005500"))))
     (tool-bar ((default (:foreground "black")) (((type x w32 ns) (class color)) (:background "grey75")))))))


(defun color-theme-bespin ()
  (interactive)
  (color-theme-install
   '(color-theme-bespin
     ((background-color . "#302620")
      (foreground-color . "#BAAE9E")
      (cursor-color . "#A7A7A7"))
     (default ((t (nil))))
     (modeline
      ((t (:background "#4c4a41" :foreground "#BAAE9E"
		       :box (:line-width 1 :style released-button)))))
     (mode-line-inactive ((t (:background "#1e1915"))))
     (minibuffer-prompt ((t (:foreground "#5EA6EA"))))
     (font-lock-builtin-face ((t (:foreground "#5EA6EA"))))
     (font-lock-comment-face ((t (:italic t :foreground "#666666"))))
     (font-lock-constant-face ((t (:foreground "#DDF2A4"))))
     (font-lock-doc-string-face ((t (:foreground "#5EA6EA"))))
     (font-lock-string-face ((t (:foreground "#00920A"))))
     (font-lock-function-name-face ((t (:foreground "#A8937A" :italic t))))
     (font-lock-keyword-face ((t (:foreground "#0074e8"))))
     (font-lock-type-face ((t (:underline t :foreground "#89BDFF"))))
     (font-lock-variable-name-face ((t (:foreground "#7587A6" :bold t))))
     (font-lock-warning-face ((t (:bold t :foreground "#F9EE98"))))
     (comint-highlight-prompt ((t (:foreground "#5EA6EA"))))
     (highlight-80+ ((t (:background "#F9EE98"))))
     (hl-line ((t (:background "#2A2A2A"))))
     (region ((t (:background "#f8fab7"))))
     (ido-subdir ((t (:foreground "#F1266F")))))))

(provide 'custom-themes)