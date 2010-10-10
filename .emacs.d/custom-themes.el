;; Custom Themes
(defun color-theme-twilighter ()
  "Color theme by Marcus Crafter, based off the TextMate Twilight
theme, created 2008-04-18; modded bny Kieran Healy"
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
     (font-lock-warning-face
      ((t (:bold t :background "#420e09" :foreground "#eeeeee"))))
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
  "Color theme by JD Huntington, based off the TextMate
Blackboard theme, created 2008-11-27"
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
  "Mostly green and purple color theme by Brian Carper"
  (interactive)
  (color-theme-install
   '(color-theme-gentooish
     ((foreground-color . "#c0c0c0")
      (background-color . "#171717")
      (border-color . "black")
      (cursor-color . "green")
      (background-mode . dark))
     (bold ((t (:foreground "white" :weight normal))))
     (font-lock-builtin-face
      ((((class color) (min-colors 88) (background dark))
        (:foreground "#c476f1"))))
     (font-lock-comment-face
      ((((class color) (min-colors 88) (background dark))
        (:foreground "grey30" :slant italic))))
     (font-lock-function-name-face
      ((((class color) (min-colors 88) (background dark))
        (:foreground "#4cbbd1"))))
     (font-lock-keyword-face
      ((((class color) (min-colors 88) (background dark))
        (:foreground "#9a383a"))))
     (font-lock-string-face
      ((((class color) (min-colors 88) (background dark))
        (:background "#0f291a" :foreground "#5dff9e"))))
     (hi-blue ((((background dark)) (:background "grey20"))))
     (ido-first-match
      ((t (:background "#361d45" :foreground "#cf7dff" :weight bold))))
     (ido-only-match
      ((((class color)) (:background "#361d45" :foreground "#cf7dff"
                                     :weight bold))))
     (ido-subdir ((((min-colors 88) (class color)) (:foreground "#7dcfff"))))
     (linum ((t (:inherit shadow :background "grey12"))))
     (minibuffer-prompt ((((background dark)) (:foreground "#863335"))))
     (mode-line
      ((((class color) (min-colors 88))
        (:background "#333333" :foreground "#ffffff"
                     :box (:line-width -1 :color "#333333")))))
     (mode-line-highlight ((((class color) (min-colors 88)) nil)))
     (mode-line-inactive
      ((default (:inherit mode-line))
       (((class color) (min-colors 88) (background dark))
        (:foreground "#8b8b8b" :weight light))))
     (show-paren-match
      ((((class color) (background dark)) (:background "#005500"))))
     (tool-bar
      ((default (:foreground "black")) (((type x w32 ns) (class color))
                                        (:background "grey75")))))))

(defun color-theme-molokai ()
  "Color theme based on the Molokai color scheme for vim. By alloy-d"
  (interactive)
  (color-theme-install
   '(color-theme-molokai
     ((foreground-color . "#F8F8F2")
      (background-color . "#1B1D1E")
      (cursor-color . "#F8F8F0")
      (background-mode . dark))
     (default ((t (:foreground "#F8F8F2" :background "#1B1D1E"))))
     (bold ((t (:weight bold))))
     (bold-italic ((t (:weight bold :slant italic))))
     (custom-face-tag ((t (:foreground "#66D9EF" :weight bold))))
     (custom-state ((t (:foreground "#A6E22E"))))
     (italic ((t (:slant italic))))
     (region ((t (:background "#403D3D"))))
     (underline ((t (:underline t))))
     (css-selector ((t (:foreground "#F92672"))))
     (css-property ((t (:foreground "#66D9EF"))))
     (diff-added ((t (:foreground "#A6E22E" :weight bold))))
     (diff-context ((t (:foreground "#F8F8F2"))))
     (diff-file-header ((t (:foreground "#66D9EF" :background nil))))
     (diff-indicator-added ((t (:foreground "#A6E22E"))))
     (diff-indicator-removed ((t (:foreground "#F92672"))))
     (diff-header ((t (:foreground "#F8F8F2" :background "#232526"))))
     (diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
     (diff-removed ((t (:foreground "#F92672" :weight bold))))
     (escape-glyph ((t (:foreground "#E6DB74"))))
     (minibuffer-prompt ((t (:foreground "#66D9EF"))))
     (mode-line
      ((t (:foreground "#F8F8F2" :background "#000000"
                       :box (:line-width 1 :color "#000000"
                                         :style released-button)))))
     (mode-line-buffer-id
      ((t (:foreground nil :background "#000000" :weight semi-bold))))
     (mode-line-inactive
      ((t (:foreground "#BCBCBC" :background "#000000"
                       :box (:line-width 1 :color "#232526")))))
     (mode-line-mousable ((t (:foreground "#BCBCBC" :background "#000000"))))
     (mode-line-mousable-minor-mode
      ((t (:foreground "#BCBCBC" :background "#000000"))))
     (font-lock-builtin-face ((t (:foreground "#A6E22E"))))
     (font-lock-comment-face ((t (:foreground "#465457" :slant italic))))
     (font-lock-comment-delimiter-face
      ((t (:foreground "#465457" :slant italic))))
     (font-lock-constant-face ((t (:foreground "#AE81FF"))))
     (font-lock-doc-face ((t (:foreground "#E6DB74" :slant italic))))
     (font-lock-function-name-face ((t (:foreground "#F92672" :slant italic))))
     (font-lock-keyword-face ((t (:foreground "#66D9EF"))))
     (font-lock-negation-char-face ((t (:weight bold))))
     (font-lock-preprocessor-face ((t (:foreground "#A6E22E"))))
     (font-lock-regexp-grouping-backslash ((t (:weight bold))))
     (font-lock-regexp-grouping-construct ((t (:weight bold))))
     (font-lock-string-face ((t (:foreground "#E6DB74"))))
     (font-lock-type-face ((t (:foreground "#66D9EF"))))
     (font-lock-variable-name-face ((t (:foreground "#F92672"))))
     (font-lock-warning-face ((t (:foreground "#FFFFFF"
                                              :background "#333333"))))
     (fringe ((t (:background "#232526"))))
     (highlight ((t (:foreground "#000000" :background "#C4BE89"))))
     (hl-line ((t (:background "#293739"))))
     (icompletep-choices ((t (:foreground "#F92672"))))
     (icompletep-determined ((t (:foreground "#A6E22E"))))
     (icompletep-keys ((t (:foreground "#F92672"))))
     (icompletep-nb-candidates ((t (:foreground "#AE81FF"))))
     (isearch ((t (:foreground "#C4BE89" :background "#000000"))))
     (isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))
     (lazy-highlight ((t (:foreground "#465457" :background "#000000"))))
     (markdown-italic-face ((t (:slant italic))))
     (markdown-bold-face ((t (:weight bold))))
     (markdown-header-face ((t (:weight normal))))
     (markdown-header-face-1 ((t (:foreground "#66D9EF"))))
     (markdown-header-face-2 ((t (:foreground "#F92672"))))
     (markdown-header-face-3 ((t (:foreground "#A6E22E"))))
     (markdown-header-face-4 ((t (:foreground "#AE81FF"))))
     (markdown-header-face-5 ((t (:foreground "#E6DB74"))))
     (markdown-header-face-6 ((t (:foreground "#66D9EF"))))
     (markdown-inline-code-face ((t (:foreground "#66D9EF"))))
     (markdown-list-face ((t (:foreground "#A6E22E"))))
     (markdown-blockquote-face ((t (:slant italic))))
     (markdown-pre-face ((t (:foreground "#AE81FF"))))
     (markdown-link-face ((t (:foreground "#66D9EF"))))
     (markdown-reference-face ((t (:foreground "#66D9EF"))))
     (markdown-url-face ((t (:foreground "#E6DB74"))))
     (markdown-link-title-face ((t (:foreground "#F92672"))))
     (markdown-comment-face ((t (:foreground "#465457"))))
     (markdown-math-face ((t (:foreground "#AE81FF" :slant italic))))
     (mumamo-background-chunk-major ((t (:background "#272822"))))
     (mumamo-background-chunk-submode ((t (:background "#1B1D1E"))))
     (outline-1 ((t (:foreground "#66D9EF"))))
     (outline-2 ((t (:foreground "#F92672"))))
     (outline-3 ((t (:foreground "#A6E22E"))))
     (outline-4 ((t (:foreground "#AE81FF"))))
     (outline-5 ((t (:foreground "#E6DB74"))))
     (outline-6 ((t (:foreground "#66D9EF"))))
     (outline-7 ((t (:foreground "#F92672"))))
     (outline-8 ((t (:foreground "#A6E22E"))))
     (secondary-selection ((t (:background "#272822"))))
     (show-paren-match-face ((t (:foreground "#000000" :background "#FD971F"))))
     (show-paren-mismatch-face
      ((t (:foreground "#960050" :background "#1E0010"))))
     (widget-inactive-face ((t (:background "#ff0000"))))
     (woman-addition ((t (:foreground "#AE81FF"))))
     (woman-bold ((t (:foreground "#F92672"))))
     (woman-italic ((t (:foreground "#A6E22E"))))
     (woman-unknown ((t (:foreground "#66D9EF")))))))

(defun color-theme-wombat ()
  "Color theme 'wombat', taken from
http://dengmao.wordpress.com/2007/01/22/vim-color-scheme-wombat/#comment-1051,
created 2009-06-23."
  (interactive)
  (color-theme-install
   '(color-theme-wombat
     ((background-color . "#242424")
      (background-mode . dark)
      (border-color . "#888a85")
      (cursor-color . "#656565")
      (foreground-color . "#f6f3e8")
      (mouse-color . "#333333"))
     ((apropos-keybinding-face . underline)
      (apropos-label-face italic variable-pitch)
      (apropos-match-face . match)
      (apropos-property-face . bold-italic)
      (apropos-symbol-face . bold)
      (compilation-message-face . underline)
      (list-matching-lines-buffer-name-face . underline)
      (list-matching-lines-face . match)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default
       ((t (:stipple nil :background "#242424" :foreground "#f6f3e8"
                     :inverse-video nil :box nil :strike-through nil
                     :overline nil :underline nil :slant normal :weight normal
                     :height 98 :width normal :foundry "outline"))))
     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border ((t (:background "#888a85"))))
     (buffer-menu-buffer ((t (:bold t :weight bold))))
     (button ((t (:underline t))))
     (comint-highlight-input ((t (:italic t :bold t :slant italic
                                          :weight bold))))
     (comint-highlight-prompt ((t (:foreground "#8ae234"))))
     (compilation-column-number ((t (:foreground "#cae682"))))
     (compilation-error ((t (:bold t :weight bold :foreground "#cc0000"))))
     (compilation-info ((t (:bold t :foreground "Green1" :weight bold))))
     (compilation-line-number ((t (:foreground "#cae682"))))
     (compilation-warning ((t (:bold t :foreground "Orange" :weight bold))))
     (completions-common-part
      ((t (:foundry "outline" :width normal :weight normal :slant normal
                    :underline nil :overline nil :strike-through nil :box nil
                    :inverse-video nil :foreground "#f6f3e8"
                    :background "#242424" :stipple nil :height 98))))
     (completions-first-difference ((t (:bold t :weight bold))))
     (cursor ((t (:background "#656565"))))
     (dired-directory ((t (:foreground "#cae682"))))
     (dired-flagged ((t (:bold t :weight bold :foreground "#cc0000"))))
     (dired-header ((t (:foreground "#cae682"))))
     (dired-ignored ((t (:foreground "grey70"))))
     (dired-mark ((t (:foreground "#e5786d"))))
     (dired-marked ((t (:bold t :weight bold :foreground "#cc0000"))))
     (dired-perm-write
      ((t (:foundry "outline" :width normal :weight normal :slant normal
                    :underline nil :overline nil :strike-through nil :box nil
                    :inverse-video nil :foreground "#f6f3e8"
                    :background "#242424" :stipple nil :height 98))))
     (dired-symlink ((t (:foreground "#8ac6f2"))))
     (dired-warning ((t (:bold t :weight bold :foreground "#cc0000"))))
     (escape-glyph ((t (:foreground "cyan"))))
     (file-name-shadow ((t (:foreground "grey70"))))
     (font-lock-builtin-face ((t (:foreground "#8ac6f2"))))
     (font-lock-comment-delimiter-face
      ((t (:italic t :slant italic :foreground "#99968b"))))
     (font-lock-comment-face
      ((t (:italic t :foreground "#99968b" :slant italic))))
     (font-lock-constant-face ((t (:foreground "#e5786d"))))
     (font-lock-doc-face ((t (:italic t :foreground "#99968b" :slant italic))))
     (font-lock-function-name-face ((t (:foreground "#cae682"))))
     (font-lock-keyword-face ((t (:foreground "#8ac6f2"))))
     (font-lock-negation-char-face ((t (:foreground "#e7f6da"))))
     (font-lock-preprocessor-face ((t (:foreground "#e5786d"))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
     (font-lock-string-face
      ((t (:italic t :foreground "#95e454" :slant italic))))
     (font-lock-type-face ((t (:foreground "#cae682"))))
     (font-lock-variable-name-face ((t (:foreground "#cae682"))))
     (font-lock-warning-face ((t (:bold t :foreground "#cc0000" :weight bold))))
     (fringe ((t (:background "grey10"))))
     (header-line ((t (:background "grey20" :foreground "grey90" :box nil))))
     (help-argument-name ((t (:italic t :slant italic))))
     (highlight ((t (:background "darkolivegreen"))))
     (info-xref ((t (:foreground "#729fcf"))))
     (info-xref-visited ((t (:foreground "#ad7fa8"))))
     (isearch ((t (:background "#f57900" :foreground "#2e3436"))))
     (isearch-fail ((t (:background "red4"))))
     (iswitchb-current-match ((t (:foreground "#cae682"))))
     (iswitchb-invalid-regexp
      ((t (:bold t :weight bold :foreground "#cc0000"))))
     (iswitchb-single-match
      ((t (:italic t :slant italic :foreground "#99968b"))))
     (iswitchb-virtual-matches ((t (:foreground "#8ac6f2"))))
     (italic ((t (:italic t :slant italic))))
     (lazy-highlight ((t (:background "yellow" :foreground "black"))))
     (link ((t (:bold t :foreground "#8ac6f2" :underline t :weight bold))))
     (link-visited
      ((t (:bold t :weight bold :underline t :foreground "violet"))))
     (match ((t (:background "RoyalBlue3"))))
     (menu ((t (:foreground "systemmenu" :background "systemmenutext"))))
     (minibuffer-noticeable-prompt ((t (nil))))
     (minibuffer-prompt ((t (:foreground "#729fcf"))))
     (mode-line ((t (:background "#333333" :foreground "#eeeeec"))))
     (mode-line-buffer-id ((t (:bold t :weight bold))))
     (mode-line-emphasis ((t (:bold t :weight bold))))
     (mode-line-highlight
      ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
     (mode-line-inactive ((t (:background "#222222" :foreground "#cccddd"))))
     (mouse ((t (:background "#333333"))))
     (next-error ((t (:foreground "#f6f3e8" :background "#444444"))))
     (nobreak-space ((t (:foreground "cyan" :underline t))))
     (org-date ((t (:foreground "LightSteelBlue" :underline t))))
     (org-hide ((t (:foreground "#2e3436"))))
     (org-level-1 ((t (:foreground "#cae682"))))
     (org-level-2 ((t (:foreground "#cae682"))))
     (org-level-3 ((t (:foreground "#8ac6f2"))))
     (org-level-4 ((t (:italic t :slant italic :foreground "#95e454"))))
     (org-level-5 ((t (:foreground "#e5786d"))))
     (org-todo ((t (:bold t :foreground "#8ac6f2" :weight bold))))
     (paren-face-match ((t (:foreground "#eeeeec" :background "#729fcf"))))
     (paren-face-match-light
      ((t (:foreground "#eeeeec" :background "#729fcf"))))
     (paren-face-mismatch ((t (:foreground "#2e3436" :background "#ad7fa8"))))
     (persp-selected-face ((t (:foreground "#729fcf"))))
     (query-replace ((t (:foreground "#2e3436" :background "#f57900"))))
     (region ((t (:background "#444444" :foreground "#f6f3e8"))))
     (scroll-bar ((t (:foreground "systemscrollbar"))))
     (secondary-selection ((t (:background "SkyBlue4"))))
     (sgml-namespace ((t (:foreground "#8ac6f2"))))
     (shadow ((t (:foreground "grey70"))))
     (show-paren-match ((t (:background "#729fcf" :foreground "#eeeeec"))))
     (show-paren-mismatch ((t (:background "#ad7fa8" :foreground "#2e3436"))))
     (tmm-inactive ((t (:foreground "grey70"))))
     (tool-bar
      ((t (:background "systembuttonface" :foreground "systembuttontext"
                       :box (:line-width 1 :style released-button)))))
     (tooltip
      ((t (:background "systeminfowindow" :foreground "systeminfotext"))))
     (trailing-whitespace ((t (:background "red1"))))
     (underline ((t (:underline t))))
     (vertical-border ((t (nil))))
     (widget-button ((t (:bold t :weight bold))))
     (widget-button-pressed ((t (:foreground "red1"))))
     (widget-documentation ((t (:foreground "lime green"))))
     (widget-field ((t (:background "dim gray"))))
     (widget-inactive ((t (:foreground "grey70"))))
     (widget-single-line-field ((t (:background "dim gray")))))))

(defun color-theme-ir-black ()
  (interactive)
  (let ((*normal* "#F6F3E8")
        (*background* "#000000")
        (*cursor* "#FFA560")
        (*mouse* "sienna1")
        (*region* "#666666")
        (*current-line* "#151515")
        (*string* "#A8FF60")
        (*builtin* "#5EA6EA")
        (*keyword* "#0074E8")
        (*method* "#A8937A")
        (*comment* "#7C7C7C")
        (*constant* "#99CC99")
        (*red* "#F1266F")
        (*operator* "#FFFFFF")
        (*class* "#FFFFB6")
        (*variable* "#C6C5FE"))

    (flet ((color (fgcolor &optional (bgcolor nil) (bold nil) (italic nil)
                           (underline nil))
                  `((t (,@(if fgcolor `(:foreground ,fgcolor))
                        ,@(if bgcolor `(:background ,bgcolor))
                        ,@(if bold '(:bold t))
                        ,@(if italic '(:italic t))
                        ,@(if underline '(:underline t))))))
           (face (face &rest args)
                 `(,(intern (concat "font-lock-" face "-face"))
                   ,(apply #'color args))))

      (color-theme-install
       `(color-theme-ir-black
         ((background-color . ,*background*)
          (background-mode . dark)
          (border-color . ,*background*)
          (cursor-color . ,*cursor*)
          (foreground-color . ,*normal*)
          (mouse-color . ,*mouse*))
         (default ,(color *normal* *background*))
         (blue ,(color "blue"))
         (border-glyph ((t (nil))))
         (buffers-tab ,(color *normal* *background*))
         ,(face "builtin" *builtin*)
         ,(face "comment" *comment*)
         ,(face "constant" *constant*)
         ,(face "doc-string" *string*)
         ,(face "function-name" *method*)
         ,(face "keyword" *keyword*)
         ,(face "preprocessor" *keyword*)
         ,(face "reference" "#99CC99")
         ,(face "regexp-grouping-backslash" *red*)
         ,(face "regexp-grouping-construct" *red*)
         ,(face "string" *string*)
         ,(face "type" "#FFB774")
         ,(face "variable-name" *variable*)
         ,(face "warning" "white" *red*)
         (gui-element ,(color *background* "#D4D0C8"))
         (region ,(color nil *region*))
         (mode-line ,(color *background* "grey75"))
         (highlight ,(color nil *current-line*))
         (highline-face ,(color nil *current-line*))
         (italic ((t (nil))))
         (left-margin ((t (nil))))
         (text-cursor ,(color *background* "yellow"))
         (toolbar ((t (nil))))
         (bold ((t (:bold t))))
         (bold-italic ((t (:bold t))))
         (underline ((nil (:underline nil))))
         (ido-subdir ,(color *red* *background*)))))))

(defun color-theme-railscasts ()
  "MIT License Copyright (c) 2009 Oleg Shaldybin
 <oleg.shaldybin@gmail.com> Inspired by the brilliant Railscasts
 theme for TextMate."
  (interactive)
  (color-theme-install
   '(color-theme-railscasts
     ((background-color . "#232323")
      (background-mode . dark)
      (cursor-color . "#5A647E")
      (foreground-color . "#E6E1DC"))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (fringe ((t (:background "#232323"))))
     (font-lock-builtin-face ((t (:foreground "#D0D0FF"))))
     (font-lock-comment-face ((t (:foreground "#BC9458" :italic t))))
     (font-lock-constant-face ((t (:foreground "#6D9CBE"))))
     (font-lock-doc-string-face ((t (:foreground "#A5C261"))))
     (font-lock-function-name-face ((t (:foreground "#FFC66D"))))
     (font-lock-keyword-face ((t (:foreground "#CC7833"))))
     (font-lock-preprocessor-face ((t (:foreground "#CC7833"))))
     (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-string-face ((t (:foreground "#A5C261"))))
     (font-lock-type-face ((t (:foreground "white"))))
     (font-lock-variable-name-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-warning-face ((t (:foreground "Pink"))))
     (paren-face-match-light
      ((t (:foreground "#FFC66D" :background "#555577"))))
     (highlight ((t (:background "darkolivegreen"))))
     (italic ((t (:italic t))))
     (modeline ((t (:background "#A5BAF1" :foreground "black"))))
     (modeline-buffer-id ((t (:background "#A5BAF1" :foreground "black"))))
     (modeline-mousable ((t (:background "#A5BAF1" :foreground "black"))))
     (modeline-mousable-minor-mode
      ((t (:background "#A5BAF1" :foreground "black"))))
     (region ((t (:background "#555577"))))
     (primary-selection ((t (:background "#555577"))))
     (isearch ((t (:background "#555555"))))
     (zmacs-region ((t (:background "#555577"))))
     (secondary-selection ((t (:background "darkslateblue"))))
     (flymake-errline ((t (:background "LightSalmon" :foreground "black"))))
     (flymake-warnline ((t (:background "LightSteelBlue" :foreground "black"))))
     (underline ((t (:underline t))))
     (minibuffer-prompt ((t (:bold t :foreground "#FF6600")))))))

(defun color-theme-blissterm ()
  "Terminal color theme by mrBliss"
  (interactive)
  (color-theme-install
   '(color-theme-blissterm
     ((background-color . "black")
      (background-mode . dark)
      (foreground-color . "white")
      (cursor-color . "white"))
     (default ((t (nil))))
     (mode-line-inactive ((t (:background "black" :foreground "white"))))
     (flyspell-duplicate ((t (:foreground "yellow"))))
     (flyspell-incorrect ((t (:foreground "red"))))
     (font-lock-builtin-face ((t (:foreground "magenta"))))
     (font-lock-comment-face ((t (:foreground "black" :bold t))))
     (font-lock-comment-delimiter-face ((t (:foreground "black" :bold t))))
     (font-lock-constant-face ((t (:foreground "blue"))))
     (font-lock-doc-string-face ((t (:foreground "green" :bold t))))
     (font-lock-string-face ((t (:foreground "green" :bold t))))
     (font-lock-function-name-face ((t (:foreground "green"))))
     (font-lock-keyword-face ((t (:foreground "cyan"))))
     (font-lock-type-face ((t (:underline t :foreground "yellow"))))
     (font-lock-variable-name-face ((t (:foreground "#7587A6" :bold t))))
     (font-lock-warning-face ((t (:bold t :foreground "#F9EE98"))))
     (erc-notice-face ((t (:foreground "black" :bold t))))
     (erc-timestamp-face ((t (:foreground "#F1266F"))))
     (erc-direct-msg-face ((t (:foreground "#0074e8"))))
     (erc-nick-msg-face ((t (:foreground "black" :bold t))))
     (erc-pal-face ((t (:foreground "green"))))
     (erc-input-face ((t (:foreground "yellow"))))
     (erc-current-nick-face ((t (:foreground "magenta"))))
     (erc-my-nick-face ((t (:foreground "magenta"))))
     (erc-prompt-face ((t (:foreground "magenta")))))))

(defun color-theme-bespin ()
  "Based on Mozilla's Bespin. By mrBliss"
  (interactive)
  (color-theme-install
   '(color-theme-bespin
     ((background-color . "#302620")
      (foreground-color . "#BAAE9E")
      (cursor-color . "#ffffff"))
     (default ((t (nil))))
     (lazy-highlight
      ((t (:background "#5EA6EA" :foreground "#ffffff"
                       :box nil))))
     (highlight
      ((t (:background "#0074e8" :foreground "#ffffff"))))
     (modeline
      ((t (:background "#433E37" :foreground "#BAAE9E"
                       :box (:line-width 1 :color "#4c4a41" :style nil)))))
     (modeline-buffer-id ((t nil)))
     (mode-line-highlight
      ((t (:box (:line-width 1 :color "grey40" :style released-button)))))
     (mode-line-inactive
      ((t (:background "#1e1915" :foreground "#BAAE9E"
                       :box (:line-width 1 :color "#1e1915")))))
     (minibuffer-prompt ((t (:foreground "#5EA6EA"))))
     (flyspell-duplicate
      ((t (:foreground "#FF8844" :bold t :underline t))))
     (flyspell-incorrect
      ((t (:foreground "#F1266F" :bold t :underline t))))
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
     (hl-line ((t (:background "#211B15"))))
     (region ((t (:background "#f8fab7"))))
     (link ((t (:foreground "#5EA6EA" :underline t))))
     (ido-subdir ((t (:foreground "#F1266F"))))
     (diredp-dir-heading
      ((t (:background nil :underline t :foreground "#5EA6EA"))))
     (diredp-inode+size ((t (:foreground "#A8937A" :italic t))))
     (diredp-file-name ((t (:foreground "#0074e8"))))
     (diredp-file-suffix ((t (:foreground "#7587A6"))))
     (diredp-dir-priv
      ((t (:background nil :italic t :foreground "#89BDFF"))))
     (diredp-read-priv ((t (:background nil :foreground "#BAAE9E"))))
     (diredp-write-priv ((t (:background nil :foreground "#F1266F"))))
     (diredp-exec-priv ((t (:background nil :foreground "#FF8844"))))
     (diredp-no-priv ((t (:background nil :foreground "#BAAE9E"))))
     (diredp-date-time ((t (:foreground "#00920A"))))
     (diredp-flag-mark ((t (:background nil :foreground "#FF8844"))))
     (diredp-flag-mark-line ((t (:background "#F9EE98" :foreground "#666666"))))
     (diredp-deletion ((t (:background nil :foreground "#F1266F"))))
     (diredp-deletion-file-name ((t (:background nil :foreground "#F1266F"))))
     (diredp-ignored-file-name ((t (:foreground "#666666"))))
     (diredp-compressed-file-suffix ((t (:foreground "#DDF2A4"))))
     (diredp-executable-tag ((t (:foreground "#DDF2A4"))))
     (erc-notice-face ((t (:foreground "#666666"))))
     (erc-timestamp-face ((t (:foreground "#F1266F"))))
     (erc-direct-msg-face ((t (:foreground "#0074e8"))))
     (erc-nick-msg-face ((t (:foreground "#0074e8" :bold t))))
     (erc-pal-face ((t (:foreground "#0074e8"))))
     (erc-input-face ((t (:foreground "#5EA6EA" :italic t))))
     (erc-current-nick-face ((t (:foreground "#F1266F"))))
     (erc-my-nick-face ((t (:foreground "#A8937A" :italic t))))
     (erc-prompt-face ((t (:background nil :foreground "#F1266F"))))
     (nxml-processing-instruction-delimiter-face ((t (:foreground "#0074e8"))))
     (nxml-tag-delimiter-face ((t (:foreground "#5EA6EA"))))
     (nxml-element-local-name-face ((t (:foreground "#0074e8"))))
     (nxml-attribute-local-name-face ((t (:foreground "#5EA6EA"))))
     (nxml-processing-instruction-target-face ((t (:foreground "#0074e8"))))
     (nxml-attribute-value-face ((t (:foreground "#00920A"))))
     (nxml-prolog-literal-content-face ((t (:foreground "#00920A"))))
     (nxml-prolog-keyword-face ((t (:foreground "#5EA6EA"))))
     (nxml-namespace-attribute-xmlns-face ((t (:foreground "#5EA6EA"))))
     (escape-glyph ((t (:foreground "#F1266F"))))
     (markdown-inline-code-face ((t (:foreground "#5EA6EA"))))
     (markdown-pre-face ((t (:foreground "#5EA6EA"))))
     (undo-tree-visualizer-current-face ((t (:foreground "#F1266F"))))
     (slime-repl-result-face ((t (:foreground "#a8937a")))))))

(provide 'custom-themes)

