(deftheme mesa "mesa theme")

(custom-theme-set-variables
 'mesa
 '(fringe-mode 6 nil (fringe) )
 '(linum-format     " %7d "   )
 '(powerline-color1 "grey40"  )
 '(powerline-color2 "grey50"  )
 )


(custom-theme-set-faces
 'mesa

 '(fixed-pitch                         ((t (                                                                                                          ))))
 '(variable-pitch                      ((t (                                                                                                          ))))
 '(cursor                              ((t (                           :background "orange"                                                           ))))
 '(default                             ((t (:foreground "grey30"       :background "#ECE8E1"          :height 120   :inherit (fixed-pitch)            ))))
 '(linum                               ((t (:foreground "#8D8B87"      :background "#D4D0CA"          :height 70                                      ))))
 '(minibuffer-prompt                   ((t (:foreground "DeepSkyBlue4" :background nil                :weight bold                                    ))))
 '(escape-glyph                        ((t (:foreground "orange"       :background nil                                                                ))))
 '(highlight                           ((t (:foreground "#004A4F"      :background "DarkOrange"                                                       ))))
 '(region                              ((t (:foreground "#F8F8F8"      :background "#D4D0CA"                                                          ))))
 '(shadow                              ((t (:foreground "#999999"                                                                                     ))))
 '(secondary-selection                 ((t (                           :background "grey20"                                                           ))))
 '(trailing-whitespace                 ((t (                           :background "#ff0000"                                                          ))))

 '(font-lock-builtin-face              ((t (:foreground "DodgerBlue4"  :background nil                                                                ))))
 '(font-lock-comment-face              ((t (:foreground "#697799"      :background nil :slant italic                                                  ))))
 '(font-lock-constant-face             ((t (:foreground "snow4"        :background nil                                                                ))))
 `(font-lock-doc-string-face           ((t (:foreground "Brown"        :background nil                                                                ))))
 '(font-lock-function-name-face        ((t (:foreground "#1388a2"      :background nil                                                                ))))
 '(font-lock-keyword-face              ((t (:foreground "DeepSkyBlue4" :background nil                                                                ))))
 '(font-lock-negation-char-face        ((t (:foreground "#aF771F"      :background nil                                                                ))))
 '(font-lock-string-face               ((t (:foreground "DodgerBlue4"  :background "#e0eaef"                                                          ))))
 '(font-lock-variable-name-face        ((t (:foreground "wheat4"       :background nil                                                                ))))
 '(font-lock-comment-delimiter-face    ((t (:foreground "#3499aa"      :background nil                              :inherit (font-lock-comment-face) ))))
 '(font-lock-preprocessor-face         ((t (:foreground "#aC3D1A"      :background nil                              :inherit (font-lock-builtin-face) ))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f66500"      :background nil                              :inherit (bold)                   ))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red"          :background nil                              :inherit (bold)                   ))))
 '(font-lock-doc-face                  ((t (:foreground "#878289"      :background nil                              :inherit (font-lock-string-face)  ))))
 '(font-lock-warning-face              ((t (:foreground "red"          :background nil                              :inherit (error)                  ))))
 '(font-lock-type-face                 ((t (:foreground "#3388dd"                                                   :inherit (default)                ))))

 '(link                                ((t (:foreground "#00b7f0"      :background nil       :underline t                                             ))))
 '(link-visited                        ((t (:foreground "magenta4"                           :underline t           :inherit (link)                   ))))
 '(button                              ((t (                           :background "#aaa"    :underline t           :inherit (link)                   ))))
 '(fringe                              ((t (                           :background "#D4D0CA"                                                          ))))
 '(next-error                          ((t (                                                                        :inherit (region)                 ))))
 '(query-replace                       ((t (                                                                        :inherit (isearch)                ))))
 '(header-line                         ((t (:foreground "#222222"      :background "#bbbbbb" :box nil               :inherit (mode-line)              ))))
 '(mode-line-highlight                 ((t (                                                 :box nil                                                 ))))
 '(mode-line-emphasis                  ((t (                                                          :weight bold                                    ))))
 '(mode-line-buffer-id                 ((t (                                                 :box nil :weight bold                                    ))))
 '(mode-line-inactive                  ((t (:foreground "grey40"       :background "grey60"  :box nil :weight light :inherit (mode-line)              ))))
 '(mode-line                           ((t (:foreground "grey10"       :background "grey50"  :box nil :height 85    :inherit (variable-pitch)         ))))
 '(isearch                             ((t (:foreground "#99ccee"      :background "#444444"                                                          ))))
 '(isearch-fail                        ((t (                           :background "#ffaaaa"                                                          ))))
 '(lazy-highlight                      ((t (                           :background "#77bbdd"                                                          ))))
 '(match                               ((t (                           :background "#3388cc"                                                          ))))
 '(tooltip                             ((t (:foreground "black"        :background "LightYellow"                    :inherit (variable-pitch)         ))))
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'mesa)
;;; mesa-theme.el ends here
