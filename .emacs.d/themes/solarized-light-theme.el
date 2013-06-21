;;; solarized-light-theme.el --- Solarized for Emacs

;; Copyright (C) 2013 Thomas Winant

;; Author: Thomas Winant <dewinant@gmail.com>
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this theme, download it to ~/.emacs.d/themes. In your `.emacs' or
;; `init.el', add this line:
;;
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;
;; Once you have reloaded your configuration (`eval-buffer'), do `M-x
;; load-theme' and select "solarized-light".

;;; Credits:

;; Original theme by Ethan Schoonover.
;; Original port to Emacs by Greg Pfeil

;;; Code:


(deftheme solarized-light "The light variant of the Solarized theme")

(let ((class '((class color) (min-colors 89)))
      (base03  "#002b36")
      (base02  "#073642")
      (base01  "#586e75")
      (base00  "#657b83")
      (base0   "#839496")
      (base1   "#93a1a1")
      (base2   "#eee8d5")
      (base25  "#f9f2e0")
      (base3   "#fdf6e3")
      (yellow  "#b58900")
      (orange  "#cb4b16")
      (red     "#dc322f")
      (magenta "#d33682")
      (violet  "#6c71c4")
      (blue    "#268bd2")
      (cyan    "#2aa198")
      (green   "#859900"))
  (custom-theme-set-faces
   'solarized-light
   `(cursor ((,class (:foreground ,base0 :background ,base03
                                  :inverse-video t))))
   `(default ((,class (:foreground ,base00 :background ,base3))))
   `(header-line ((,class (:foreground ,base00 :background ,base02))))
   `(fringe ((,class (:foreground ,base1 :background ,base2))))

   `(mode-line ((,class (:foreground ,base25 :background ,base01
                                     :box (:color ,base2 :style nil)))))
   `(mode-line-buffer-id ((,class (:foreground ,base01))))
   `(mode-line-inactive
     ((,class (:foreground ,base00  :background ,base3
                           :box (:color ,base3 :style nil)))))
   `(linum ((,class (:background ,base2))))
   `(powerline-active1 ((,class (:foreground ,base3 :background ,base1))))
   `(powerline-active2 ((,class (:foreground ,base00 :background ,base2))))
   `(powerline-inactive1 ((,class (:foreground ,base3 :background ,base1))))
   `(powerline-inactive2 ((,class (:foreground ,base00 :background ,base2))))

   `(font-lock-builtin-face ((,class (:foreground ,magenta))))
   `(font-lock-comment-face ((,class (:foreground ,base1 :italic t
                                                  :background ,base25))))
   `(font-lock-constant-face ((,class (:foreground ,magenta))))
   `(font-lock-string-face ((,class (:foreground ,cyan :background nil))))
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue :bold t))))
   `(font-lock-warning-face ((,class (:foreground ,magenta :bold t))))
   `(font-lock-doc-face ((,class (:foreground ,cyan :italic t))))
   `(font-lock-color-constant-face ((,class (:foreground ,green))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,base1 :bold t))))
   `(font-lock-doc-string-face ((,class (:foreground ,green))))
   `(font-lock-preprocessor-face ((,class (:foreground ,orange))))
   `(font-lock-reference-face ((,class (:foreground ,cyan))))
   `(font-lock-negation-char-face ((,class (:foreground ,red))))
   `(font-lock-other-type-face ((,class (:foreground ,blue :italic t))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,orange))))
   `(font-lock-special-keyword-face ((,class (:foreground ,magenta))))
   `(font-lock-exit-face ((,class (:foreground ,red))))
   `(font-lock-other-emphasized-face ((,class (:foreground ,violet :bold t
                                                           :italic t))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))

   `(region ((,class (:background ,base2))))
   `(highlight ((,class (:background ,base2))))
   `(hl-line ((,class (:background ,base2))))
   `(isearch ((,class (:foreground ,yellow :inverse-video t))))
   `(highlight-80+ ((,class (:background ,magenta :foreground ,base2))))

   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(link ((,class (:foreground ,violet :underline t))))
   `(link-visited ((,class (:foreground ,magenta :underline t))))
   `(comint-highlight-prompt ((,class (:foreground ,blue))))
   `(geiser-font-lock-doc-link ((,class (:foreground ,violet :underline t))))

   `(info-menu-star ((,class (:foreground ,magenta))))
   `(escape-glyph-face ((,class (:foreground ,red))))
   `(ido-subdir ((,class (:foreground ,blue))))
   `(ido-first-match ((,class (:foreground ,green :bold t))))
   `(ido-only-match ((,class (:foreground ,green))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,red))))

   `(flyspell-duplicate ((,class (:background ,orange :background nil :bold t
                                              :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,green :background nil :bold t
                                              :underline t))))

   `(flymake-errline ((,class (:underline ,red :background nil))))
   `(flymake-warnline ((,class (:underline ,green :background nil))))

   `(diff-added ((,class (:foreground ,green :inverse-video t))))
   `(diff-indicator-added ((,class (:foreground ,green :inverse-video t))))
   `(diff-changed ((,class (:foreground ,yellow :inverse-video t))))
   `(diff-indicator-changed ((,class (:foreground ,yellow :inverse-video t))))
   `(diff-removed ((,class (:foreground ,red :inverse-video t))))
   `(diff-indicator-removed ((,class (:foreground ,red :inverse-video t))))
   `(diff-header ((,class (:background ,base1))))
   `(diff-file-header ((,class (:background ,base01 :foreground ,base1
                                            :bold t))))
   `(diff-refine-change ((,class (:background ,base01))))

   `(diredp-dir-heading ((,class (:background nil :underline t
                                              :foreground ,magenta))))
   `(diredp-inode+size ((,class (:foreground ,blue :italic t))))
   `(diredp-file-name ((,class (:foreground ,base02))))
   `(diredp-file-suffix ((,class (:foreground ,base00))))
   `(diredp-dir-priv ((,class (:background nil :italic t :foreground ,base01))))
   `(diredp-link-priv ((,class (:foreground ,base1))))
   `(diredp-rare-priv ((,class (:foreground ,yellow :background nil))))
   `(diredp-other-priv ((,class (:foreground ,cyan :background nil))))
   `(diredp-read-priv ((,class (:background nil :foreground ,blue))))
   `(diredp-write-priv ((,class (:background nil :foreground ,violet))))
   `(diredp-exec-priv ((,class (:background nil :foreground ,magenta))))
   `(diredp-no-priv ((,class (:background nil :foreground ,base2))))
   `(diredp-date-time ((,class (:foreground ,blue))))
   `(diredp-flag-mark ((,class (:background nil :foreground ,base02))))
   `(diredp-flag-mark-line ((,class (:background ,base01 :foreground ,base2))))
   `(diredp-deletion ((,class (:background nil :foreground ,red))))
   `(diredp-deletion-file-name ((,class (:background nil :foreground ,red))))
   `(diredp-ignored-file-name ((,class (:foreground ,base00))))
   `(diredp-compressed-file-suffix ((,class (:foreground ,yellow))))
   `(diredp-executable-tag ((,class (:foreground ,magenta))))
   `(diredp-symlink ((,class (:foreground ,cyan))))
   `(diredp-number ((,class (:foreground ,base01))))

   `(magit-section-title ((,class (:foreground ,blue))))
   `(magit-branch ((,class (:foreground ,magenta))))
   `(magit-item-highlight ((,class (:background ,base2))))
   `(magit-log-sha1 ((,class (:foreground ,magenta))))

   `(erc-default-face ((,class (:foreground ,base03))))
   `(erc-notice-face ((,class (:foreground ,base1))))
   `(erc-timestamp-face ((,class (:foreground ,magenta))))
   `(erc-direct-msg-face ((,class (:foreground ,magenta))))
   `(erc-nick-msg-face ((,class (:foreground ,magenta :bold t))))
   `(erc-pal-face ((,class (:foreground ,blue))))
   `(erc-input-face ((,class (:foreground ,cyan :italic t))))
   `(erc-current-nick-face ((,class (:foreground ,magenta))))
   `(erc-nick-default-face ((,class (:foreground ,blue))))
   `(erc-my-nick-face ((,class (:foreground ,violet :italic t))))
   `(erc-prompt-face ((,class (:background nil :foreground ,blue))))

   `(font-latex-warning-face ((,class (:foreground ,magenta))))
   `(font-latex-italic-face ((,class (:foreground ,violet :italic t
                                                  :underline nil))))
   `(font-latex-bold-face ((,class (:foreground ,base03 :bold t))))
   `(font-latex-sectioning-1-face ((,class (:foreground ,base03))))
   `(font-latex-sectioning-2-face ((,class (:foreground ,base02))))
   `(font-latex-sectioning-3-face ((,class (:foreground ,base01))))
   `(font-latex-sectioning-4-face ((,class (:foreground ,base1))))
   `(font-latex-sectioning-5-face ((,class (:foreground ,base01))))
   `(font-latex-sedate-face ((,class (:foreground ,blue))))

   `(markdown-inline-code-face ((,class (:foreground ,blue))))
   `(markdown-pre-face ((,class (:foreground ,blue))))

   `(nxml-processing-instruction-delimiter-face ((,class (:foreground ,base1))))
   `(nxml-tag-delimiter-face ((,class (:foreground ,base1))))
   `(nxml-element-local-name-face ((,class (:foreground ,base1))))
   `(nxml-attribute-local-name-face ((,class (:foreground ,blue))))
   `(nxml-processing-instruction-target-face ((,class (:foreground ,base1))))
   `(nxml-attribute-value-face ((,class (:foreground ,violet))))
   `(nxml-attribute-value-delimiter-face ((,class (:foreground ,violet))))
   `(nxml-entity-ref-name-face ((,class (:foreground ,yellow))))
   `(nxml-entity-ref-delimiter-face ((,class (:foreground ,yellow))))

   `(w3m-anchor ((,class (:foreground ,violet))))
   `(w3m-current-anchor ((,class (:foreground ,magenta))))
   `(w3m-header-line-location-title ((,class (:foreground ,base01))))
   `(w3m-header-line-location-content ((,class (:foreground ,base02))))
   `(w3m-image-anchor ((,class (:foreground ,violet))))

   `(show-paren-match-face ((,class (:background ,magenta :foreground ,base3))))
   `(paren-face-match ((,class (:background ,blue :foreground ,base3))))

   `(sml-modeline-end-face ((,class (:background ,base01 :foreground ,base3))))
   `(sml-modeline-vis-face ((,class (:background ,base03 :foreground ,base3))))

   `(clojure-parens ((,class (:foreground ,base1))))
   `(clojure-braces ((,class (:foreground ,violet))))
   `(clojure-brackets ((,class (:foreground ,blue))))
   `(clojure-java-call ((,class (:foreground ,violet))))
   `(clojure-keyword ((,class (:foreground ,yellow))))
   `(clojure-double-quote ((,class (:foreground ,cyan))))
   `(clojure-special ((,class (:foreground ,magenta))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'solarized-light)
;;; solarized-light-theme.el ends here
