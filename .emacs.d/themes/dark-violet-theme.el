;;; dark-violet-theme.el --- A dark, violet theme

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
;; load-theme' and select "dark-violet".

;;; Code:

(deftheme dark-violet "A dark, violet and blue color-theme")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'dark-violet
   `(cursor ((,class (:background "#ffffff"))))
   `(default ((,class (:background "#1C1C1C" :foreground "#DDEEDD"))))
   `(mode-line
     ((,class (:background "#4E4E4E" :foreground "#FFFFFF"
                           :box (:line-width 1 :color "#3D3D3D" :style nil)))))
   `(mode-line-inactive
     ((,class (:background "#353535" :foreground "#DDEEDD"
                           :box (:line-width 1 :color "#454545")))))
   `(fringe ((,class (:background "#303030"))))
   `(linum ((,class (:background "#303030"))))

   `(font-lock-builtin-face ((,class (:foreground "#D7AFD7"))))
   `(font-lock-comment-face ((,class (:foreground "#808080" :background "#1A1A1A"))))
   `(font-lock-constant-face ((,class (:foreground "#86A2BE"))))
   `(font-lock-string-face ((,class (:foreground "#5E468C" :background "#202020"))))
   `(font-lock-function-name-face ((,class (:foreground "#9C75DD" :italic t))))
   `(font-lock-keyword-face ((,class (:foreground "#8CC8DB"))))
   `(font-lock-type-face ((,class (:foreground "#6096BF"))))
   `(font-lock-variable-name-face ((,class (:foreground "#395573" :bold t))))
   `(font-lock-warning-face ((,class (:bold t :foreground "#AFD787"))))
   ;; (font-lock-doc-string-face ((t (:foreground "#5EA6EA"))))

   `(region ((,class (:background "#477AB3" :foreground "#FFFFFF"))))
   `(highlight ((,class (:background "#477AB3" :foreground "#ffffff"))))
   `(highlight-80+ ((,class (:background "#BF4D80"))))
   `(hl-line ((,class (:background "#222222"))))

   `(minibuffer-prompt ((,class (:foreground "#6096BF"))))
   `(link ((,class (:foreground "#6096BF" :underline t))))
   `(comint-highlight-prompt ((,class (:foreground "#6096BF"))))
   `(geiser-font-lock-doc-link ((,class (:foreground "#6096BF" :underline t))))
   `(org-link ((,class (:foreground "#6096BF" :underline t))))

   `(info-menu-star ((,class (:foreground "#BF4D80"))))
   `(escape-glyph ((,class (:foreground "#BF4D80"))))
   `(ido-subdir ((,class (:foreground "#AF87D7"))))
   `(ido-first-match ((,class (:foreground "#5E468C"))))
   `(ido-only-match ((,class (:foreground "#31658C"))))
   `(undo-tree-visualizer-current-face ((,class (:foreground "#BF4D80"))))

   `(flyspell-duplicate
     ((,class (:foreground "#FF907A" :bold t :underline t))))
   `(flyspell-incorrect
     ((,class (:foreground "#BF4D80" :bold t :underline t))))

   `(flymake-errline ((,class (:underline "red" :background nil))))
   `(flymake-warnline ((,class (:underline "#FF907A" :background nil))))

   `(ensime-errline-highlight ((,class (:underline "red" :background nil))))

   `(diff-added ((,class (:foreground "green3"))))
   `(diff-indicator-added ((,class (:foreground "green3"))))
   `(diff-changed ((,class (:foreground "#FF907A"))))
   `(diff-indicator-changed ((,class (:foreground "#FF907A"))))
   `(diff-removed ((,class (:foreground "red3"))))
   `(diff-indicator-removed ((,class (:foreground "red3"))))

   `(diredp-dir-heading
     ((,class (:background nil :underline t :foreground "#6096BF"))))
   `(diredp-inode+size ((,class (:foreground "#8CC8DB" :italic t))))
   `(diredp-file-name ((,class (:foreground "#A9D1DF"))))
   `(diredp-file-suffix ((,class (:foreground "#8B769F"))))
   `(diredp-dir-priv
     ((,class (:background nil :italic t :foreground "#6096BF"))))
   `(diredp-link-priv ((,class (:foreground "#287373"))))
   `(diredp-rare-priv ((,class (:foreground "#BF4D80" :background nil))))
   `(diredp-other-priv ((,class (:foreground "#477AB3" :background nil))))
   `(diredp-read-priv ((,class (:background nil :foreground "#6096BF"))))
   `(diredp-write-priv ((,class (:background nil :foreground "#D7AFD7"))))
   `(diredp-exec-priv ((,class (:background nil :foreground "#5E468C"))))
   `(diredp-no-priv ((,class (:background nil :foreground "#DDEEDD"))))
   `(diredp-date-time ((,class (:foreground "#8CC8DB"))))
   `(diredp-flag-mark ((,class (:background nil :foreground "#8CC8DB"))))
   `(diredp-flag-mark-line ((,class (:background "#8CC8DB" :foreground "#1A1A1A"))))
   `(diredp-deletion ((,class (:background nil :foreground "#FF00FF"))))
   `(diredp-deletion-file-name ((,class (:background nil :foreground "#BF4D80"))))
   `(diredp-ignored-file-name ((,class (:foreground "#808080"))))
   `(diredp-compressed-file-suffix ((,class (:foreground "#D7AF87"))))
   `(diredp-executable-tag ((,class (:foreground "#BF4D80"))))
   `(diredp-symlink ((,class (:foreground "#287373"))))
   `(diredp-number ((,class (:foreground "#D7AFD7"))))

   `(speedbar-button-face ((,class (:foreground "#BF4D80"))))
   `(speedbar-file-face ((,class (:foreground "#A9D1DF"))))
   `(speedbar-directory-face ((,class (:foreground "#6096BF"))))
   `(ecb-default-highlight-face
     ((,class (:background "#BF4D80" :foreground "#FFFFFF"))))

   `(magit-section-title ((,class (:foreground "#86A2BE"))))
   `(magit-branch ((,class (:foreground "#D7AFD7"))))
   `(magit-item-highlight ((,class (:background "#353535"))))
   `(magit-log-sha1 ((,class (:foreground "#AF87D7"))))

   `(erc-default-face ((,class (:foreground "#999999"))))
   `(erc-notice-face ((,class (:foreground "#3D3D3D"))))
   `(erc-timestamp-face ((,class (:foreground "#BF4D80"))))
   `(erc-direct-msg-face ((,class (:foreground "#BF4D80"))))
   `(erc-nick-msg-face ((,class (:foreground "#BF4D80" :bold t))))
   `(erc-pal-face ((,class (:foreground "#53A6A6"))))
   `(erc-input-face ((,class (:foreground "#86A2BE" :italic t))))
   `(erc-current-nick-face ((,class (:foreground "#BF4D80"))))
   `(erc-nick-default-face ((,class (:foreground "#477AB3"))))
   `(erc-my-nick-face ((,class (:foreground "#BF4D80" :italic t))))
   `(erc-prompt-face ((,class (:background nil :foreground "#6096BF"))))

   `(garak-system-message-face ((,class (:foreground "#3D3D3D"))))
   `(garak-nick-face ((,class (:foreground "#477AB3"))))
   `(garak-own-nick-face ((,class (:foreground "#BF4D80"))))
   `(lui-time-stamp-face ((,class (:foreground "#BF4D80"))))
   `(lui-button-face ((,class (:foreground "#6096BF" :underline t))))

   `(font-latex-warning-face ((,class (:foreground "#FF00FF"))))
   `(font-latex-italic-face
     ((,class (:foreground "#AF87D7" :italic t :underline nil))))
   `(font-latex-bold-face ((,class (:foreground "#849DA2" :bold t))))
   `(font-latex-sectioning-1-face ((,class (:foreground "#395573"))))
   `(font-latex-sectioning-2-face ((,class (:foreground "#31658C"))))
   `(font-latex-sectioning-3-face ((,class (:foreground "#6096BF"))))
   `(font-latex-sectioning-4-face ((,class (:foreground "#86A2BE"))))
   `(font-latex-sectioning-5-face ((,class (:foreground "#A9D1DF"))))
   `(font-latex-sedate-face ((,class (:foreground "#6096BF"))))

   `(markdown-inline-code-face ((,class (:foreground "#6096BF"))))
   `(markdown-pre-face ((,class (:foreground "#6096BF"))))

   `(nxml-processing-instruction-delimiter-face ((,class (:foreground "#31658C"))))
   `(nxml-tag-delimiter-face ((,class (:foreground "#31658C"))))
   `(nxml-element-local-name-face ((,class (:foreground "#31658C"))))
   `(nxml-attribute-local-name-face ((,class (:foreground "#86A2BE"))))
   `(nxml-processing-instruction-target-face ((,class (:foreground "#31658C"))))
   `(nxml-attribute-value-face ((,class (:foreground "#9C75DD"))))
   `(nxml-attribute-value-delimiter-face ((,class (:foreground "#9C75DD"))))
   `(nxml-comment-content-face ((,class (:foreground "#808080"))))
   `(nxml-comment-delimiter-face ((,class (:foreground "#808080"))))
   `(nxml-entity-ref-name-face ((,class (:foreground "#D7AF87"))))
   `(nxml-entity-ref-delimiter-face ((,class (:foreground "#D7AF87"))))
   ;; (nxml-prolog-literal-content-face ((t (:foreground "#808080"))))
   ;; (nxml-prolog-keyword-face ((t (:foreground "#5EA6EA"))))
   ;; (nxml-namespace-attribute-xmlns-face ((t (:foreground "#5EA6EA"))))

   `(w3m-anchor ((,class (:foreground "#6096BF" ))))
   `(w3m-header-line-location-title ((,class (:foreground "#9C75DD"))))
   `(w3m-header-line-location-content ((,class (:foreground "#D7AF87"))))
   `(w3m-image-anchor ((,class (:background "#31658C" :foreground "#DDEEDD"))))

   `(sml-modeline-end-face ((,class (:background "#222222"))))
   `(sml-modeline-vis-face ((,class (:background "#808080"))))

   `(num3-face-even ((,class (:background nil :foreground nil :underline t :weight bold))))

   `(clojure-parens ((,class (:foreground "#4E4E4E"))))
   `(clojure-keyword ((,class (:foreground "#45b8f2"))))
   `(clojure-double-quote ((,class (:foreground "#5E468C"))))
   `(clojure-special ((,class (:foreground "#AF87D7"))))))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))



(provide-theme 'dark-violet)
;;; dark-violet-theme.el ends here
