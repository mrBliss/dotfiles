;;; custom-eproject.el --- Custom eprojects
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: eproject, project


(require 'eproject)
(require 'eproject-extras)

;; Don't turn it on for all files
(remove-hook 'find-file-hook 'eproject-maybe-turn-on)
(remove-hook 'dired-mode-hook 'eproject-maybe-turn-on)
(remove-hook 'after-change-major-mode-hook
             'eproject--after-change-major-mode-hook)
(add-hook 'c-mode-common-hook 'eproject-maybe-turn-on)
(add-hook 'lisp-common-hook 'eproject-maybe-turn-on)

;; Use ido's completing-read function
(setq eproject-completing-read-function 'eproject--ido-completing-read)

;; Clojure eprojects
(define-project-type clojure (generic)
  (look-for "project.clj")
  :relevant-files ("\\.clj"))

;; .conkerorrc eproject
(define-project-type conkerorrc (generic)
  (look-for "init.js")
  :relevant-files ("\\.js"))

;; Java (Eclipse) eproject
(define-project-type eclipse (generic)
  (look-for ".project")
  :relevant-files ("\\.java"))

;; Scala (ENSIME) eprojects
(define-project-type ensime (generic)
  (look-for ".ensime")
  :relevant-files ("\\.scala"))


(provide 'custom-eproject)