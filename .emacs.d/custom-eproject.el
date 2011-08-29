;;; custom-eproject.el --- Custom eprojects
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: eproject, project


(require 'eproject)
(require 'eproject-extras)

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

;; Latex eproject
(define-project-type latex (generic)
  (look-for "_region_.tex")
  :relevant-files ("\\.tex")
  :irrelevant-files ("auto" "_region_.tex"))


(provide 'custom-eproject)