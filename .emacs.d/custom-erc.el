;;; custom-irc.el --- ERC settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: erc, irc


;; ERC preferences
(setq erc-nick "mrBliss")
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure")
        ("blinkenshell.org" "#blinkenshell")))

;; 'Wise' guys or bots in #clojure
(setq erc-pals '("LauJensen" "cemerick" "cgrand" "chouser" "clojurebot"
                 "danlarkin" "dnolen" "fogus" "rhickey" "sexpbot"
                 "stuartsierra" "stuarthalloway" "technomancy"))

;; Chekc my splelgin
(erc-spelling-mode 1)
(setq erc-spelling-dictionaries '(("#clojure" "english")))

;; Leaving/disconnecting behavior
(setq erc-server-reconnect-timeout 30
      erc-kill-buffer-on-part nil
      erc-kill-server-buffer-on-quit t)

;; ERC looks
(setq erc-smiley-mode t
      erc-header-line-face-method t)

(defun irc-connect ()
  "Connect to the IRC servers and open some channels."
  (interactive)
  (let ((pwd (read-passwd "Password: ")))
    (erc :server "irc.freenode.net" :port 6667 :nick "mrBliss" :password pwd)
    (erc-tls :server "irc.blinkenshell.org" :port 6697 :nick "mrBliss"
             :password pwd)))

(provide 'custom-erc)