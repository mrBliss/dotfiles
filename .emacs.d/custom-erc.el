;;; custom-irc.el --- ERC settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: erc, irc


;; ERC preferences
(setq erc-nick "mrBliss")
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure" "#emacs")
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

;; Change the format of the JOIN, QUIT and PART messages
(defun erc-message-join-format (&rest args)
  "Format a JOIN message. If the host ends with a TLD, display it
in parentheses."
  (let* ((nick (cadr (memq ?n args)))
         (host (cadr (memq ?h args)))
         (match (string-match ".+\\(\\.[a-z]+\\)$" host)))
    (if match
        (format ">>>> %s (%s)" nick (downcase (match-string 1 host)))
      (format ">>>> %s" nick))))

(defun erc-message-quit-format (&rest args)
  "Format a QUIT message. If the host ends with a TLD, display it
in parentheses."
  (let* ((nick (cadr (memq ?n args)))
         (host (cadr (memq ?h args)))
         (reason (cadr (memq ?r args)))
         (reason-sans-erc               ; I know what ERC is
          (replace-regexp-in-string " (IRC client for Emacs)" "" reason))
         (match (string-match ".+\\(\\.[a-z]+\\)$" host)))
    (if match
        (format "<<<< %s (%s) (%s)" nick (downcase (match-string 1 host))
                reason-sans-erc)
      (format "<<<< %s (%s)" nick reason-sans-erc))))

(defun erc-message-part-format (&rest args)
  "Format a proper PART message. If the host ends with a TLD, display it
in parentheses."
  (let* ((nick (cadr (memq ?n args)))
         (host (cadr (memq ?h args)))
         (channel (cadr (memq ?c args)))
         (reason (cadr (memq ?r args)))
         (reason-fmt (if (or (string= reason "")
                             (string= reason channel))
                         "" (format " (%s)" ; I know what ERC is
                                    (replace-regexp-in-string
                                     " (IRC client for Emacs)" "" reason))))
         (match (string-match ".+\\(\\.[a-z]+\\)$" host)))
    (cond ((string= nick (erc-current-nick))
           (format "You have left channel %s%s" channel reason-fmt))
          (match
           (format "<<<< %s (%s) left %s%s"
                   nick (downcase (match-string 1 host)) channel reason-fmt))
          (t (format "<<<< %s left %s%s" nick channel reason-fmt)))))

(setq erc-custom-catalog-entries
      '((JOIN . erc-message-join-format)
        (QUIT . erc-message-quit-format)
        (PART . erc-message-part-format)
        (NICK . "%n -> %N")))
(dolist (msg erc-custom-catalog-entries)
  (erc-define-catalog-entry 'english (car msg) (cdr msg)))

;; Color the header-line red when disconnected
(defface erc-header-line-disconnected
  '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
          (t 'erc-header-line-disconnected))))
(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

;; Show the number of members in a channel in the mode line
(define-minor-mode erc-members-mode "" nil
  (:eval (format " %S users" (hash-table-count erc-channel-users))))
(add-hook 'erc-mode-hook 'erc-members-mode)

(defun irc-connect ()
  "Connect to the IRC servers and open some channels."
  (interactive)
  (let ((pwd (read-passwd "Password: ")))
    (erc :server "irc.freenode.net" :port 6667 :nick "mrBliss" :password pwd)
    (erc-tls :server "irc.blinkenshell.org" :port 6697 :nick "mrBliss"
             :password pwd)))

(provide 'custom-erc)