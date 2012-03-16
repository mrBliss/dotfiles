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
(defmacro with-erc (args props &rest body)
  "Anaphoric macro that takes `args' that are passed to a
`erc-catalog-entry' and a list of properties. The properties in
the `props' list are the bindings that will be constructed from
the erc arguments.

 Available properties: `nick', `host', `tld', `channel' and
`reason'. The `reason' property will be changed: if it is the
same as the channel name, it will be nil. ERC returns '\"ERC
Version 5.3\" (IRC client for Emacs)' as `reason', since we know
ERC, the 'IRC clie..' part will be stripped from the `reason'.

The `tld' property is extracted from the `host' property: the
alphanumeric characters after the last '.' in lowercase. Will be
nil when it could not be extracted.

Some properties will be bound even though they weren't in the
`props' list, because they will be needed for other
properties (`tld' requires `host' and `reason' requires
`channel')."
  `(let* ,(remove-if-not
           (lambda (b) (or (memq (car b) props)
                      (and (eq (car b) 'host) (memq 'tld props))
                      (and (eq (car b) 'channel) (memq 'reason props))))
           '((nick (cadr (memq ?n args)))
             (host (cadr (memq ?h args)))
             (tld (when (string-match ".+\\(\\.[a-z]+\\)$" host)
                    (downcase (match-string 1 host))))
             (channel (cadr (memq ?c args)))
             (reason (let ((raw (cadr (memq ?r args))))
                       (unless (or (not raw) (string= raw "")
                                   (string= raw channel))
                         (replace-regexp-in-string " (IRC client for Emacs)" ""
                                                   raw))))))
     ,@body))

;; Fix indentation of with-erc macro
(put 'with-erc 'lisp-indent-function 2)

(defun erc-message-join-format (&rest args)
  "Format a JOIN message. If the host ends with a TLD, display it
in parentheses."
  (with-erc args (nick tld)
    (format (if tld ">>>> %s (%s)" ">>>> %s") nick tld)))

(defun erc-message-quit-format (&rest args)
  "Format a QUIT message. If the host ends with a TLD, display it
in parentheses. A non-empty reason will also be displayed."
  (with-erc args (nick tld reason)
    (let ((reason-fmt (if reason (format " (%s)" reason) "")))
      (if tld
          (format "<<<< %s (%s)%s" nick tld reason-fmt)
        (format "<<<< %s%s" nick reason-fmt)))))

(defun erc-message-part-format (&rest args)
  "Format a proper PART message. If the host ends with a TLD, display it
in parentheses. A non-empty reason will also be displayed."
  (with-erc args (nick tld reason)
    (let ((reason-fmt (if reason (format " (%s)" reason) "")))
      (cond ((string= nick (erc-current-nick))
             (format "You have left channel %s%s" channel reason-fmt))
            (tld (format "<<<< %s (%s) left%s" nick tld reason-fmt))
            (t (format "<<<< %s left%s" nick reason-fmt))))))

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
  (:eval (format " %S users" (if (hash-table-p erc-channel-users)
                                 (hash-table-count erc-channel-users)
                               0))))
(add-hook 'erc-mode-hook 'erc-members-mode)

(defun irc-connect (&optional arg)
  "Connect to some IRC servers and open some channels. When
invoked with a prefix argument, all existing ERC buffers are
killed before connecting."
  (interactive "P")
  (when arg
    (mapc 'kill-buffer (erc-buffer-list nil)))
  (let ((pwd (read-passwd "Password: ")))
    (erc :server "irc.freenode.net" :port 6667 :nick "mrBliss" :password pwd)))


;; ZNC configuration

(require 'znc)

(defun znc ()
  "Connect to ZNC.
Prompt for password first."
  (interactive)
  (let ((pwd (read-passwd "Password: ")))
    (znc-erc-connect `(znc "192.168.1.3" 6668 nil "mrBliss" ,pwd))))

;; Make playback messages look like the originals

(require 'erc-replace)

(defun erc-replace-replay-join (s)
  (save-match-data
    (let ((nick (match-string 2))
          (host (match-string 3)))
      (erc-propertize (format "%s%s" erc-notice-prefix
                              (erc-message-join-format 110 nick 104 host))
                      'face
                      'erc-notice-face))))

(defun erc-replace-replay-quit (s)
  (save-match-data
    (let ((nick (match-string 2))
          (host (match-string 3))
          (reason (match-string 4)))
      (erc-propertize (format "%s%s" erc-notice-prefix
                              (erc-message-quit-format 110 nick 104 host 114 reason))
                      'face
                      'erc-notice-face))))

(defun erc-replace-replay-rename (s)
  (save-match-data
    (let ((prev-nick (match-string 2))
          (new-nick (match-string 4)))
      (erc-propertize (format "%s%s -> %s" erc-notice-prefix
                              prev-nick new-nick)
                      'face
                      'erc-notice-face))))

(setq erc-replace-alist
      `((,(concat "<\\*buffextras> \\(\\[[0-9][0-9]:[0-9][0-9]\\]\\)"
                  "\\s-+\\([^!]+\\)!\\(.+\\)\\s-+joined")
         . erc-replace-replay-join)
        (,(concat "<\\*buffextras> \\(\\[[0-9][0-9]:[0-9][0-9]\\]\\)"
                  "\\s-+\\([^!]+\\)!\\(.+\\)\\s-+\\(quit\\|parted\\)\\s-+with"
                  "\\s-+message:\\s-+\\[\\([^]]*\\)\\]")
         . erc-replace-replay-quit)
        (,(concat "<\\*buffextras> \\(\\[[0-9][0-9]:[0-9][0-9]\\]\\)"
                  "\\s-+\\([^!]+\\)!\\(.+\\)\\s-+is\\s-+now\\s-+known"
                  "\\s-+as\\s-+\\(.+\\)")
         . erc-replace-replay-rename)))

(erc-replace-mode 1)


(provide 'custom-erc)

