(setq erc-log-channels-directory "~/.erc/logs/")

(defun z/erc-generate-log-file-name (buffer target nick server port)
  "Generates a log-file name in the way ERC always did it.
This results in a file name of the form #channel!nick@server:port.txt.
This function is a possible value for `erc-generate-log-file-name-function'."
  (let ((file (concat
	       (if target (concat target "!"))
	       nick "@" server "_" (cond ((stringp port) port)
					 ((numberp port)
					  (number-to-string port))) ".txt")))
    ;; we need a make-safe-file-name function.
    (convert-standard-filename file)))

(setq erc-generate-log-file-name-function 'z/erc-generate-log-file-name)
(setq erc-log-file-coding-system 'utf-8)

(defun z/bitlbee-connect ()
  (interactive)
  (erc :server "localhost"
       :nick z/bitlbee-nickname))
;;; set z/bitlbee-nickname in custom.el

(defalias 'z/erc 'z/bitlbee-connect)

;;; https://github.com/fgeller/emacs-init/blob/master/init-erc.el
;; http://emacs-fu.blogspot.de/2012/03/social-networking-with-bitlbee-and-erc.html
(defun fg/bitlbee-identify ()
  (when (and (string= "localhost" erc-session-server)
             (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
                                   (erc-default-target)
                                   z/bitlbee-password))))
;;; set z/bitlbee-password in custom.el

(add-hook 'erc-join-hook 'fg/bitlbee-identify)

;;; https://github.com/fgeller/emacs-init/blob/master/init-erc.el
(defun fg/notify-privmsg (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      (todochiku-message (format "ERC message from: %s" nick)
                         msg
                         (todochiku-icon 'irc)
                         nil)))
  nil)

(add-hook 'erc-server-PRIVMSG-functions 'fg/notify-privmsg t)

(provide 'init-erc)
