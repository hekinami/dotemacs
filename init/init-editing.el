;;; ------------------------------------------------------------
;;;
;;; environment
;;;
;;; ------------------------------------------------------------
(setq user-full-name "Zou Bibo")
(setq user-mail-address "hekinami@amiunique.net")

(setq savehist-file (concat (bibo/get-runtimes-dir) "history"))
(setq auto-save-list-file-prefix (concat (bibo/get-runtimes-dir) "auto-save-list/.saves-"))
(global-auto-revert-mode)
(setq make-backup-files nil)
(auto-compression-mode t)
(auto-image-file-mode t)
(setq auto-save-mode -1)
(desktop-save-mode 0)

(require-package 'undo-tree)
(global-undo-tree-mode)

;;; ------------------------------------------------------------
;;;
;;; encoding
;;;
;;; ------------------------------------------------------------
(set-language-environment 'utf-8)
(setq encoding 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)

(when *is-windows*
  (set-clipboard-coding-system 'gb18030)
  (setq file-name-coding-system 'gb18030)
  
  (add-hook
   'shell-mode-hook
   '(lambda ()
      (set-buffer-process-coding-system 'gb18030 'gb18030)))
  (add-hook
   'eshell-mode-hook
   '(lambda ()
      (set-buffer-process-coding-system 'gb18030 'gb18030)))
  )

(when *is-linux*
  (set-clipboard-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8)
  )

;;; ------------------------------------------------------------
;;;
;;; buffer
;;;
;;; ------------------------------------------------------------
(require 'uniquify)
(setq  uniquify-buffer-name-style 'post-forward
       uniquify-separator ":")

(defun kill-buffer-when-exit ()
  "Close assotiated buffer when a process exited"
  (let ((current-process (ignore-errors (get-buffer-process (current-buffer)))))
    (when current-process
      (set-process-sentinel current-process
			    (lambda (watch-process change-state)
			      (when (string-match "\\(finished\\|exited\\)" change-state)
				(kill-buffer (process-buffer watch-process))))))))
(add-hook 'gdb-mode-hook 'kill-buffer-when-exit)
(add-hook 'jdb-mode-hook 'kill-buffer-when-exit)
(add-hook 'pdb-mode-hook 'kill-buffer-when-exit)
(add-hook 'comint-mode-hook 'kill-buffer-when-exit)
(add-hook 'shell-mode-hook 'kill-buffer-when-exit)
(add-hook 'inferior-python-mode-hook 'kill-buffer-when-exit)
(add-hook 'inferior-js-mode-hook 'kill-buffer-when-exit)
(add-hook 'compilation-mode-hook 'kill-buffer-when-exit)

(defun kill-buffer-and-its-windows (buffer)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer  (get-buffer buffer))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
      (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (and (buffer-modified-p buffer)
                   (fboundp '1on1-flash-ding-minibuffer-frame))
          (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
        (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
          (dolist (win  wins)           ; (User might keep buffer if modified.)
            (when (window-live-p win) (delete-window win)))))
    (when (interactive-p)
      (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-buffers '("gtd.org$" "\\*"))
(setq ido-save-directory-list-file (concat (bibo/get-runtimes-dir) "ido.last"))

;;; ------------------------------------------------------------
;;;
;;; miscellaneous
;;;
;;; ------------------------------------------------------------
(setq auto-save-mode -1)

(global-set-key (kbd "C-c r") 'replace-regexp)

;;; ------------------------------------------------------------
;;;
;;; navigation
;;;
;;; ------------------------------------------------------------
(require-package 'avy)
(setq avy-keys (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
(setq avy-style 'at)
(setq avy-background t)

;;; select current position to the position jumped to
(advice-add 'avy-goto-char :around (lambda (orig-fun &rest args)
                                     (push-mark)
                                     (apply orig-fun args)
                                     (forward-char)))

(define-key global-map (kbd "M-z") 'avy-goto-word-1)
(define-key global-map (kbd "M-/") 'avy-goto-char)

(provide 'init-editing)
