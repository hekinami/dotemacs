;;; ------------------------------------------------------------
;;;
;;; miscellaneous
;;;
;;; ------------------------------------------------------------
(setq user-full-name "Zou Bibo")
(setq user-mail-address "hekinami@amiunique.net")

(setq savehist-file (concat (bibo/get-runtimes-dir) "history"))
(setq auto-save-list-file-prefix (concat (bibo/get-runtimes-dir) "auto-save-list/.saves-"))
(setq tramp-persistency-file-name (concat (bibo/get-runtimes-dir) "tramp"))
(global-auto-revert-mode)
(setq make-backup-files nil)
(auto-compression-mode t)
(auto-image-file-mode t)
(setq auto-save-mode -1)
(desktop-save-mode 0)

(require-package 'undo-tree)
(global-undo-tree-mode)

(fset 'yes-or-no-p 'y-or-n-p)
(ffap-bindings)

(global-set-key (kbd "C-c r") 'replace-regexp)
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)

(setq-default indent-tabs-mode nil)
(require-package 'aggressive-indent)
(global-aggressive-indent-mode 1)

(electric-pair-mode)

(require-package 'fancy-narrow)
(fancy-narrow-mode)

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

(global-set-key (kbd "<f1>") (lambda () (interactive)(switch-to-buffer "*scratch*")))
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

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

;;; ------------------------------------------------------------
;;;
;;; server
;;;
;;; ------------------------------------------------------------
(setq server-auth-dir (if *is-windows* (concat (file-name-as-directory (getenv "USERPROFILE") ) (file-name-as-directory "emacserver"))
                        (concat (bibo/get-runtimes-dir) "emacsserver")))
(unless (and (functionp 'server-running-p)
	     (server-running-p))
  (server-start))

;;; ------------------------------------------------------------
;;;
;;; multiple cursors
;;;
;;; ------------------------------------------------------------
(require-package 'multiple-cursors)
(setq mc/list-file (concat (bibo/get-runtimes-dir) ".mc-lists.el"))
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; ------------------------------------------------------------
;;;
;;; bm.el
;;;
;;; ------------------------------------------------------------
(require-package 'bm)
(require-package 'helm-bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "<C-S-f2>") 'helm-bm)

(provide 'init-editing)
