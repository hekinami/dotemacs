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

;;; ------------------------------------------------------------
;;;
;;; sudo
;;;
;;; ------------------------------------------------------------

;; based on
;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
;; https://www.emacswiki.org/emacs/TrampMode#toc14
(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!"))
    (setq header-line-format
	  (propertize warning 'face 'find-file-root-header-face))))

(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root (&optional arg)
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive "P")
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
	 ;; use a separate history list for "root" files.
	 (file-name-history find-file-root-history)
	 (name (or buffer-file-name default-directory))
	 (tramp (and (tramp-tramp-file-p name)
		     (tramp-dissect-file-name name)))
	 path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
	    dir (file-name-directory path)))

    (if arg
        (progn
          (find-alternate-file (concat find-file-root-prefix buffer-file-name))
          (setq find-file-root-history file-name-history)
          (run-hooks 'find-file-root-hook)
          )
      (when (setq file (read-file-name "find file (UID = 0): " dir path))
        (find-file (concat find-file-root-prefix file))
        ;; If this all succeeded save our new history list.
        (setq find-file-root-history file-name-history)
        ;; allow some user customization
        (run-hooks 'find-file-root-hook))
      )
    ))

(add-hook 'find-file-root-hook 'find-file-root-header-warning)
(global-set-key [(control x) (control r)] 'find-file-root)

(provide 'init-editing)
