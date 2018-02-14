;;; ------------------------------------------------------------
;;;
;;; miscellaneous
;;;
;;; ------------------------------------------------------------
(setq user-full-name "Zou Bibo")
(setq user-mail-address "hekinami@amiunique.net")

(use-package savehist
  :config
  (setq savehist-file (concat (z/get-runtimes-dir) "history")))

(setq auto-save-list-file-prefix (concat (z/get-runtimes-dir) "auto-save-list/.saves-"))
(setq tramp-persistency-file-name (concat (z/get-runtimes-dir) "tramp"))
(global-auto-revert-mode)
(setq make-backup-files nil)
(auto-compression-mode t)
(auto-image-file-mode t)
(setq auto-save-mode -1)
(desktop-save-mode 0)

(use-package undo-tree
  :ensure t
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (global-undo-tree-mode))

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-c r") 'replace-regexp)
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)

(setq-default indent-tabs-mode nil)

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

(electric-pair-mode)

(use-package fancy-narrow
  :ensure t
  :config
  (fancy-narrow-mode))

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
;;; locale
;;;
;;; ------------------------------------------------------------
(set-locale-environment "C")

;;; ------------------------------------------------------------
;;;
;;; buffer
;;;
;;; ------------------------------------------------------------
(use-package uniquify
  :config
  (setq  uniquify-buffer-name-style 'post-forward
         uniquify-separator ":"))

(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  (setq ido-create-new-buffer 'always)
  (setq ido-ignore-buffers '("gtd.org$" "\\*"))
  (setq ido-save-directory-list-file (concat (z/get-runtimes-dir) "ido.last")))

(global-set-key (kbd "<f1>") (lambda () (interactive)(switch-to-buffer "*scratch*")))

;;; ------------------------------------------------------------
;;;
;;; bs-configurations
;;;
;;; ------------------------------------------------------------
(use-package bs
  :bind ("C-x C-b" . bs-show)
  :config
  (setcar bs-configurations '("onlyfiles" nil nil "\\(gtd\\.org\\)\\|\\(diary\\)$" bs-visits-non-file bs-sort-buffer-interns-are-last))
  (setq bs-default-configuration "onlyfiles"))

;;; ------------------------------------------------------------
;;;
;;; navigation
;;;
;;; ------------------------------------------------------------
(use-package avy
  :ensure t
  :bind ("M-z" . avy-goto-word-1)
  :config
  (setq avy-keys (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
  (setq avy-style 'at)
  (setq avy-background t)
  ;;; select current position to the position jumped to
  (advice-add 'avy-goto-char :around (lambda (orig-fun &rest args)
                                       (push-mark)
                                       (apply orig-fun args)
                                       (forward-char))))

(use-package ace-pinyin
  :ensure t
  :bind
  (("M-/" . ace-pinyin-dwim)))

;;; ------------------------------------------------------------
;;;
;;; server
;;;
;;; ------------------------------------------------------------
(setq server-auth-dir (if *is-windows* (concat (file-name-as-directory (getenv "USERPROFILE") ) (file-name-as-directory "emacserver"))
                        (concat (z/get-runtimes-dir) "emacsserver")))
(unless (and (functionp 'server-running-p)
	     (server-running-p))
  (server-start))

;;; ------------------------------------------------------------
;;;
;;; multiple cursors
;;;
;;; ------------------------------------------------------------
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         :map mc/keymap
         ("C-z n" . mc/insert-numbers)
         ("C-z l" . mc/insert-letters))
  :config
  (add-hook 'multiple-cursors-mode-hook
            (lambda ()
              (define-key mc/keymap (kbd "C-z n") 'mc/insert-numbers)
              (define-key mc/keymap (kbd "C-z l") 'mc/insert-letters)
              )))

;;; ------------------------------------------------------------
;;;
;;; bm.el
;;;
;;; ------------------------------------------------------------
(use-package bm
  :ensure t
  :bind
  (("C-<f2>" . bm-toggle)
   ("<f2>" . bm-next)
   ("S-<f2>" . bm-previous)))

(use-package helm-bm
  :ensure t
  :bind ("C-S-<f2>" . helm-bm))

(use-package z-sudo
  :bind ("C-x C-r" . find-file-root))

;;; ------------------------------------------------------------
;;;
;;; swoop
;;;
;;; ------------------------------------------------------------
(use-package swoop
  :ensure t
  :bind
  (("C-o" . swoop)
   ("M-o" . swoop-pcre-regexp)
   ("C-S-o" . swoop-back-to-last-position)
   :map swoop-map
   ("C-o" . swoop-multi-from-swoop))
  :config
  (setq swoop-use-target-magnifier: nil)
  (setq swoop-font-size-change: nil)
  )

;;; ------------------------------------------------------------
;;;
;;; ciel
;;;
;;; ------------------------------------------------------------
(use-package ciel
  :ensure t
  :bind
  (("C-c i" . ciel-ci)
   ("C-c o" . ciel-co)))

;;; ------------------------------------------------------------
;;;
;;; markdown
;;;
;;; ------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :defer t)

;;; ------------------------------------------------------------
;;;
;;; dockerfile
;;;
;;; ------------------------------------------------------------
(use-package dockerfile-mode
  :ensure t
  :defer t)

;;; ------------------------------------------------------------
;;;
;;; terraform
;;;
;;; ------------------------------------------------------------
(use-package terraform-mode
  :ensure t
  :defer t)

(use-package embrace
  :ensure t
  :bind ("C-," . embrace-commander))

(use-package z-edit-ext
  :init
  (add-hook 'gdb-mode-hook 'kill-buffer-when-exit)
  (add-hook 'jdb-mode-hook 'kill-buffer-when-exit)
  (add-hook 'pdb-mode-hook 'kill-buffer-when-exit)
  (add-hook 'comint-mode-hook 'kill-buffer-when-exit)
  (add-hook 'shell-mode-hook 'kill-buffer-when-exit)
  (add-hook 'inferior-python-mode-hook 'kill-buffer-when-exit)
  (add-hook 'inferior-js-mode-hook 'kill-buffer-when-exit)
  (add-hook 'compilation-mode-hook 'kill-buffer-when-exit))

(provide 'init-edit)
