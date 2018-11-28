(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;;; ------------------------------------------------------------
;;;
;;; auto-complete
;;;
;;; ------------------------------------------------------------
(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :bind
  (:map ac-completing-map
        ("M-/" . ac-stop))
  :config
  (ac-linum-workaround)
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (locate-contents-file "ac-dict"))
  (setq ac-comphist-file (locate-runtimes-file "ac-comphist.dat")) 
  (ac-config-default)
  (global-auto-complete-mode t)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))

;;; ------------------------------------------------------------
;;;
;;; yasnippet
;;;
;;; ------------------------------------------------------------
(add-hook 'after-init-hook 'yas-global-mode)
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil)
        ("<backtab>" . yas-expand))
  :config
  (use-package yasnippet-snippets
    :defer t
    :ensure t)
  (setq yas-also-auto-indent-first-line t)
  (setq yas-prompt-functions
        '(yas-ido-prompt
          yas-completing-prompt
          yas-x-prompt yas-dropdown-prompt yas-no-prompt))
  (setq yas-triggers-in-field t)
  (yas-reload-all))

;;; ------------------------------------------------------------
;;;
;;; helm
;;;
;;; ------------------------------------------------------------
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (global-unset-key (kbd "C-x c"))
  :bind
  (("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("C-x r l" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files))
  :init
  (setq bookmark-file (locate-runtimes-file "bookmarks")) ; must be set before enable helm-mode
  :config
  (require 'helm-config)
  (add-hook
   'helm-minibuffer-set-up-hook
   (lambda ()
     (set-face-attribute 'helm-selection nil :background (face-attribute 'hl-line :background))
     (set-face-attribute 'helm-source-header nil :background nil)
     (set-face-attribute 'helm-match nil :foreground (face-attribute 'font-lock-constant-face :foreground))
     ))
  (helm-mode 1))

;;; ------------------------------------------------------------
;;;
;;; projectile
;;;
;;; ------------------------------------------------------------
(use-package projectile
  :ensure t
  :bind ("C-x C-b" . helm-projectile-switch-to-buffer)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-known-projects-file (locate-runtimes-file "projectile-bookmarks.eld"))
  (setq projectile-mode-line-prefix "")
  (projectile-global-mode)
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  :after
  projectile)

(provide 'init-framework)
