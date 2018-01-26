(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; ------------------------------------------------------------
;;;
;;; auto-complete
;;;
;;; ------------------------------------------------------------
(use-package auto-complete
  :ensure t
  :bind
  (:map ac-completing-map
        ("M-/" . ac-stop))
  :config
  (ac-linum-workaround)
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (concat (z/get-contents-dir) (file-name-as-directory "ac-dict")))
  (setq ac-comphist-file (concat (z/get-runtimes-dir) "ac-comphist.dat")) 
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
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil)
        ("<backtab>" . yas-expand))
  :config
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
  :init
  (global-unset-key (kbd "C-x c"))
  :bind
  (("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("C-x r l" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files))
  :init
  (setq bookmark-file (concat (z/get-runtimes-dir) "bookmarks")) ; must be set before enable helm-mode
  :config
  (require 'helm-config)
  (add-hook
   'helm-minibuffer-set-up-hook
   (lambda ()
     (set-face-attribute 'helm-selection nil :background (face-attribute 'hl-line :background))
     (set-face-attribute 'helm-source-header nil :background nil)
     (set-face-attribute 'helm-match nil :foreground (face-attribute 'font-lock-constant-face :foreground))
     (global-set-key (kbd "C-x b") #'persp-switch-to-buffer) ; workaround for overwriting persp-mode key binding
     ))
  (helm-mode 1))

(provide 'init-fmwk)
