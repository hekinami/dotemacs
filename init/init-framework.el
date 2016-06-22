(require-package 'which-key)
(which-key-mode)

;;; ------------------------------------------------------------
;;;
;;; auto-complete
;;;
;;; ------------------------------------------------------------
(require-package 'auto-complete)
(require 'auto-complete)
(ac-linum-workaround)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat (bibo/get-contents-dir) (file-name-as-directory "ac-dict")))
(setq ac-comphist-file (concat (bibo/get-runtimes-dir) "ac-comphist.dat")) 
(ac-config-default)
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(add-hook 'auto-complete-mode-hook (lambda ()
                                     (define-key ac-completing-map "\M-/" 'ac-stop)
                                     ))

;;; ------------------------------------------------------------
;;;
;;; yasnippet
;;;
;;; ------------------------------------------------------------
(require-package 'yasnippet)
(yas-global-mode)
(yas-reload-all)
;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
(setq yas-also-auto-indent-first-line t)
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt yas-x-prompt yas-dropdown-prompt yas-no-prompt))
(setq yas-triggers-in-field t)

;;; ------------------------------------------------------------
;;;
;;; helm
;;;
;;; ------------------------------------------------------------
(require-package 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)

(add-hook 'helm-minibuffer-set-up-hook (lambda ()
					 (set-face-attribute 'helm-selection nil :background (face-attribute 'hl-line :background))
					 (set-face-attribute 'helm-source-header nil :background nil)
					 (set-face-attribute 'helm-match nil :foreground (face-attribute 'font-lock-constant-face :foreground))
					 ))

(setq bookmark-file (concat (bibo/get-runtimes-dir) "bookmarks")) ; must be set before enable helm-mode
(global-set-key (kbd "C-x r l") 'helm-filtered-bookmarks)

(helm-mode 1)

(provide 'init-framework)
