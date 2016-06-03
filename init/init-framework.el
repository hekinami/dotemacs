(require-package 'which-key)
(setq httpd-port 3721)
(setq httpd-root (concat (bibo/get-contents-dir) (file-name-as-directory "orgp")))

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

(provide 'init-framework)
