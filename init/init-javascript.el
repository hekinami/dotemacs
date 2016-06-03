(require-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2")))

(require-package 'tern)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(require-package 'tern-auto-complete)
(add-hook 'js2-mode-hook 'auto-complete-mode)
(add-hook 'js2-mode-hook 'tern-ac-setup)

(require-package 'js-comint)
(setenv "NODE_NO_READLINE" "1")		;http://stackoverflow.com/questions/9390770/node-js-prompt-can-not-show-in-eshell
(setq inferior-js-program-command "node")

(add-hook 'js2-mode-hook '(lambda () 
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

(require-package 'skewer-mode)

(require-package 'json-mode)

(provide 'init-javascript)
