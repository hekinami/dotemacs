(require-package 'jedi)

(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda ()
                              (setq ac-sources (append ac-sources '(ac-source-yasnippet)))))

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)
(setq python-indent-guess-indent-offset nil)

(require-package 'traad)
(setq traad-server-program "traad")
(setq traad-auto-revert t)

(provide 'init-python)
