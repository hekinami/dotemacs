(require-package 'jedi)

(setq python-environment-directory (concat (bibo/get-runtimes-dir) ".python-environments"))
(setq jedi:environment-root "python3dev")
(setq jedi:environment-virtualenv '("virtualenv" "--system-site-packages" "--always-copy" "--quiet"))
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)
(setq python-indent-guess-indent-offset nil)

(add-hook 'python-mode-hook (lambda ()
                              (jedi:setup)
                              (yas-minor-mode)
                              (setq ac-sources (append ac-sources '(ac-source-yasnippet)))))


(require-package 'traad)
(setq traad-server-program "traad")
(setq traad-auto-revert t)

(require-package 'python-django)
(require 'python-django)
(global-set-key (kbd "C-x j") 'python-django-open-project)

(provide 'init-python)
