(global-set-key (kbd "<f12>") 'compile)

(require-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(require-package 'diff-hl)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(global-diff-hl-mode 1)

(provide 'init-development)
