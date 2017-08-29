(global-set-key (kbd "<f12>") 'compile)

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup))
  :init
  (require-package 'magit)
  :config
  (use-package diff-hl
    :init
    (require-package 'diff-hl)
    :config
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (global-diff-hl-mode 1)))

(use-package realgud
  :defer t
  :init
  (require-package 'realgud))

(provide 'init-development)
