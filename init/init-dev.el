(use-package compile
  :bind ("<f12>" . compile))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)))

(use-package dired-k
  :ensure t)

(use-package diff-hl
  :ensure t
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config    
  (global-diff-hl-mode 1))

(use-package git-timemachine
  :ensure t
  :commands (git-timemachine git-timemachine-toggle))

(use-package realgud
  :defer t
  :ensure t)

(provide 'init-dev)
