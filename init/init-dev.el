(use-package compile
  :bind ("<f12>" . compile))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)))

(use-package diff-hl
  :ensure t
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config    
  (global-diff-hl-mode 1))

(use-package realgud
  :defer t
  :ensure t)

(provide 'init-dev)
