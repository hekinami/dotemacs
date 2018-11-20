(use-package compile
  :bind ("<f12>" . compile))

(use-package magit
  :ensure t
  :init
  (setq auto-revert-check-vc-info t)
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

(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'git-radar))

(use-package git-messenger
  :ensure t
  :bind ("C-x v p" . git-messenger:popup-message))

(use-package git-timemachine
  :ensure t
  :commands (git-timemachine git-timemachine-toggle))

(use-package realgud
  :defer t
  :ensure t)

(use-package edbi
  :defer t
  :ensure t
  :config
  (setq edbi:query-result-fix-header nil)
  (setq edbi:ds-history-file (locate-runtimes-file ".edbi-ds-history")))

(use-package edbi-database-url
  :ensure t
  :commands (edbi-database-url))

(provide 'init-dev)
