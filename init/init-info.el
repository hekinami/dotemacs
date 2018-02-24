(use-package info+
  :ensure t
  :defer t)

(use-package irfc
  :ensure t
  :defer t
  :config
  (setq irfc-directory (concat (z/get-runtimes-dir) (file-name-as-directory "RFC")))
  (setq irfc-assoc-mode t))

(use-package xkcd
  :ensure t
  :defer t
  :config
  (setq xkcd-cache-dir (concat (z/get-runtimes-dir) "xkcd"))
  (setq xkcd-cache-latest (concat (z/get-runtimes-dir) "xkcd/latest")))

(use-package hackernews
  :ensure t
  :commands (hackernews))

(use-package elfeed
  :ensure t
  :commands (elfeed)
  :config
  (use-package elfeed-org
    :ensure t
    :config
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))
  (use-package elfeed-goodies
    :ensure t
    :config
    (elfeed-goodies/setup)))

;;; ------------------------------------------------------------
;;;
;;; xwidget webkit
;;;
;;; ------------------------------------------------------------
(use-package xwidget
  :bind
  (:map xwidget-webkit-mode-map
        ("<mouse-5>" . xwidget-webkit-scroll-up)
        ("<mouse-4>" . xwidget-webkit-scroll-down)))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat (z/get-runtimes-dir) "nov-places")))

(provide 'init-info)
