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

(provide 'init-info)
