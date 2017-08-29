(use-package info+
  :defer t
  :init
  (require-package 'info+))

(use-package irfc
  :defer t
  :init
  (require-package 'irfc)
  :config
  (setq irfc-directory (concat (bibo/get-runtimes-dir) (file-name-as-directory "RFC")))
  (setq irfc-assoc-mode t))

(use-package xkcd
  :defer t
  :init
  (require-package 'xkcd)
  :config
  (setq xkcd-cache-dir (concat (bibo/get-runtimes-dir) "xkcd"))
  (setq xkcd-cache-latest (concat (bibo/get-runtimes-dir) "xkcd/latest")))

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

(provide 'init-information)
