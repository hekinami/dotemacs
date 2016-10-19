(require-package 'info+)
(require 'info+)

(require-package 'irfc)
(setq irfc-directory (concat (bibo/get-runtimes-dir) (file-name-as-directory "RFC")))
(setq irfc-assoc-mode t)

(require-package 'xkcd)
(setq xkcd-cache-dir (concat (bibo/get-runtimes-dir) "xkcd"))
(setq xkcd-cache-latest (concat (bibo/get-runtimes-dir) "xkcd/latest"))

;;; ------------------------------------------------------------
;;;
;;; xwidget webkit
;;;
;;; ------------------------------------------------------------
(require 'xwidget)
(define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
(define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)

(provide 'init-information)
