(require-package 'info+)
(require 'info+)

(require-package 'irfc)
(setq irfc-directory (concat (bibo/get-runtimes-dir) (file-name-as-directory "RFC")))
(setq irfc-assoc-mode t)

(require-package 'xkcd)
(setq xkcd-cache-dir (concat (bibo/get-runtimes-dir) "xkcd"))
(setq xkcd-cache-latest (concat (bibo/get-runtimes-dir) "xkcd/latest"))

(provide 'init-information)
