(require-package 'info+)
(require 'info+)

(require-package 'irfc)
(setq irfc-directory (concat (bibo/get-runtimes-dir) (file-name-as-directory "RFC")))
(setq irfc-assoc-mode t)

(provide 'init-information)
