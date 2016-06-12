(require-package 'rust-mode)
(require-package 'racer)
(require-package 'ac-racer)

;;; set racer-rust-src-path, racer-cmd in custom.el

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'ac-racer-setup)
(add-hook 'racer-mode-hook #'eldoc-mode)

(require-package 'toml-mode)

(provide 'init-rust)
