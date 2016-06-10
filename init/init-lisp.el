(require-package 'paredit)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (show-paren-mode 1)
                                  (turn-on-eldoc-mode)
                                  (paredit-mode)
                                  (setq skeleton-pair t)
                                  (mapcar (lambda (token)
                                            (local-set-key token 'skeleton-pair-insert-maybe))
                                          '("(" "[" "{" "\""))
                                  (local-set-key (kbd "C-c s") 'elisp-index-search)))

(provide 'init-lisp)
