(require-package 'ac-html)
(require-package 'web-mode)
(require-package 'emmet-mode)

(setq sgml-basic-offset 4)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("layout.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.swig\\'" . web-mode))

(setq web-mode-engines-alist
      '(("django" . "\\.swig\\'")
        ("django" . "\\.djhtml\\'")
	)
      )

(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-markup-indent-offset 4
                                 web-mode-css-indent-offset 4
                                 web-mode-code-indent-offset 4
                                 web-mode-indent-style 2
                                 web-mode-style-padding 1
                                 web-mode-script-padding 1
                                 web-mode-block-padding 0
                                 web-mode-comment-style 2
                                 web-mode-enable-auto-pairing nil)
                           (setq-local
                            electric-pair-pairs
                            (append electric-pair-pairs '((?% . ?%))))
                           (setq web-mode-enable-current-column-highlight t)
                           (setq web-mode-enable-current-element-highlight t)
                           (emmet-mode)
                           (setq emmet-preview-default t)
                           (auto-complete-mode)
                           (require 'ac-html)
                           (add-to-list 'web-mode-ac-sources-alist
                                        '("html" . (
                                                    ;; attribute-value better to be first
                                                    ac-source-html-attribute-value
                                                    ac-source-html-tag
                                                    ac-source-html-attribute)))

                           (add-to-list 'web-mode-ac-sources-alist
                                        '("css" . (ac-source-css-property)))
                           ))

(add-hook 'css-mode-hook 'emmet-mode)
(require-package 'less-css-mode)
(require-package 'sass-mode)
(require-package 'scss-mode)

(require-package 'impatient-mode)
(require-package 'restclient)

(require-package 'php-mode)
(require-package 'geben)
(setq geben-pause-at-entry-line nil)
(setq geben-display-window-function 'pop-to-buffer-same-window)
(setq geben-temporary-file-directory (concat (bibo/get-runtimes-dir) "geben"))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

(require-package 'web-beautify)
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(provide 'init-web)
