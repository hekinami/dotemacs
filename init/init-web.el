(use-package ac-html
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("layout.*\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.swig\\'" . web-mode))
  :config
  (setq sgml-basic-offset 4)
  (setq web-mode-engines-alist
        '(("django" . "\\.swig\\'")
          ("django" . "\\.djhtml\\'")))
  ;;; redefine the django engine keywords with new ones
  (setq web-mode-django-keywords
        (regexp-opt
         '("and" "as" "assign"
           "break" "cache" "call" "case" "context" "continue"
           "do" "flush" "from" "ignore" "import" "in" "is"
           "layout" "load" "missing" "none" "not" "or" "pluralize"
           "random" "set" "unless" "use" "var"
           "with"                         ; new added
           )))
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-indent-style 2
        web-mode-style-padding 1
        web-mode-script-padding 1
        web-mode-block-padding 0
        web-mode-comment-style 2
        web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (add-hook
   'web-mode-hook
   (lambda ()
     (setq-local
      electric-pair-pairs
      (append electric-pair-pairs '((?% . ?%))))
     (emmet-mode)
     (setq emmet-preview-default t)
     (auto-complete-mode)
     (require 'ac-html)
     (add-to-list
      'web-mode-ac-sources-alist
      '("html" . (
                  ;; attribute-value better to be first
                  ac-source-html-attribute-value
                  ac-source-html-tag
                  ac-source-html-attribute)))

     (add-to-list
      'web-mode-ac-sources-alist
      '("css" . (ac-source-css-property)))
     )))

(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package less-css-mode
  :ensure t
  :defer t)

(use-package sass-mode
  :ensure t
  :defer t)

(use-package scss-mode
  :ensure t
  :defer t)

(use-package impatient-mode
  :ensure t
  :defer t)

(use-package restclient
  :ensure t
  :defer t
  :config
  (defun restclient nil
    (interactive)
    (switch-to-buffer (generate-new-buffer "*restclient*"))
    (restclient-mode))
  )

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

(use-package geben
  :ensure t
  :config
  (setq geben-pause-at-entry-line nil)
  (setq geben-display-window-function 'pop-to-buffer-same-window)
  (setq geben-temporary-file-directory (locate-runtimes-file "geben")))

(use-package web-beautify
  :ensure t
  :bind (:map
         js2-mode-map
         ("C-c b" . web-beautify-js)
         :map
         json-mode-map
         ("C-c b" . web-beautify-js)
         :map
         css-mode-map
         ("C-c b" . web-beautify-css)
         :map
         html-mode-map
         ("C-c b" . web-beautify-html))
  :after (js2-mode json-mode css-mode sgml-mode))

(use-package apib-mode
  :ensure t
  :defer t
  :mode ("\\.apib\\'" . apib-mode))

(provide 'init-web)
