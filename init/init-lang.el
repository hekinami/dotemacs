;;; ------------------------------------------------------------
;;;
;;; javascript
;;;
;;; ------------------------------------------------------------
(use-package js2-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2"))))

(use-package tern
  :ensure t
  :defer t
  :init
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  :after js2-mode)

(use-package tern-auto-complete
  :ensure t
  :defer
  :init
  (add-hook 'js2-mode-hook 'auto-complete-mode)
  (add-hook 'js2-mode-hook 'tern-ac-setup)
  :after (js2-mode tern))

(use-package js-comint
  :ensure t
  :defer t
  :init
  (setenv "NODE_NO_READLINE" "1")		;http://stackoverflow.com/questions/9390770/node-js-prompt-can-not-show-in-eshell
  :config
  (setq inferior-js-program-command "node")

  (add-hook
   'js2-mode-hook
   '(lambda () 
      (local-set-key "\C-x\C-e" 'js-send-last-sexp)
      (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
      (local-set-key "\C-cb" 'js-send-buffer)
      (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
      (local-set-key "\C-cl" 'js-load-file-and-go)
      ))  
  )

(use-package indium
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'indium-interaction-mode))

(use-package skewer-mode
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

;;; ------------------------------------------------------------
;;;
;;; lisp
;;;
;;; ------------------------------------------------------------
(use-package paredit
  :ensure t
  :defer t)

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (show-paren-mode 1)
   (turn-on-eldoc-mode)
   (paredit-mode)
   (auto-complete-mode)
   (local-set-key (kbd "C-c s") 'elisp-index-search)))

(add-hook
 'lisp-interaction-mode-hook
 (lambda ()
   (add-to-list 'ac-sources 'ac-source-variables)
   (add-to-list 'ac-sources 'ac-source-symbols)
   (add-to-list 'ac-sources 'ac-source-functions)
   (add-to-list 'ac-sources 'ac-source-features)
   (auto-complete-mode)
   ))

(add-hook
 'ielm-mode-hook
 (lambda ()
   (add-to-list 'ac-sources 'ac-source-variables)
   (add-to-list 'ac-sources 'ac-source-symbols)
   (add-to-list 'ac-sources 'ac-source-functions)
   (add-to-list 'ac-sources 'ac-source-features)
   (auto-complete-mode)
   (eldoc-mode)
   ))

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy)))

;;; ------------------------------------------------------------
;;;
;;; python
;;;
;;; ------------------------------------------------------------
(setq python-environment-directory (concat (z/get-runtimes-dir) ".python-environments"))
(setq python-indent-guess-indent-offset nil)
(require-package 'jedi)
(setq jedi:environment-root "py3jedi")
(setq jedi:environment-virtualenv '("virtualenv" "--system-site-packages" "--always-copy" "--quiet"))
(when *is-linux*
  (setq jedi:environment-virtualenv '("virtualenv" "--system-site-packages" "-p" "python3" "--always-copy" "--quiet"))
  )
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)
(add-hook 'python-mode-hook (lambda ()
                              (jedi:setup)
                              (yas-minor-mode)
                              (setq ac-sources (append ac-sources '(ac-source-yasnippet)))))

(use-package python-django
  :ensure t
  :bind ("C-x j" . python-django-open-project))

;;; ------------------------------------------------------------
;;;
;;; rust
;;;
;;; ------------------------------------------------------------
(use-package rust-mode
  :ensure t
  :defer t)

(use-package racer
  :ensure t
  :init
  ;;; set racer-rust-src-path, racer-cmd in custom.el

  (add-hook 'rust-mode-hook (lambda ()
                              (racer-mode)
                              (if (not (string-match "rust" compile-command))
                                  (set (make-local-variable 'compile-command)
                                       "cargo run"))
                              ))
  :after rust-mode)

(use-package ac-racer
  :ensure t
  :init
  (add-hook 'racer-mode-hook (lambda ()
                               (eldoc-mode)
                               (ac-racer-setup)
                               ;; workaround to prevent completion menu open after type space
                               (ac-define-source racer
                                 '((prefix . ac-racer--prefix)
                                   (candidates . ac-racer--candidates)
                                   (requires . 1)))
                               ))
  :after (rust-mode racer))

(use-package rust-playground
  :ensure t
  :config
  (setq rust-playground-basedir (concat (z/get-contents-dir) "rust-playground") ))

(use-package toml-mode
  :ensure t
  :defer t
  :mode ("Cargo.lock\\'" . toml-mode))

;;; ------------------------------------------------------------
;;;
;;; ruby
;;;
;;; ------------------------------------------------------------
(use-package robe
  :ensure t
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  :config
  (add-hook 'robe-mode-hook 'ac-robe-setup))

;;; ------------------------------------------------------------
;;;
;;; go
;;;
;;; ------------------------------------------------------------
;;; configuration based on http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;;; go get github.com/rogpeppe/godef
;;; go get -u github.com/nsf/gocode
(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq standard-indent 4)
              (setq indent-tabs-mode nil)
              (local-set-key (kbd "C-c .") 'godef-jump)
              (local-set-key (kbd "C-c ,") 'pop-tag-mark)
              (auto-complete-mode 1)
              (if (not (string-match "go" compile-command))
                  (set (make-local-variable 'compile-command)
                       "go build -v && go test -v && go vet"))
              )))

(use-package go-autocomplete
  :ensure t
  :after go-mode)

;;; ------------------------------------------------------------
;;;
;;; yaml
;;;
;;; ------------------------------------------------------------
(use-package yaml-mode
  :ensure t
  :defer t)

(use-package solidity-mode
  :ensure t
  :mode ("\\.sol\\'" . solidity-mode)
  :config
  (setq c-basic-offset 4))

(provide 'init-lang)
