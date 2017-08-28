;;; ------------------------------------------------------------
;;;
;;; javascript
;;;
;;; ------------------------------------------------------------
(require-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2")))

(require-package 'tern)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(require-package 'tern-auto-complete)
(add-hook 'js2-mode-hook 'auto-complete-mode)
(add-hook 'js2-mode-hook 'tern-ac-setup)

(require-package 'js-comint)
(setenv "NODE_NO_READLINE" "1")		;http://stackoverflow.com/questions/9390770/node-js-prompt-can-not-show-in-eshell
(setq inferior-js-program-command "node")

(add-hook 'js2-mode-hook '(lambda () 
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

(require-package 'skewer-mode)
(require-package 'json-mode)

;;; ------------------------------------------------------------
;;;
;;; lisp
;;;
;;; ------------------------------------------------------------

(require-package 'paredit)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (show-paren-mode 1)
                                  (turn-on-eldoc-mode)
                                  (paredit-mode)
                                  (auto-complete-mode)
                                  (local-set-key (kbd "C-c s") 'elisp-index-search)))

(add-hook 'lisp-interaction-mode-hook (lambda ()
                                        (add-to-list 'ac-sources 'ac-source-variables)
                                        (add-to-list 'ac-sources 'ac-source-symbols)
                                        (add-to-list 'ac-sources 'ac-source-functions)
                                        (add-to-list 'ac-sources 'ac-source-features)
                                        (auto-complete-mode)
                                        ))
(add-hook 'ielm-mode-hook (lambda ()
                            (add-to-list 'ac-sources 'ac-source-variables)
                            (add-to-list 'ac-sources 'ac-source-symbols)
                            (add-to-list 'ac-sources 'ac-source-functions)
                            (add-to-list 'ac-sources 'ac-source-features)
                            (auto-complete-mode)
                            (eldoc-mode)
                            ))

;;; ------------------------------------------------------------
;;;
;;; python
;;;
;;; ------------------------------------------------------------
(require-package 'jedi)

(setq python-environment-directory (concat (bibo/get-runtimes-dir) ".python-environments"))
(setq jedi:environment-root "py3jedi")
(setq jedi:environment-virtualenv '("virtualenv" "--system-site-packages" "--always-copy" "--quiet"))
(when *is-linux*
  (setq jedi:environment-virtualenv '("virtualenv" "--system-site-packages" "-p" "python3" "--always-copy" "--quiet"))
  )
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)
(setq python-indent-guess-indent-offset nil)

(add-hook 'python-mode-hook (lambda ()
                              (jedi:setup)
                              (yas-minor-mode)
                              (setq ac-sources (append ac-sources '(ac-source-yasnippet)))))


(require-package 'traad)
;; https://github.com/abingham/emacs-traad/issues/6
(setq venv-location (concat (bibo/get-runtimes-dir) ".python-environments"))
(setq traad-environment-name "py3traad")
(setq traad-server-program "traad")
(setq traad-auto-revert t)

(require-package 'python-django)
(require 'python-django)
(global-set-key (kbd "C-x j") 'python-django-open-project)

;;; ------------------------------------------------------------
;;;
;;; rust
;;;
;;; ------------------------------------------------------------
(require-package 'rust-mode)
(require-package 'racer)
(require-package 'ac-racer)

;;; set racer-rust-src-path, racer-cmd in custom.el

(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (if (not (string-match "rust" compile-command))
                                (set (make-local-variable 'compile-command)
                                     "cargo run"))
                            ))

(add-hook 'racer-mode-hook (lambda ()
                             (eldoc-mode)
                             (ac-racer-setup)
                             ;; workaround to prevent completion menu open after type space
                             (ac-define-source racer
                               '((prefix . ac-racer--prefix)
                                 (candidates . ac-racer--candidates)
                                 (requires . 1)))
                             ))

(require-package 'toml-mode)
(add-to-list 'auto-mode-alist '("Cargo.lock\\'" . toml-mode))

;;; ------------------------------------------------------------
;;;
;;; ruby
;;;
;;; ------------------------------------------------------------
(require-package 'robe)
(add-hook 'robe-mode-hook 'ac-robe-setup)
(add-hook 'ruby-mode-hook 'robe-mode)

;;; ------------------------------------------------------------
;;;
;;; go
;;;
;;; ------------------------------------------------------------
;;; configuration based on http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;;; go get github.com/rogpeppe/godef
;;; go get -u github.com/nsf/gocode

(require-package 'go-mode)
(require-package 'go-autocomplete)
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

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
            ))

;;; ------------------------------------------------------------
;;;
;;; yaml
;;;
;;; ------------------------------------------------------------
(require-package 'yaml-mode)

(provide 'init-languages)
