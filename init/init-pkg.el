(require 'package)
(setq package-enable-at-startup nil)
;; (package-initialize)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(setq package-archives
      '(("gnu-cn" . "http://elpa.emacs-china.org/gnu/")
        ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
        ("melpa-stable-cn" . "http://elpa.emacs-china.org/melpa-stable/")
        ("marmalade-cn" . "http://elpa.emacs-china.org/marmalade/")
        ("org-cn" . "http://elpa.emacs-china.org/org/")
        ("sunrise-cn" . "http://elpa.emacs-china.org/sunrise-commander/")
        ("user42-cn" . "http://elpa.emacs-china.org/user42/")
	("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	)
      )

(setq ad-redefinition-action 'accept)   ;https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/

(require-package 'use-package)

(use-package try
  :defer t
  :ensure t)

(provide 'init-pkg)
