;;; customization group for z's
(defgroup z nil
  "z's customization."
  :group 'emacs)

;;; performance
(defun z/time-difference-in-millis (start end)
  (* 1000.0 (float-time (time-subtract end start))))

(defvar z/feature-loading-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require (around z/record-feature-loading-times (feature &optional filename noerror) activate)
  "Note in `z/feature-loading-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (z/time-difference-in-millis require-start-time (current-time) )))
          (add-to-list 'z/feature-loading-times
                       (cons feature time)
                       t))))))

(defun locate-runtimes-file (file)
  (let ((base (locate-user-emacs-file "runtimes/")))
    (make-directory base t)
    (concat base file)))

(defun locate-contents-file (file)
  (let ((base (locate-user-emacs-file "contents/")))
    (make-directory base t)
    (concat base file)))

(defun locate-stuff-file (file)
  (let ((base (locate-user-emacs-file "stuff/")))
    (make-directory base t)
    (concat base file)))

(defun locate-tools-file (file)
  (let ((base (locate-user-emacs-file "tools/")))
    (make-directory base t)
    (concat base file)))

(require 'cl)
(require 'cl-lib)

;;; ------------------------------------------------------------
;;;
;;; package management infrastructure
;;;
;;; ------------------------------------------------------------
(setq package-enable-at-startup nil)
(package-initialize)

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

(use-package quelpa-use-package
  :ensure t
  :init
  (setq quelpa-update-melpa-p nil)) 

(use-package try
  :defer t
  :ensure t)

(provide 'init-prepare)
