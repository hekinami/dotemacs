;;; ------------------------------------------------------------
;;;
;;; sudo
;;;
;;; ------------------------------------------------------------

;; based on
;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
;; https://www.emacswiki.org/emacs/TrampMode#toc14
(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!"))
    (setq header-line-format
	  (propertize warning 'face 'find-file-root-header-face))))

(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root (&optional arg)
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive "P")
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
	 ;; use a separate history list for "root" files.
	 (file-name-history find-file-root-history)
	 (name (or buffer-file-name default-directory))
	 (tramp (and (tramp-tramp-file-p name)
		     (tramp-dissect-file-name name)))
	 path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
	    dir (file-name-directory path)))

    (if arg
        (progn
          (find-alternate-file (concat find-file-root-prefix buffer-file-name))
          (setq find-file-root-history file-name-history)
          (run-hooks 'find-file-root-hook)
          )
      (when (setq file (read-file-name "find file (UID = 0): " dir path))
        (find-file (concat find-file-root-prefix file))
        ;; If this all succeeded save our new history list.
        (setq find-file-root-history file-name-history)
        ;; allow some user customization
        (run-hooks 'find-file-root-hook))
      )
    ))

(add-hook 'find-file-root-hook 'find-file-root-header-warning)

(provide 'z-sudo)
