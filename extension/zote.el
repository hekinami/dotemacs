(require 'f)
(require 'helm)

(defcustom zote-source-dir "./zote"
  "zote notebook source directory")

(defcustom zote-target-dir "./zote-published"
  "zote notebook published direcotry")

(defcustom zote-theme "default"
  "the org theme for zote publish")

(defun zote-generate-volume-oppa (volume-name source-base-dir target-base-dir)
  (let ((target-dir (f-join target-base-dir "volumes" volume-name))
        (source-dir (f-join source-base-dir "volumes" volume-name))
        (html-head (concat "<link rel=\"stylesheet\" href=\"../../styles/normalize.css\" />\n"
                           "<link rel=\"stylesheet\" href=\"../../styles/default.css\" />\n"
                           "<link rel=\"stylesheet\" href=\"../../styles/styles.css\" />\n"
                           "<script src=\"../../scripts/script.js\"></script>\n")))
    (list (list (format "volume-%s-assets" volume-name)
                :base-directory source-dir
                :base-extension "css\\|js\\|png"
                :publishing-directory target-dir
                :publishing-function 'org-publish-attachment)
          (list (format "volume-%s-pages" volume-name)
                :base-directory source-dir
                :base-extension "org"
                :publishing-directory target-dir
                :publishing-function 'org-html-publish-to-html
                :html-head html-head)
          (list (format "volume-%s" volume-name)
                :components (list (format "volume-%s-assets" volume-name)
                                  (format "volume-%s-pages" volume-name)))))
  )

(defun zote-generate-notebook-oppa (notebook-source-dir notebook-target-dir theme)
  "Generate value of org-publish-project-alist"
  (let* ((volume-dirs (f-directories (f-join notebook-source-dir "volumes")))
         (volume-names (mapcar #'file-name-base volume-dirs))
         (html-head (concat "<link rel=\"stylesheet\" href=\"./styles/normalize.css\" />\n"
                            "<link rel=\"stylesheet\" href=\"./styles/default.css\" />\n"
                            "<link rel=\"stylesheet\" href=\"./styles/styles.css\" />\n"
                            "<script src=\"./scripts/script.js\"></script>\n"))
         (volume-project-alist (reduce #'append
                                       (mapcar (lambda (volume)
                                                 (zote-generate-volume-oppa volume notebook-source-dir notebook-target-dir))
                                               volume-names))))
    (append volume-project-alist (list (list "notebook-theme"
                                             :base-directory (f-join org-tpl-directory theme)
                                             :base-extension "css\\|js\\png"
                                             :publishing-directory notebook-target-dir
                                             :publishing-function 'org-publish-attachment
                                             :recursive t)
                                       (list "notebook-pages"
                                             :base-directory notebook-source-dir
                                             :base-extension "org"
                                             :publishing-directory notebook-target-dir
                                             :publishing-function 'org-html-publish-to-html
                                             :html-head html-head)
                                       (list "notebook"
                                             :components (append (mapcar (lambda (volume)
                                                                           (format "volume-%s" volume))
                                                                         volume-names)
                                                                 '("notebook-theme" "notebook-pages")))))
    )
  )

(defun zote--get-volume-list ()
  (let* ((volume-dirs (f-directories (f-join zote-source-dir "volumes")))
         (volume-names (mapcar #'file-name-base volume-dirs)))
    volume-names))

(defun zote-publish ()
  (interactive)
  (let ((org-publish-project-alist (zote-generate-notebook-oppa zote-source-dir zote-target-dir zote-theme)))
    (org-publish-project "notebook" t)))

(defun zote-volume-add ()
  (interactive)
  (let* ((volume-name (read-string "volume name: "))
         (target (f-join zote-source-dir "volumes" volume-name)))
    (if (f-exists-p target)
        (message "already exists")
      (f-mkdir target)
      (f-mkdir (f-join target "images"))
      (f-touch (f-join target "index.org"))
      (message (format "volume %s added" volume-name)))))

(defun zote-volume-delete ()
  (interactive)
  (let* ((volume-name (helm-comp-read "volume name: " (zote--get-volume-list)))
         (target (f-join zote-source-dir "volumes" volume-name)))
    (if (not (f-exists-p target))
        (message "no such volume")
      (when (yes-or-no-p "delete?")
        (f-delete target t)
        (message (format "volume %s deleted" volume-name))))))

(defun zote-volume-view ()
  (interactive)
  (let ((volume-name (helm-comp-read "volume name: " (zote--get-volume-list))))
    (browse-url (format "http://127.0.0.1:%d/volumes/%s" httpd-port volume-name))))

(defun zote-volume-edit-1 ()
  (interactive)
  (let ((volume-name (helm-comp-read "volume name: " (zote--get-volume-list))))
    (find-file (f-join zote-source-dir "volumes" volume-name "index.org"))))

(defun zote-volume-edit ()
  (interactive)
  (let ((volume-name (helm-comp-read "volume name: " (zote--get-volume-list))))
    (find-file (helm-comp-read "select one file: " (f-files (f-join zote-source-dir "volumes" volume-name))))))

(provide 'zote)
