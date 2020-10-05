(require 'org-clock)

(defun z/open-browser nil
  (interactive)
  (browse-url "http://localhost:3721"))

(defun z/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (let* ((capturer (concat "/usr/bin/python3 " user-emacs-directory "tools/capclip.py "))
         p	     (buffer-file-name-no-ext (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         (buffer-file-dir (file-name-directory (buffer-file-name)))
         (buffer-file-dir-img (concat buffer-file-dir "images/"))
         (exist-dir-img (file-accessible-directory-p buffer-file-dir-img))
         (target-dir (if (not exist-dir-img)
                         buffer-file-dir
                       buffer-file-dir-img))
         (uuid (uuidgen-4))
         (target-file
          (replace-regexp-in-string "/" "/"
                                    (concat (concat target-dir uuid) ".png") t t)))
    (if (= 0 (call-process-shell-command (concat capturer target-file) nil))
        (insert (concat "[[file:./" (if exist-dir-img "images/" "") (file-name-nondirectory target-file)  "]]"))
      (message "no images in clipboard")))
  )

(defun z/org-delete-linked-file-in-point ()
  "Delete a file in point if exists."
  (interactive)
  (let* ((context (org-element-context))
         (element-type (car context))
         (element-props (car (cdr context))))
    (when (and (eq element-type 'link)
               (string= "file" (plist-get element-props :type)))
      (let ((filename (plist-get element-props :path)))
        (if (file-exists-p filename)
	    (if (y-or-n-p (format "Delete %s ? " filename))
                (progn
                  (delete-file filename)
                  (message (format "File %s deleted" filename))
                  (let ((beg (org-element-property :begin context))
                        (end (org-element-property :end context)))
                    (delete-region beg end))))
          (message "file not existing")
          ))))
  )

(defvar z/org-clock-in-todo-keywords '("DOING" "READING")
  "the todo keywords when changed into, trigger clock in")

(defvar z/org-clock-out-todo-keywords '("WAITING" "CONTINUE" "TODO" "TOREAD" "BREAK")
  "the todo keywords when changed into, trigger clock out")

(defun z/org-clock-in-if-todo-keywords ()
       "Clock in when the task is marked as z/org-clock-in-todo-keywords"
       (let* ((ele (org-element-at-point))
              (state (substring-no-properties (or (org-element-property :todo-keyword ele) ""))))
         (when (and (member state z/org-clock-in-todo-keywords)
                    (not (org-clocking-p)))
	 (org-clock-in)))
       )

(defun z/org-clock-out-if-todo-keywords ()
      "Clock out when the task is marked as z/org-clock-out-todo-keywords"
      (let* ((state (or (substring-no-properties (or (org-element-property :todo-keyword (org-element-at-point)) "")))))
        (when (and (org-clocking-p)
               (member state z/org-clock-out-todo-keywords)
               (equal (marker-buffer org-clock-marker) (current-buffer))
               (< (point) org-clock-marker)
               (> (save-excursion (outline-next-heading) (point))
                  org-clock-marker)
               )
          (org-clock-out)))
      )


(provide 'z-org-ext)
