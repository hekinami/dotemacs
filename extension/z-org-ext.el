(defun z/open-browser nil
  (interactive)
  (browse-url "http://localhost:3721"))

(defun z/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (when *is-windows*
    (let* ((capturer "\"C:\\Program Files (x86)\\IrfanView\\i_view32.exe\" /clippaste /convert ")
           (buffer-file-name-no-ext (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (buffer-file-dir (file-name-directory (buffer-file-name)))
           (buffer-file-dir-img (concat buffer-file-dir "images/"))
           (exist-dir-img (file-accessible-directory-p buffer-file-dir-img))
           (target-dir (if (not exist-dir-img)
                           buffer-file-dir
                         buffer-file-dir-img))
           (target-file
            (replace-regexp-in-string "/" "\\"
                                      (concat (concat target-dir (uuidgen-4)) ".png") t t)))
      (call-process-shell-command (concat capturer target-file) nil nil nil)
      (insert (concat "[[file:./" (if exist-dir-img "images/" "") (file-name-nondirectory target-file)  "]]")))
    )
  (when *is-linux*
    (let* ((capturer (concat "python " user-emacs-directory "tools/capclip.py "))
           p	     (buffer-file-name-no-ext (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (buffer-file-dir (file-name-directory (buffer-file-name)))
           (buffer-file-dir-img (concat buffer-file-dir "images/"))
           (exist-dir-img (file-accessible-directory-p buffer-file-dir-img))
           (target-dir (if (not exist-dir-img)
                           buffer-file-dir
                         buffer-file-dir-img))
           (target-file
            (replace-regexp-in-string "/" "/"
                                      (concat (concat target-dir (uuidgen-4)) ".png") t t)))
      (if (= 0 (call-process-shell-command (concat capturer target-file) nil))
          (insert (concat "[[file:./" (if exist-dir-img "images/" "") (file-name-nondirectory target-file)  "]]"))
        (message "no images in clipboard")))
    )
  )



(defun z/org-delete-linked-file-in-point ()
  "delete a file in point if exists."
  (interactive)
  (let* ((raw-string (or (thing-at-point 'filename) "neverexists"))
	 (rel-filename (replace-regexp-in-string "^file:" "" raw-string))
	 (full-name (expand-file-name rel-filename)))
    (if (file-exists-p full-name)
	(if (y-or-n-p (format "delete %s " full-name))
	    (delete-file full-name))
      (message "no file can be deleted")
      )
    )
  )

(provide 'z-org-ext)
