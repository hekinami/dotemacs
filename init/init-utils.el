(defun z/get-runtimes-dir ()  
  (let ((dir (concat user-emacs-directory
                     (file-name-as-directory "runtimes"))))
    (unless (file-exists-p dir)
      (make-directory dir)
      ) dir
        )
  )

(defun z/get-contents-dir ()  
  (let ((dir (concat user-emacs-directory
                     (file-name-as-directory "contents"))))
    (unless (file-exists-p dir)
      (make-directory dir)
      ) dir
        )
  )

(defun z/get-stuff-dir ()  
  (let ((dir (concat user-emacs-directory
                     (file-name-as-directory "stuff"))))
    (unless (file-exists-p dir)
      (make-directory dir)
      ) dir
        )
  )

(defun z/get-tools-dir ()  
  (let ((dir (concat user-emacs-directory
                     (file-name-as-directory "tools"))))
    (unless (file-exists-p dir)
      (make-directory dir)
      ) dir
        )
  )

(provide 'init-utils)


