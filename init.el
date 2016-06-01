(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(let ((loading-start-time (current-time)))
  (require 'init-infra)
  (require 'init-utils)
  (require 'init-packages)

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (and (file-exists-p custom-file)
       (load-file custom-file)) 

  (message "loading process took %f ms" (bibo/time-difference-in-millis loading-start-time (current-time)))
  )
