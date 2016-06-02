(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(let ((loading-start-time (current-time)))
  (let ((proxy-file (expand-file-name ".proxy" user-emacs-directory)))
    (and (file-exists-p proxy-file)
         (load-file proxy-file))
    )
  
  (require 'init-infra)
  (require 'init-utils)
  (require 'init-packages)
  (require 'init-ui)
  (require 'init-editing)
  (require 'init-orgnization)

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (and (file-exists-p custom-file)
       (load-file custom-file)) 

  (message "loading process took %f ms" (bibo/time-difference-in-millis loading-start-time (current-time)))
  )
