;; (package-initialize)

(let ((loading-start-time (current-time)))
  (load (locate-user-emacs-file ".before_everything.el") t)
  
  (add-to-list 'load-path (locate-user-emacs-file "init/"))
  (add-to-list 'load-path (locate-user-emacs-file "extension/"))
  
  (require 'init-prepare)
  (require 'init-looks)
  (require 'init-framework)
  (require 'init-editing)
  (require 'init-reading)
  (require 'init-writing)
  (require 'init-organizer)
  (require 'init-sys)
  (require 'init-erc)
  (require 'init-dev)
  (require 'init-lang)
  (require 'init-web)
  (require 'init-media)

  (load (locate-user-emacs-file "custom.el") t)
  
  (message "loading process took %f ms"
           (z/time-difference-in-millis loading-start-time (current-time)))
  
  (load (locate-user-emacs-file ".after_everything.el") t)
  )
