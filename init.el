
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst *is-windows* (string= system-type "windows-nt"))
(defconst *is-linux* (string= system-type "gnu/linux"))
(defconst *is-mac* (string= system-type "darwin"))

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
  (require 'init-framework)
  (require 'init-editing)
  (require 'init-orgnization)
  (require 'init-system)
  (require 'init-information)
  (require 'init-erc)

  (require 'init-development)
  (require 'init-languages)
  (require 'init-web)

  (require 'init-media)

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (and (file-exists-p custom-file)
       (load-file custom-file)) 

  (message "loading process took %f ms" (bibo/time-difference-in-millis loading-start-time (current-time)))
  )
