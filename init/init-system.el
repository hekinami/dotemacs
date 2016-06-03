;;; ------------------------------------------------------------
;;;
;;; dired
;;;
;;; ------------------------------------------------------------
(require 'dired-x)

;;; ------------------------------------------------------------
;;;
;;; shell
;;;
;;; ------------------------------------------------------------
(setq eshell-directory-name (concat (bibo/get-runtimes-dir) (file-name-as-directory "eshell")))

(global-set-key (kbd "<f2>") 'eshell)

(provide 'init-system)
