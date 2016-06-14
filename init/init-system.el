;;; ------------------------------------------------------------
;;;
;;; dired
;;;
;;; ------------------------------------------------------------
(require 'dired-x)

(require-package 'dired-single)
(global-set-key [(f5)] 'dired-single-magic-buffer)
(global-set-key [(control f5)] (function
                                (lambda nil (interactive)
                                  (dired-single-magic-buffer default-directory))))
(global-set-key [(shift f5)] (function
                              (lambda nil (interactive)
                                (message "Current directory is: %s" default-directory))))
(global-set-key [(alt f5)] 'dired-single-toggle-buffer-name)

;;; ------------------------------------------------------------
;;;
;;; shell
;;;
;;; ------------------------------------------------------------
(setq eshell-directory-name (concat (bibo/get-runtimes-dir) (file-name-as-directory "eshell")))

(add-hook 'eshell-mode-hook (lambda ()
                              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
                              (define-key eshell-mode-map (kbd "M-n") 'helm-esh-pcomplete)
                              ))

(provide 'init-system)
