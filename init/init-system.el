;;; ------------------------------------------------------------
;;;
;;; dired
;;;
;;; ------------------------------------------------------------
(require 'dired-x)

(require-package 'dired-single)
(require-package 'dired-k)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer ".."))))
  (define-key dired-mode-map (kbd "K") 'dired-k))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

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
