;;; customization group for z's
(defgroup z nil
  "z's customization."
  :group 'emacs)

;;; performance
(defun z/time-difference-in-millis (start end)
  (* 1000.0 (float-time (time-subtract end start))))

(defvar z/feature-loading-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require (around z/record-feature-loading-times (feature &optional filename noerror) activate)
  "Note in `z/feature-loading-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (z/time-difference-in-millis require-start-time (current-time) )))
          (add-to-list 'z/feature-loading-times
                       (cons feature time)
                       t))))))

(defun z/get-runtimes-dir ()
  (let ((dir (locate-user-emacs-file "runtimes/")))
    (unless (file-exists-p dir)
      (make-directory dir)) dir))

(defun z/get-contents-dir ()
  (let ((dir (locate-user-emacs-file "contents/")))
    (unless (file-exists-p dir)
      (make-directory dir)) dir))

(defun z/get-stuff-dir ()
  (let ((dir (locate-user-emacs-file "stuff/")))
    (unless (file-exists-p dir)
      (make-directory dir)) dir))

(defun z/get-tools-dir ()
  (let ((dir (locate-user-emacs-file "tools/")))
    (unless (file-exists-p dir)
      (make-directory dir)) dir))

(require 'cl)
(require 'cl-lib)

(provide 'init-infra)
