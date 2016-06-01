(defconst *is-windows* (string= system-type "windows-nt"))
(defconst *is-linux* (string= system-type "gnu/linux"))
(defconst *is-mac* (string= system-type "darwin"))

;;; customization group for bibo's
(defgroup bibo nil
  "Bibo's customization."
  :group 'emacs)

;;; performance
(defun bibo/time-difference-in-millis (start end)
  (* 1000.0 (float-time (time-subtract end start))))

(defvar bibo/feature-loading-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require (around bibo/record-feature-loading-times (feature &optional filename noerror) activate)
  "Note in `bibo/feature-loading-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (bibo/time-difference-in-millis require-start-time (current-time) )))
          (add-to-list 'bibo/feature-loading-times
                       (cons feature time)
                       t))))))

(require 'cl)
(require 'cl-lib)

(provide 'init-infra)
