;;; ------------------------------------------------------------
;;;
;;; some customization
;;;
;;; ------------------------------------------------------------
;;; repeat TODO entry task for times
;;; One property defined for this purpose:
;;; 1. REPEAT_TIMES, if t, then add new TODO entry after checkbox checked
(defun z/org-auto-repeat-maybe-advice (orig-fun &rest args)
  "Set REPEAT_TIMES property to a number n to make the task repeats for n times"
  (let ((raw-times (org-entry-get nil "REPEAT_TIMES")))
    (if raw-times
        (let ((times (1- (string-to-number (org-entry-get nil "REPEAT_TIMES")))))
          (org-set-property "REPEAT_TIMES" (number-to-string (if (< times 0) 0 times)))
          (if (> times 0)
              (apply orig-fun args)))
      (apply orig-fun args)
      )
    )
  )

(defun z/org-repeat-enable ()
  (advice-add 'org-auto-repeat-maybe :around #'z/org-auto-repeat-maybe-advice))


(provide 'z-org-repeat)
