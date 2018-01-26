;;; ------------------------------------------------------------
;;;
;;; some customization
;;;
;;; ------------------------------------------------------------
;;; Capture a new TODO entry when a checkbox item checked.
;;; Two properties defined for this purpose:
;;; 1. CHECKED-THEN-TODO, if t, then add new TODO entry after checkbox checked
;;; 2. TODO-TEMPLATE, the value if the key for template, if nil, then capture template
;;;    could be selected from list.
(defvar z/current-item-content ""
  "Used to save content of current item of list")

(defun z/get-current-item-content nil
  "Return current content of current item of list, used in capture template"
  z/current-item-content)

(defun z/org-at-item-checked-checkbox-p ()
  "Check if current org checkbox is checked"
  (org-list-at-regexp-after-bullet-p "\\(\\[[X]\\]\\)[ \t]+"))

(defun z/org-checkbox-checked-to-todo-advice (orig-fun &rest args)
  "Set CHECKED-TO-TODO property to `t' to open new TODO entry after checked"
  (apply orig-fun args)
  (when (and (string= "t" (org-entry-get nil "CHECKED-TO-TODO")) 
             (z/org-at-item-checked-checkbox-p))
    (save-excursion
      (back-to-indentation)
      (let* ((pl (car (cdr (org-element-context))))
             (cbegin (plist-get pl :contents-begin))
             (cend (plist-get pl :contents-end)))
        (setq z/current-item-content
              (s-trim (buffer-substring-no-properties cbegin cend)))
        ))
    (org-capture nil (org-entry-get nil "TODO-TEMPLATE"))))

(defun z/checked-to-todo-enable ()
  (advice-add 'org-ctrl-c-ctrl-c :around #'z/org-checkbox-checked-to-todo-advice))

(provide 'z-org-checkbox)
