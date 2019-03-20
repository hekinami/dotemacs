(use-package subr-x)

(defvar bibo/ledger-file (locate-contents-file "organizer/fin/fin.ledger")
  "Ledger file")

(defun bibo/ledger-accounts nil
  (with-temp-buffer
    (let ((ledger-file bibo/ledger-file))
      (insert-file-contents ledger-file)
      (string-join (ledger-accounts-list-in-buffer) "|")
      )
    ))

(defun bibo/ledger-payees nil
  (with-temp-buffer
    (let ((ledger-file bibo/ledger-file))
      (insert-file-contents ledger-file)
      (string-join (ledger-payees-in-buffer) "|")
      )
    ))

(defun bibo/read-date ()
  "Parse date for capturing ledger entries via org mode"
  (replace-regexp-in-string "-" "/" (org-read-date)))

(let ((tpl-string (format "%%(bibo/read-date) %%^{Payee|%s}\n    %%^{Target Account|%s}  %%^{Amount} CNY\n    %%^{Source Account|%s}\n\n"
                          (bibo/ledger-payees)
                          (bibo/ledger-accounts)
                          (bibo/ledger-accounts))))
(setq org-capture-templates
      (append `(("f" "Ledger" plain
                 (file ,bibo/ledger-file) ,tpl-string :empty-lines 1))
              org-capture-templates)))


(provide 'ledger-capture)
