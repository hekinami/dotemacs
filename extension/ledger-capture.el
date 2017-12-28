(use-package ledger-mode)
(use-package subr-x)

(defvar bibo/ledger-file (concat org-directory "/fin.ledger")
  "Ledger file")

(defun bibo/ledger-accounts nil
  (let ((ledger-master-file bibo/ledger-file))
    (string-join (ledger-accounts-in-buffer) "|")
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

(let ((tpl-string (format "%%(bibo/read-date) %%^{收款人|%s}\n    %%^{付款账号|%s}  -%%^{金额} CNY\n    %%^{收款账号|%s}\n\n"
                          (bibo/ledger-payees)
                          (bibo/ledger-accounts)
                          (bibo/ledger-accounts))))
  (setq org-capture-templates
        (append `(("f" "记账" plain
                   (file ,(concat org-directory "/fin.ledger")) ,tpl-string :empty-lines 1))
                org-capture-templates)))


(provide 'ledger-capture)
