(use-package org
  :ensure org-plus-contrib
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-switchb)
   ("C-c c" . org-capture)
   :map org-mode-map
   (("C-c s" . z/org-screenshot)
    ("C-c d" . z/org-delete-linked-file-in-point)))
  :config
  (setq org-modules '(org-crypt org-drill org-checklist org-habit))

  
;;; ------------------------------------------------------------
;;;
;;; appearance
;;;
;;; ------------------------------------------------------------ 
  (setq org-hide-leading-stars t)
  (setq org-startup-indented nil)
  (setq org-cycle-separator-lines 0)

  (setq org-catch-invisible-edits 'smart)
  (setq org-agenda-window-setup 'other-window)
  ;; table
  (setq table-html-th-rows 1)
  (setq table-html-table-attribute "")
  (setq table-inhibit-auto-fill-paragraph t)

  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)
                             (z/timestamp-format-setting)
                             ))
;;; ------------------------------------------------------------
;;;
;;; agenda & gtd
;;;
;;; ------------------------------------------------------------
  (setq org-agenda-overriding-columns-format "%25ITEM %TODO %CATEGORY %3PRIORITY %20TAGS")
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-span 'day)
  (setq org-agenda-sorting-strategy '(todo-state-down deadline-up scheduled-up))

  (add-hook 'org-agenda-mode-hook (lambda ()
                                    (z/timestamp-format-setting)
                                    (define-key org-agenda-mode-map " " 'org-agenda-cycle-show)
                                    ))

  (setq org-directory (concat (z/get-contents-dir) (file-name-as-directory "gtd")))
  (setq org-agenda-files `(,(concat (z/get-contents-dir) (file-name-as-directory "gtd"))))

  (setq org-deadline-warning-days 3)
  (setq org-log-into-drawer t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;;; ------------------------------------------------------------
;;;
;;; capture
;;;
;;; ------------------------------------------------------------
  (let ((template-file (expand-file-name ".org-capture-templates" user-emacs-directory)))
    (when (file-exists-p template-file)
      (load-file template-file)
      )
    )

;;; ------------------------------------------------------------
;;;
;;; babel
;;;
;;; ------------------------------------------------------------
  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (shell . t)
     (restclient . t)
     (ledger . t)
     ))
  
;;; ------------------------------------------------------------
;;;
;;; refile
;;;
;;; ------------------------------------------------------------
  (add-hook
   'org-mode-hook
   (lambda ()
     (when (string-match "gtd.org" (or buffer-file-name (buffer-name)))
       (make-variable-buffer-local 'org-refile-targets)
       (setq org-refile-targets (quote ((nil :maxlevel . 2)
                                        (org-agenda-files :maxlevel . 2))))
       )
     ))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  )

(use-package ob-restclient
  :ensure t
  :defer t)

;;; modify columns font to mono
;;; the reason is that origin function use default face to decide the font family, which may not be mono
(advice-add 'org-columns-display-here :around
	    (lambda (orig-fun &rest args)
	      (let ((temp-family (face-attribute 'default :family)))
		(apply orig-fun args)
		(set-face-attribute 'default nil :family temp-family)
		)
	      ))

(use-package org-bullets
  :ensure t
  :defer t
  :config
  (setq org-bullets-bullet-list '("♠" "♥" "♣" "♦"))
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)))
  :after org)

(use-package org-drill-table
  :ensure t
  :defer t)

;;; ------------------------------------------------------------
;;;
;;; org-crypt
;;;
;;; ------------------------------------------------------------
(use-package org-crypt
  :bind
  (:map org-mode-map
        ("C-c C-/" . org-decrypt-entry))
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key "z")
  (setq auto-save-default nil)
  )

;;; ------------------------------------------------------------
;;;
;;; pomodoro technique
;;;
;;; ------------------------------------------------------------
(use-package org-pomodoro
  :ensure t
  :bind ("<f11>" . org-pomodoro)
  :config
  (setq org-pomodoro-length 25)
  (setq org-pomodoro-long-break-frequency 4)
  (setq org-pomodoro-short-break-length 5)
  (setq org-pomodoro-long-break-length 10)
  (setq org-pomodoro-format "P:%s")
  (when *is-windows*
    ;; http://www.elifulkerson.com/projects/commandline-wav-player.php
    (setq org-pomodoro-audio-player (expand-file-name (concat (z/get-tools-dir) "/sounder.exe")))))

;;; ------------------------------------------------------------
;;;
;;; canlendar & date/time
;;;
;;; ------------------------------------------------------------
(setq diary-file (concat (z/get-runtimes-dir) "diary"))
(unless (file-exists-p diary-file) (write-region nil nil diary-file))
(setq view-diary-entries-initially t)
(setq mark-diary-entries-in-calendar t)
(setq mark-holidays-in-calendar t)
(setq number-of-diary-entries 7)

(add-hook 'diary-display-hook 'diary-fancy-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(use-package calfw
  :ensure t
  :defer t
  :commands cfw:org-create-source
  :config
  (add-hook 'cfw:calendar-mode-hook
            (lambda ()
              (when (equal z/current-theme-name "molokai")
                (set-face-attribute 'cfw:face-toolbar-button-off nil :foreground "white")
                (set-face-attribute 'cfw:face-toolbar nil :background nil))
              (z/timestamp-format-setting)
              )))

(use-package cal-china-x
  :ensure t
  :defer t
  :config
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq calendar-holidays cal-china-x-important-holidays))

(use-package calfw-ical
  :ensure t
  :defer t)

(defun z/open-calendar ()
  (interactive)
  (let* ((sources (list (cfw:org-create-source "Green"))))
    (when (boundp 'z/ical-source-list) ; z/ical-source-list can be set in custom.el, and cfw:ical-create-source will create one item
      (setcdr sources z/ical-source-list)
      )
    (cfw:open-calendar-buffer :contents-sources sources)
    )
  )

;;; redefine the original dispatcher
(defun org-agenda-view-mode-dispatch ()
  "Call one of the view mode commands."
  (interactive)
  (message "View:  [d]ay        [w]eek       [m]onth   [y]ear   [SPC]reset  [q]uit/abort
      time[G]rid   [[]inactive  [f]ollow      [l]og    [L]og-all   [c]lockcheck
      [a]rch-trees [A]rch-files clock[R]eport include[D]iary       [E]ntryText
      [i]cfw-view")
  (let ((a (read-char-exclusive)))
    (cl-case a
      (?\  (call-interactively 'org-agenda-reset-view))
      (?d (call-interactively 'org-agenda-day-view))
      (?w (call-interactively 'org-agenda-week-view))
      (?m (call-interactively 'org-agenda-month-view))
      (?y (call-interactively 'org-agenda-year-view))
      (?l (call-interactively 'org-agenda-log-mode))
      (?L (org-agenda-log-mode '(4)))
      (?c (org-agenda-log-mode 'clockcheck))
      ((?F ?f) (call-interactively 'org-agenda-follow-mode))
      (?a (call-interactively 'org-agenda-archives-mode))
      (?A (org-agenda-archives-mode 'files))
      ((?R ?r) (call-interactively 'org-agenda-clockreport-mode))
      ((?E ?e) (call-interactively 'org-agenda-entry-text-mode))
      (?G (call-interactively 'org-agenda-toggle-time-grid))
      (?D (call-interactively 'org-agenda-toggle-diary))
      (?\! (call-interactively 'org-agenda-toggle-deadlines))
      (?i (call-interactively 'z/open-calendar))
      (?\[ (let ((org-agenda-include-inactive-timestamps t))
             (org-agenda-check-type t 'timeline 'agenda)
             (org-agenda-redo))
           (message "Display now includes inactive timestamps as well"))
      (?q (message "Abort"))
      (otherwise (error "Invalid key" )))))

(set-time-zone-rule "GMT-8")
(setq org-time-stamp-custom-formats '("<%y/%m/%d %w>" . "<%y/%m/%d %w %H:%M>"))


;; ;;; ------------------------------------------------------------
;; ;;;
;; ;;; todochiku
;; ;;;
;; ;;; ------------------------------------------------------------
;; ;; (require-package 'todochiku)
;; ;; (if *is-windows*
;; ;;     (setq todochiku-command "C:/Program Files (x86)/full phat/Snarl/tools/heysnarl.exe")
;; ;;   )
;; ;; (require 'todochiku)
;; ;; ;;; overwrite the origin one
;; ;; (defun todochiku-get-arguments (title message icon sticky)
;; ;;   "Gets todochiku arguments.
;; ;; This would be better done through a customization probably."
;; ;;   (cl-case system-type
;; ;;     ('windows-nt (list (concat "notify" 
;; ;;                                "?title=" (encode-coding-string title 'gb18030)
;; ;;                                "&text=" (encode-coding-string message 'gb18030)
;; ;;                                "&icon=" icon 
;; ;;                                (when sticky "&timeout=0")))) ; modified this line for Snarl R3.1
;; ;;     ('darwin (list title (if sticky "-s" "") "-m" message "--image" icon ))
;; ;;     (t (list "-i" icon "-t"
;; ;;              (if sticky "0" (int-to-string (* 1000 todochiku-timeout)))
;; ;;              title message))))

;;; ------------------------------------------------------------
;;;
;;; deft
;;;
;;; ------------------------------------------------------------
(use-package deft
  :ensure t
  :bind
  (("<f9>" . deft))
  :config
  (setq deft-default-extension "org")
  (setq deft-extensions '("org"))
  (setq deft-directory (concat (z/get-contents-dir) "deft"))
  (setq deft-new-file-format "%Y-%m-%dT%H%M")
  )

;;; ------------------------------------------------------------
;;;
;;; screenshot
;;;
;;; ------------------------------------------------------------
(use-package uuidgen
  :ensure t
  :defer t)

;;; ------------------------------------------------------------
;;;
;;; appointment
;;;
;;; ------------------------------------------------------------
(require 'appt)
(appt-activate t)

(setq appt-message-warning-time 10)
(setq appt-display-interval (1+ appt-message-warning-time)) ; disable multiple reminders
(setq appt-display-mode-line nil)

; use appointment data from org-mode
(defun z/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

                                        ; run when starting Emacs and everyday at 12:05am
(z/org-agenda-to-appt)
(run-at-time "12:05am" (* 24 3600) 'z/org-agenda-to-appt)

                                        ; automatically update appointments when TODO.txt is saved
(add-hook 'after-save-hook
          '(lambda ()
             (if (string= (buffer-file-name) (expand-file-name
                                              (concat (z/get-contents-dir) "gtd/event.gtd.org")))
                 (z/org-agenda-to-appt))))

;;; ------------------------------------------------------------
;;;
;;; projects and publish
;;;
;;; ------------------------------------------------------------
(setq org-projects-base (concat (z/get-contents-dir) (file-name-as-directory "org")))
(setq org-projects-publish (concat (z/get-contents-dir) (file-name-as-directory "orgp")))

;;; use a .org-project file in each project directory to define a project
;;; org-publish-project-alist would be set just before we try to publish
(advice-add 'org-publish-current-project :around (lambda (orig-fun &rest args)
                                                   (if (file-exists-p ".org-project")
                                                       (progn
                                                         (setq org-publish-project-alist ())
                                                         (load-file ".org-project")
                                                         (apply orig-fun args)
                                                         (setq org-publish-project-alist ()))
                                                     (message "no .org-project definition found.")
                                                     )
                                                   ))

(defun z/org-init-project-directory (&optional template)
  "for now, use default template only"
  (interactive)
  (if (file-exists-p ".org-project")
      (message ".org-project file already existed.")
    (let* ((template-candidates (cl-remove-if (lambda (x)
                                                (or (string= "." x)
                                                    (string= ".." x))
                                                )
                                              (directory-files org-tpl-directory)))
           (template (helm-comp-read "Select template: " template-candidates)))
      (progn
        (copy-file (concat org-tpl-directory (concat template "/.org-project")) ".org-project" )
        (message ".org-project file created.")
        ))
    )
  )

(define-key org-mode-map "\C-c\C-xh" 'z/org-init-project-directory)

;;; ------------------------------------------------------------
;;;
;;; export
;;;
;;; ------------------------------------------------------------
(define-key org-mode-map "\C-cp" 'org-publish-current-project)
(setq org-tpl-directory (concat (z/get-stuff-dir) (file-name-as-directory "orgtemplate")))

(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-publish-timestamp-directory (concat (z/get-runtimes-dir) "org-timestamps"))
(setq org-id-locations-file (concat (z/get-runtimes-dir) "org-id-locations"))
(setq org-export-with-sub-superscripts nil)
(setq org-html-htmlize-output-type 'inline-css)
(setq org-export-headline-levels 4)
(setq org-src-fontify-natively t)

;;; redefine the original one, move the svg related stuff
(eval-after-load "ox-html"
  '(progn
     (defun org-html--format-image (source attributes info)
       "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
       (org-html-close-tag
        "img"
        (org-html--make-attribute-string
         (org-combine-plists
          (list :src source
                :alt (if (string-match-p "^ltxpng/" source)
                         (org-html-encode-plain-text
                          (org-find-text-property-in-string 'org-latex-src source))
                       (file-name-nondirectory source)))
          attributes))
        info)
       )
     )
  )

;;; latex
;;; font: https://www.google.com/get/noto/help/cjk/
(setq org-latex-classes
      '(("article"
         "
\\documentclass[12pt,a4paper]{article}
\\usepackage[margin=2cm]{geometry}
\\usepackage{fontspec}
\\setromanfont{Noto Serif CJK SC:style=Regular}
\\setsansfont{Noto Sans CJK SC Regular}
\\setmonofont[Color={999999}]{Noto Sans Mono CJK SC Regular}
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt
\\linespread{1.1}
\\usepackage{hyperref}
\\hypersetup{
  colorlinks=true,
  linkcolor=[rgb]{0,0.37,0.53},
  citecolor=[rgb]{0,0.47,0.68},
  filecolor=[rgb]{0,0.37,0.53},
  urlcolor=[rgb]{0,0.37,0.53},
  pagebackref=true,
  linktoc=all,}
"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ))

(setq org-latex-with-hyperref t)
(setq org-latex-default-packages-alist
      '(
        ("AUTO" "inputenc" t)
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "wasysym" t)
        ("" "multicol" t)  ; 這是我另外加的，因為常需要多欄位文件版面。
        ("" "amssymb" t)
        "\\tolerance=1000"))

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

(setq org-file-apps '((auto-mode . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . "firefox %s")
                      ("\\.pdf\\'" . "xreader %s")
                      ("\\.jpg\\'" . "xviewer %s")))

;; ;;; beamer
;; (require 'ox-beamer)

;; ;;; ------------------------------------------------------------
;; ;;;
;; ;;; org-protocol
;; ;;;
;; ;;; ------------------------------------------------------------

;; ;; Save following snippet to .reg file to register protocal in windows
;; ;; ------------
;; ;; REGEDIT4

;; ;; [HKEY_CLASSES_ROOT\org-protocol]
;; ;; @="URL:Org Protocol"
;; ;; "URL Protocol"=""
;; ;; [HKEY_CLASSES_ROOT\org-protocol\shell]
;; ;; [HKEY_CLASSES_ROOT\org-protocol\shell\open]
;; ;; [HKEY_CLASSES_ROOT\org-protocol\shell\open\command]
;; ;; @="\"C:\\Programme\\Emacs\\emacs\\bin\\emacsclientw.exe\" \"%1\""
;; ;; ------------

;; ;; http://kb.mozillazine.org/Register_protocol
(use-package org-protocol)

;;; ------------------------------------------------------------
;;;
;;; simple-httpd
;;;
;;; ------------------------------------------------------------
(use-package simple-httpd
  :ensure t
  :config
  (setq url-cache-directory (concat (z/get-runtimes-dir) "url/cache"))
  (setq httpd-port 3721)
  (setq httpd-root (concat (z/get-contents-dir) (file-name-as-directory "orgp")))
  (httpd-start)
  (advice-add 'save-buffers-kill-terminal :around (lambda (orig-fun &rest args)
                                                    (httpd-stop)
                                                    (apply orig-fun args)
                                                    )))

(use-package z-org-ext
  :bind ("<f8>" . z/open-browser)
  :after simple-httpd)

;; ;;; ------------------------------------------------------------
;; ;;;
;; ;;; bbdb
;; ;;;
;; ;;; ------------------------------------------------------------
;; (require-package 'bbdb)
;; (require-package 'bbdb-china)
;; (require-package 'bbdb-vcard)

;; (setq bbdb-file (concat (z/get-contents-dir) "bbdb"))
;; (setq bbdb-phone-style nil)

;; (add-hook 'bbdb-mode-hook (lambda nil
;;                             (z/use-buffer-face-mode-with-fontfamily z/monofont-family)))
;; ;;; ------------------------------------------------------------
;; ;;;
;; ;;; thunderbird interaction
;; ;;;
;; ;;; ------------------------------------------------------------
;; ;;; install https://github.com/poohsen/thunderlink to thunderbird

;; ;; Save following snippet to .reg file to register protocal in windows
;; ;; ------------
;; ;; REGEDIT4

;; ;; [HKEY_CLASSES_ROOT\thunderlink]
;; ;; @="URL:thunderlink Protocol"
;; ;; "URL Protocol"=""

;; ;; [HKEY_CLASSES_ROOT\thunderlink\shell]

;; ;; [HKEY_CLASSES_ROOT\thunderlink\shell\open]

;; ;; [HKEY_CLASSES_ROOT\thunderlink\shell\open\command]
;; ;; @="\"C:\\Program Files (x86)\\Mozilla Thunderbird\\thunderbird.exe\" -thunderlink \"%1\""
;; ;; ------------

;; ;; http://kb.mozillazine.org/Register_protocol#Windows

;; (org-add-link-type "thunderlink" 'org-thunderlink-open)
;; ;; (add-hook 'org-store-link-functions 'org-thunderlink-store-link)

;; (defun org-thunderlink-open (path)
;;   "visit the thunderlink on path"
;;   (browse-url (concat "thunderlink:" path)))

;; (defun org-thunderlink-store-link ()
;;   "to be implement")

;; ;;; ------------------------------------------------------------
;; ;;;
;; ;;; email
;; ;;;
;; ;;; ------------------------------------------------------------
;; (require-package 'wanderlust)

;; ;;; set following variables in custom.el
;; ;; wl-smtp-connection-type
;; ;; wl-smtp-posting-port
;; ;; wl-smtp-authenticate-type
;; ;; wl-smtp-posting-user
;; ;; wl-smtp-posting-server
;; ;; wl-local-domain
;; ;; wl-message-id-domain
;; ;;
;; ;; elmo-passwd-alist-save will save all of your Wanderlust passwords to ~/.elmo/passwd.

;; (setq wl-folders-file (concat user-emacs-directory ".wl-folder"))
;; (add-hook 'wl-summary-mode-hook (lambda nil
;;                                   (z/use-buffer-face-mode-with-fontfamily z/monofont-family)))

;; (add-hook 'mime-view-mode-hook (lambda nil
;;                                  (z/use-buffer-face-mode-with-fontfamily z/monofont-family)))

;; (require-package 'ob-restclient)
;; (require 'ob-restclient)

;; ;;; ------------------------------------------------------------
;; ;;;
;; ;;; gnuplot
;; ;;;
;; ;;; ------------------------------------------------------------
;; (require-package 'gnuplot)
;; (require 'ob-gnuplot)

;; ;;; ------------------------------------------------------------
;; ;;;
;; ;;; ledger
;; ;;;
;; ;;; ------------------------------------------------------------
(use-package ledger-mode
  :ensure t
  :mode "\\.ledger$" 
  :config
  (setq ledger-reconcile-default-commodity "CNY"))

(use-package ledger-capture
  :after ledger-mode)

;;; ------------------------------------------------------------
;;;
;;; org-brain
;;;
;;; ------------------------------------------------------------
(use-package org-brain
  :ensure t
  :bind
  (("C-z b" . org-brain-visualize))
  :config
  (setq org-id-track-globally t)
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))

(use-package z-org-checkbox
  :config
  (z/checked-to-todo-enable)
  :after org)

(provide 'init-org)
