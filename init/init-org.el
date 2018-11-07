(use-package org
  :ensure org-plus-contrib
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-switchb)
   ("C-c c" . org-capture))
  :config
  (use-package z-org-ext
    :bind (("<f8>" . z/open-browser)
           :map org-mode-map
           (("C-c s" . z/org-screenshot)
            ("C-c d" . z/org-delete-linked-file-in-point)))
    )
  (setq org-modules '(org-crypt org-drill org-checklist org-habit))

;;; ------------------------------------------------------------
;;;
;;; methodology
;;;
;;; ------------------------------------------------------------
  ;; Priority Definition
  ;; 
  ;; A: do: good, don't: harm, cannot atone
  ;; B: do: good, don't: harm, can atone
  ;; C: do: good, don't: may be harmful
  ;; D: do: good, don't: no harm
  ;; E: do: may be good, don't: no harm
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?E)
  (setq org-default-priority ?C)

;;; ------------------------------------------------------------
;;;
;;; link
;;;
;;; ------------------------------------------------------------

  ;; Thunderlink support
  ;; https://addons.thunderbird.net/en-us/thunderbird/addon/thunderlink/
  (org-add-link-type "thunderlink" 'org-thunderlink-open)

  (setq thunderlink-thunderbird
        "/usr/lib/thunderbird/thunderbird")

  (defun org-thunderlink-open (link)
    (message link)
    (start-process-shell-command "thunderbird" nil (format "%s -thunderlink thunderlink:%s" thunderlink-thunderbird link)))
  
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
  (setq org-agenda-sorting-strategy '(todo-state-down priority-down deadline-up scheduled-up))

  (add-hook 'org-agenda-mode-hook (lambda ()
                                    (z/timestamp-format-setting)
                                    (define-key org-agenda-mode-map " " 'org-agenda-cycle-show)
                                    ))

  (setq org-directory (concat (z/get-contents-dir) "organizer"))
  (setq org-agenda-files `(,(concat org-directory "/gtd")
                           ,(concat org-directory "/info")))

  (setq org-deadline-warning-days 3)
  (setq org-log-into-drawer t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

  (setq org-agenda-custom-commands
        '(("A" "Accounts" ((tags "account" ((org-agenda-hide-tags-regexp "account\\|crypt")
                                            (org-agenda-prefix-format "")))))
          ("L" "Links" ((tags "link" ((org-agenda-hide-tags-regexp "link")
                                      (org-agenda-prefix-format "")))))
          ))

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
     (rust . t)
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

;;; ------------------------------------------------------------
;;;
;;; screenshot
;;;
;;; ------------------------------------------------------------
  (use-package uuidgen
    :ensure t)

  (add-hook 'org-clock-in-hook 'save-buffer)
  (add-hook 'org-clock-out-hook 'save-buffer)
  )

(use-package ob-restclient
  :ensure t
  :defer t)

(use-package ob-rust
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
  :config
  (add-hook 'cfw:calendar-mode-hook
            (lambda ()
              (when (equal z/current-theme-name "molokai")
                (set-face-attribute 'cfw:face-toolbar-button-off nil :foreground "white")
                (set-face-attribute 'cfw:face-toolbar nil :background nil))
              (z/timestamp-format-setting)
              ))
  (setq cfw:fchar-junction ?╬
        cfw:fchar-vertical-line ?║
        cfw:fchar-horizontal-line ?═
        cfw:fchar-left-junction ?╠
        cfw:fchar-right-junction ?╣
        cfw:fchar-top-junction ?╦
        cfw:fchar-top-left-corner ?╔
        cfw:fchar-top-right-corner ?╗)
  )

(use-package cal-china-x
  :ensure t
  :config
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq calendar-holidays cal-china-x-important-holidays))

(use-package calfw-cal
  :ensure t
  :defer t
  :commands cfw:cal-create-source)

(use-package calfw-ical
  :ensure t
  :defer t
  :commands cfw:ical-create-source)

(use-package calfw-org
  :ensure t
  :defer t
  :commands cfw:org-create-source
  )

(defun z/open-calendar ()
  (interactive)
  (let* ((sources (list (cfw:cal-create-source "Green"))))
    (when (boundp 'z/ical-source-list) ; z/ical-source-list can be set in custom.el, and cfw:ical-create-source will create one item
      (setcdr sources z/ical-source-list)
      )
    (cfw:open-calendar-buffer :contents-sources sources)
    )
  )

(global-set-key (kbd "<f5>") 'z/open-calendar)

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
;;; org-journal
;;;
;;; ------------------------------------------------------------
(use-package org-journal
  :ensure t
  :bind
  (("C-c C-j" . org-journal-new-entry))
  :config
  (setq org-journal-dir (concat (z/get-contents-dir) "org/journal")))

;;; ------------------------------------------------------------
;;;
;;; diary-manager
;;;
;;; ------------------------------------------------------------
(use-package diary-manager
  :ensure t
  :config
  (setq diary-manager-location (concat (z/get-contents-dir) "org/diary"))
  (setq diary-manager-enable-git-integration nil)
  (setq diary-manager-entry-extension ".org")
  )

;;; ------------------------------------------------------------
;;;
;;; org-kanban
;;;
;;; ------------------------------------------------------------
(use-package org-kanban
  :ensure t)

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

;; use appointment data from org-mode
(defun z/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; run when starting Emacs and everyday at 12:05am
(z/org-agenda-to-appt)
(run-at-time "12:05am" (* 24 3600) 'z/org-agenda-to-appt)

;; automatically update appointments when TODO.txt is saved
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

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "file:///home/hekinami/git/reveal.js"))

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

(use-package z-org-repeat
  :config
  (z/org-repeat-enable))

(use-package simplenote2
  :ensure t)

(provide 'init-org)
