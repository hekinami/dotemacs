(require-package 'org)
(require-package 'org-plus-contrib)
(setq org-modules '(org-crypt))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-switchb)
(define-key global-map "\C-cc" 'org-capture)

;;; ------------------------------------------------------------
;;;
;;; appearance
;;;
;;; ------------------------------------------------------------
(require-package 'org-bullets)
(require 'org-bullets)
(setq org-bullets-bullet-list '("♠" "♥" "♣" "♦"))

(setq org-hide-leading-stars t)
(setq org-startup-indented nil)
(setq org-cycle-separator-lines 0)

(setq org-catch-invisible-edits 'smart)
(setq org-agenda-window-setup 'other-window)
(defun org-switch-to-buffer-other-window (&rest args)
  ;; override the original one
  (apply 'pop-to-buffer-same-window args))

(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (bibo/timestamp-format-setting)
                           (bibo/use-buffer-face-mode-with-fontfamily bibo/monofont-family)
                           ))

;;; modify columns font to mono
;;; the reason is that origin function use default face to decide the font family, which may not be mono
(advice-add 'org-columns-display-here :around
	    (lambda (orig-fun &rest args)
	      (let ((temp-family (face-attribute 'default :family)))
		(set-face-attribute 'default nil :family bibo/monofont-family)
		(set-face-attribute 'header-line nil :family bibo/monofont-family)
		(apply orig-fun args)
		(set-face-attribute 'default nil :family temp-family)
		)
	      ))

;; table
(setq table-html-th-rows 1)
(setq table-html-table-attribute "")
(setq table-inhibit-auto-fill-paragraph t)

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
                                  (bibo/timestamp-format-setting)
                                  (bibo/use-buffer-face-mode-with-fontfamily bibo/monofont-family)
				  (define-key org-agenda-mode-map " " 'org-agenda-cycle-show)
                                  ))

(setq org-directory (concat (bibo/get-contents-dir) (file-name-as-directory "gtd")))
(setq org-agenda-files `(,(concat (bibo/get-contents-dir) (file-name-as-directory "gtd"))))

(setq org-deadline-warning-days 3)
(setq org-log-into-drawer t)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(require 'org-habit)

;;; ------------------------------------------------------------
;;;
;;; org-crypt
;;;
;;; ------------------------------------------------------------
(require 'org-crypt)
(define-key org-mode-map (kbd "C-c C-/") 'org-decrypt-entry)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "bibo")
(setq auto-save-default nil)

;;; ------------------------------------------------------------
;;;
;;; refile
;;;
;;; ------------------------------------------------------------
(add-hook 'org-mode-hook (lambda ()
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
;;; pomodoro technique
;;;
;;; ------------------------------------------------------------
(require-package 'org-pomodoro)
(require 'org-pomodoro)
(setq org-pomodoro-length 25)
(setq org-pomodoro-long-break-frequency 4)
(setq org-pomodoro-short-break-length 5)
(setq org-pomodoro-long-break-length 10)
(setq org-pomodoro-format "P:%s")
(when *is-windows*
  ;; http://www.elifulkerson.com/projects/commandline-wav-player.php
  (setq org-pomodoro-audio-player (expand-file-name (concat (bibo/get-tools-dir) "tools/sounder.exe"))))

(global-set-key (kbd "<f11>") 'org-pomodoro)


;;; ------------------------------------------------------------
;;;
;;; canlendar & date/time
;;;
;;; ------------------------------------------------------------
(setq diary-file (concat (bibo/get-runtimes-dir) "diary"))
(unless (file-exists-p diary-file) (write-region nil nil diary-file))
(setq view-diary-entries-initially t)
(setq mark-diary-entries-in-calendar t)
(setq number-of-diary-entries 7)

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(require-package 'calfw)
(require-package 'cal-china-x)
(require 'calfw-org)
(require 'calfw-ical)
(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq calendar-holidays cal-china-x-important-holidays)

(add-hook 'cfw:calendar-mode-hook (lambda ()
                                    (when (equal bibo/current-theme-name "molokai")
                                      (set-face-attribute 'cfw:face-toolbar-button-off nil :foreground "white")
                                      (set-face-attribute 'cfw:face-toolbar nil :background nil))
				    (bibo/timestamp-format-setting)
				    (bibo/use-buffer-face-mode-with-fontfamily bibo/monofont-family)
                                    ) 
          )

(defun bibo/open-calendar ()
  (interactive)
  (let* ((sources (list (cfw:org-create-source "Green"))))
    (when (boundp 'bibo/ical-source-list) ; bibo/ical-source-list can be set in custom.el, and cfw:ical-create-source will create one item
      (setcdr sources bibo/ical-source-list)
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
      (?i (call-interactively 'bibo/open-calendar))
      (?\[ (let ((org-agenda-include-inactive-timestamps t))
             (org-agenda-check-type t 'timeline 'agenda)
             (org-agenda-redo))
           (message "Display now includes inactive timestamps as well"))
      (?q (message "Abort"))
      (otherwise (error "Invalid key" )))))

(set-time-zone-rule "GMT-8")
(setq org-time-stamp-custom-formats '("<%y/%m/%d %w>" . "<%y/%m/%d %w %H:%M>"))


;;; ------------------------------------------------------------
;;;
;;; todochiku
;;;
;;; ------------------------------------------------------------
(require-package 'todochiku)
(if *is-windows*
    (setq todochiku-command "C:/Program Files (x86)/full phat/Snarl/tools/heysnarl.exe")
  )
(require 'todochiku)
;;; overwrite the origin one
(defun todochiku-get-arguments (title message icon sticky)
  "Gets todochiku arguments.
This would be better done through a customization probably."
  (cl-case system-type
    ('windows-nt (list (concat "notify" 
                               "?title=" (encode-coding-string title 'gb18030)
                               "&text=" (encode-coding-string message 'gb18030)
                               "&icon=" icon 
                               (when sticky "&timeout=0")))) ; modified this line for Snarl R3.1
    ('darwin (list title (if sticky "-s" "") "-m" message "--image" icon ))
    (t (list "-i" icon "-t"
             (if sticky "0" (int-to-string (* 1000 todochiku-timeout)))
             title message))))

;;; ------------------------------------------------------------
;;;
;;; deft
;;;
;;; ------------------------------------------------------------
(require-package 'deft)
(require 'deft)
(setq deft-default-extension "org")
(setq deft-extensions '("org"))
(setq deft-directory (concat (bibo/get-contents-dir) "deft"))
(add-hook 'deft-mode-hook (lambda nil
                            (bibo/use-buffer-face-mode-with-fontfamily bibo/monofont-family)))
(global-set-key [f9] 'deft)


;;; ------------------------------------------------------------
;;;
;;; screenshot
;;;
;;; ------------------------------------------------------------
(require-package 'uuidgen)
(require 'uuidgen)
(defun bibo/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (when *is-windows*
    (let* ((capturer "\"C:\\Program Files (x86)\\IrfanView\\i_view32.exe\" /clippaste /convert ")
           (buffer-file-name-no-ext (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (buffer-file-dir (file-name-directory (buffer-file-name)))
           (buffer-file-dir-img (concat buffer-file-dir "images/"))
           (exist-dir-img (file-accessible-directory-p buffer-file-dir-img))
           (target-dir (if (not exist-dir-img)
                           buffer-file-dir
                         buffer-file-dir-img))
           (target-file
            (replace-regexp-in-string "/" "\\"
                                      (concat (concat target-dir (uuidgen-4)) ".png") t t)))
      (call-process-shell-command (concat capturer target-file) nil nil nil)
      (insert (concat "[[file:./" (if exist-dir-img "images/" "") (file-name-nondirectory target-file)  "]]")))
    )
  (when *is-linux*
    (let* ((capturer (concat "python " user-emacs-directory "tools/capclip.py "))
           p	     (buffer-file-name-no-ext (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (buffer-file-dir (file-name-directory (buffer-file-name)))
           (buffer-file-dir-img (concat buffer-file-dir "images/"))
           (exist-dir-img (file-accessible-directory-p buffer-file-dir-img))
           (target-dir (if (not exist-dir-img)
                           buffer-file-dir
                         buffer-file-dir-img))
           (target-file
            (replace-regexp-in-string "/" "/"
                                      (concat (concat target-dir (uuidgen-4)) ".png") t t)))
      (if (= 0 (call-process-shell-command (concat capturer target-file) nil))
          (insert (concat "[[file:./" (if exist-dir-img "images/" "") (file-name-nondirectory target-file)  "]]"))
        (message "no images in clipboard")))
    )
  )

(define-key org-mode-map "\C-cs" 'bibo/org-screenshot)

(defun bibo/org-delete-linked-file-in-point ()
  "delete a file in point if exists."
  (interactive)
  (let* ((raw-string (or (thing-at-point 'filename) "neverexists"))
	 (rel-filename (replace-regexp-in-string "^file:" "" raw-string))
	 (full-name (expand-file-name rel-filename)))
    (if (file-exists-p full-name)
	(if (y-or-n-p (format "delete %s " full-name))
	    (delete-file full-name))
      (message "no file can be deleted")
      )
    )
  )

(define-key org-mode-map "\C-cd" 'bibo/org-delete-linked-file-in-point)

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
;;; appointment
;;;
;;; ------------------------------------------------------------
(require 'appt)
(appt-activate t)

(setq appt-message-warning-time 10)
(setq appt-display-interval (1+ appt-message-warning-time)) ; disable multiple reminders
(setq appt-display-mode-line nil)

; use appointment data from org-mode
(defun bibo/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; run when starting Emacs and everyday at 12:05am
(bibo/org-agenda-to-appt)
(run-at-time "12:05am" (* 24 3600) 'bibo/org-agenda-to-appt)

; automatically update appointments when TODO.txt is saved
(add-hook 'after-save-hook
          '(lambda ()
             (if (string= (buffer-file-name) (expand-file-name
                                              (concat (bibo/get-contents-dir) "gtd/event.gtd.org")))
                 (bibo/org-agenda-to-appt))))

;;; ------------------------------------------------------------
;;;
;;; projects and publish
;;;
;;; ------------------------------------------------------------
(setq org-projects-base (concat (bibo/get-contents-dir) (file-name-as-directory "org")))
(setq org-projects-publish (concat (bibo/get-contents-dir) (file-name-as-directory "orgp")))

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

(defun bibo/org-init-project-directory (&optional template)
  "for now, use default template only"
  (interactive)
  (if (file-exists-p ".org-project")
      (message ".org-project file already existed.")
    (progn
      (copy-file (concat org-tpl-directory "default/.org-project") ".org-project" )
      (message ".org-project file created.")
      ))
  )

(define-key org-mode-map "\C-ci" 'bibo/org-init-project-directory)

;;; ------------------------------------------------------------
;;;
;;; export
;;;
;;; ------------------------------------------------------------
(define-key org-mode-map "\C-cp" 'org-publish-current-project)
(setq org-tpl-directory (concat (bibo/get-stuff-dir) (file-name-as-directory "orgtemplate")))

(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq org-html-doctype "html5")
(setq org-publish-timestamp-directory (concat (bibo/get-runtimes-dir) "org-timestamps"))
(setq org-id-locations-file (concat (bibo/get-runtimes-dir) "org-id-locations"))
(setq org-export-with-sub-superscripts nil)
(setq org-html-htmlize-output-type 'inline-css)
(setq org-export-headline-levels 4)
(setq org-src-fontify-natively t)

;;; ------------------------------------------------------------
;;;
;;; simple-httpd
;;;
;;; ------------------------------------------------------------
(setq httpd-port 3721)
(setq httpd-root (concat (bibo/get-contents-dir) (file-name-as-directory "orgp")))
(httpd-start)
(global-set-key (kbd "<f8>") (lambda nil
                               (interactive)
                               (browse-url "http://localhost:3721")))

(provide 'init-orgnization)
