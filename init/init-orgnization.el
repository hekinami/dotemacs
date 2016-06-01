(require-package 'org)
(require-package 'org-plus-contrib)
(setq org-modules '(org-crypt))

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
  (setq org-pomodoro-audio-player (expand-file-name (concat user-emacs-directory "tools/sounder.exe"))))
(setq org-pomodoro-sound (expand-file-name (concat user-emacs-directory "/sound/bell.wav")))

(global-set-key (kbd "<f11>") 'org-pomodoro)



(require-package 'calfw)
(require-package 'cal-china-x)

;;; ------------------------------------------------------------
;;;
;;; notification
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
;;; noting
;;;
;;; ------------------------------------------------------------
(require-package 'deft)
(require 'deft)
(setq deft-default-extension "org")
(setq deft-extensions '("org"))
(setq deft-directory (concat (bibo/get-contents-dir) "deft"))
(add-hook 'deft-mode-hook (lambda nil
                            (bibo/use-buffer-face-mode-with-fontfamily bibo/monofont-family)))

(require-package 'org-bullets)
(require 'org-bullets)
(setq org-bullets-bullet-list '("♠" "♥" "♣" "♦"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "bibo")
(setq auto-save-default nil)

(provide 'init-orgnization)
