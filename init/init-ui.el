;;; ------------------------------------------------------------
;;;
;;; customable vairables
;;;
;;; ------------------------------------------------------------
(defcustom bibo/monofont-family (if *is-windows* "YaHei Consolas Hybrid" "WenQuanYi Zen Hei Mono")
  "the proper MONO font"
  :type 'string
  :group 'bibo)

;;; ------------------------------------------------------------
;;;
;;; theme
;;;
;;; ------------------------------------------------------------
(use-package molokai
  :defer t
  :init
  (require-package 'molokai-theme)
  (load-theme 'molokai t)
  :config
  (setq bibo/current-theme-name "molokai"))

;;; ------------------------------------------------------------
;;;
;;; frame
;;;
;;; ------------------------------------------------------------
(setq frame-title-format "[%F]")

(when (and (display-graphic-p) *is-windows*) 
  (set-frame-font "Courier New-12")
  (let ((fontset (frame-parameter nil 'font)))
    (set-fontset-font fontset 'gb18030 (font-spec :family "YaHei Consolas Hybrid"))
    (set-fontset-font fontset 'kana (font-spec :family "MS Gothic"))
    ))

(when (and (display-graphic-p) *is-linux*)
  (set-frame-font "Courier 10 Pitch-12:style=Regular")
  (let ((fontset (frame-parameter nil 'font)))
    (set-fontset-font fontset 'gb18030 (font-spec :family "WenQuanYi Zen Hei Mono"))
    (set-fontset-font fontset 'kana (font-spec :family "TakaoPGothic"))
    ))

(setq default-frame-alist
      (append
       `((font . ,(frame-parameter nil 'font))
         (height . 25)
	 (width . 100)) default-frame-alist))

(global-set-key (kbd "C-x C-a f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-x C-a m") 'toggle-frame-maximized)

(set-frame-parameter nil 'alpha '(100 100))
(defun bibo/toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 50))))
(global-set-key (kbd "C-x C-a t") 'bibo/toggle-transparency)

;;; ------------------------------------------------------------
;;;
;;; miscellaneous
;;;
;;; ------------------------------------------------------------
(global-unset-key (kbd "C-z"))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
(column-number-mode 1)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(global-set-key (kbd "<f10>") 'menu-bar-mode)

(use-package sr-speedbar
  :bind ("C-z s" . sr-speedbar-toggle)
  :init (require-package 'sr-speedbar))

;;; ------------------------------------------------------------
;;;
;;; cursor
;;;
;;; ------------------------------------------------------------
;; (require-package 'highlight-tail)
;; (setq highlight-tail-timer 0.01)
;; (highlight-tail-mode 1)
(use-package highlight-tail
  :init
  (require-package 'highlight-tail)
  :config
  (setq highlight-tail-timer 0.01)
  (highlight-tail-mode 1))

(setq blink-cursor-blinks 0)

(setq hcz-set-cursor-color-color "")
(setq hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
	 (if buffer-read-only "blue"
	   (if overwrite-mode "red"
	     "white"))))
    (unless (and
	     (string= color hcz-set-cursor-color-color)
	     (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

;;; ------------------------------------------------------------
;;;
;;; scrollbar
;;;
;;; ------------------------------------------------------------
(use-package yascroll
  :init
  (require-package 'yascroll)
  :config
  (scroll-bar-mode -1)
  (global-yascroll-bar-mode))

;;; ------------------------------------------------------------
;;;
;;; assistant
;;;
;;; ------------------------------------------------------------
(require-package 'on-screen)
(on-screen-global-mode +1)

(add-hook 'linum-before-numbering-hook
	  (lambda ()
	    (set-face-foreground 'linum "#4B8DF8")))

;;; ------------------------------------------------------------
;;;
;;; modeline
;;;
;;; ------------------------------------------------------------
(require-package 'smart-mode-line)
(require-package 'smart-mode-line-powerline-theme)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'powerline)
(setq powerline-default-separator 'arrow-fade)
(setq sml/mode-width 5)
(add-to-list 'sml/replacer-regexp-list '("^:ED:gtd/" ":GTD:") t)

(require-package 'spacemacs-theme)
(require-package 'spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(setq spaceline-minor-modes-separator nil)
(set-face-attribute 'mode-line nil  :height 100)

(require-package 'diminish)
(eval-after-load "highlight-tail" '(diminish 'highlight-tail-mode))
(eval-after-load "aggressive-indent" '(diminish 'aggressive-indent-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "hideshow" '(diminish 'hs-minor-mode "ⓗs"))
(eval-after-load "paredit" '(diminish 'paredit-mode "ⓟe"))
(eval-after-load "eldoc" '(diminish 'eldoc-mode "ⓔl"))

;;; adjustment for vc-mode modeline face
(set-face-attribute 'sml/filename nil :background (face-attribute 'powerline-active1 :background))
(set-face-attribute 'sml/vc nil :background (face-attribute 'mode-line :background))
(set-face-attribute 'sml/vc nil :foreground "lawn green")
(set-face-attribute 'sml/vc-edited nil :background (face-attribute 'mode-line :background))
(set-face-attribute 'sml/vc-edited nil :foreground "red3")

;;; ------------------------------------------------------------
;;;
;;; window
;;;
;;; ------------------------------------------------------------
;; (require-package 'popwin)
;; (require 'popwin)
;; (popwin-mode 1)
;; ;; | Key    | Command                               |
;; ;; |--------+---------------------------------------|
;; ;; | b      | popwin:popup-buffer                   |
;; ;; | l      | popwin:popup-last-buffer              |
;; ;; | o      | popwin:display-buffer                 |
;; ;; | C-b    | popwin:switch-to-last-buffer          |
;; ;; | C-p    | popwin:original-pop-to-last-buffer    |
;; ;; | C-o    | popwin:original-display-last-buffer   |
;; ;; | SPC    | popwin:select-popup-window            |
;; ;; | s      | popwin:stick-popup-window             |
;; ;; | 0      | popwin:close-popup-window             |
;; ;; | f, C-f | popwin:find-file                      |
;; ;; | e      | popwin:messages                       |
;; ;; | C-u    | popwin:universal-display              |
;; ;; | 1      | popwin:one-window                     |

;; (push '("*Backtrace*" :height 15) popwin:special-display-config)
;; (push '("*Python*" :position bottom :height 20) popwin:special-display-config)
;; (push '("*jedi:doc*" :position bottom :height 20) popwin:special-display-config)
;; (push '("*Warnings*" :position bottom :height 20) popwin:special-display-config)
;; (push '("*Org Agenda*" :position bottom :height 20) popwin:special-display-config)
;; (push '("* Agenda Commands*" :position bottom :height 20) popwin:special-display-config)
;; (push '("*GEBEN<127.0.0.1:9000> output*" :position bottom :height 20) popwin:special-display-config)
;; (push '("*GEBEN<127.0.0.1:9000> context*" :position bottom :width 20) popwin:special-display-config)
;; (push '("*buffer selection*" :position bottom :width 20) popwin:special-display-config)
;; (push '("*SPEEDBAR*" :position left :width 20) popwin:special-display-config)
;; (push '("*Help*" :position bottom :width 20) popwin:special-display-config)
;; (push '("*js*" :position bottom :width 20) popwin:special-display-config)

;; (global-set-key (kbd "C-z p") popwin:keymap)

;; ;;; http://www.emacswiki.org/emacs/TransposeWindows
;; (defun swap-window-positions (&optional arg)         ; Stephen Gildea
;;    "*Swap the positions of this window and the next one."
;;    (interactive "p")
;;    (let ((other-window (next-window (selected-window) 'no-minibuf)))
;;      (let ((other-window-buffer (window-buffer other-window))
;;            (other-window-hscroll (window-hscroll other-window))
;;            (other-window-point (window-point other-window))
;;            (other-window-start (window-start other-window)))
;;        (set-window-buffer other-window (current-buffer))
;;        (set-window-hscroll other-window (window-hscroll (selected-window)))
;;        (set-window-point other-window (point))
;;        (set-window-start other-window (window-start (selected-window)))
;;        (set-window-buffer (selected-window) other-window-buffer)
;;        (set-window-hscroll (selected-window) other-window-hscroll)
;;        (set-window-point (selected-window) other-window-point)
;;        (set-window-start (selected-window) other-window-start))
;;      ;;(select-window other-window)
;;      (if (= 4 arg)
;; 	 (select-window other-window))
;;      )
;;    )

(global-set-key (kbd "C-x \\") 'swap-window-positions)
(global-set-key (kbd "C-x |") 'toggle-window-split)

;;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun z/pop-window-into-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(defun z/toggle-window-dedicated ()
  (interactive)
  (unless (boundp 'z/window-dedicated-p)
    (make-local-variable 'z/window-dedicated-p)
    (setq z/window-dedicated-p nil)
    )
  
  (if z/window-dedicated-p
      (progn
	(set-window-dedicated-p (selected-window) nil)
	(setq z/window-dedicated-p nil)
	(message "window is not dedicated")
	)
    (set-window-dedicated-p (selected-window) t)
    (setq z/window-dedicated-p t)
    (message "window is dedicated")
    )
  )

;;; ------------------------------------------------------------
;;;
;;; utility
;;;
;;; ------------------------------------------------------------
(defun bibo/use-buffer-face-mode-with-fontfamily (fontfamily) 
  (setq buffer-face-mode-face `(:font ,(font-spec :family fontfamily)))
  (buffer-face-mode)
  )

(defun bibo/timestamp-format-setting ()
  (set (make-local-variable 'system-time-locale) "C")
  (set (make-local-variable 'system-messages-locale) "C")
  )

(provide 'init-ui)
