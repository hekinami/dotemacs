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
;;; fonts
;;;
;;; ------------------------------------------------------------
(use-package cnfonts
  :defer t
  :init
  (require-package 'cnfonts)
  (cnfonts-enable))

;;; ------------------------------------------------------------
;;;
;;; frame
;;;
;;; ------------------------------------------------------------
(setq frame-title-format "[%F]")

(setq init-frame-alist
      (append
       `((height . 25)
         (width . 100)) default-frame-alist))

(setq default-frame-alist
      (append
       `((height . 25)
         (width . 100)) default-frame-alist))

(global-set-key (kbd "C-x C-a f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-x C-a m") 'toggle-frame-maximized)

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
(if (daemonp) nil (add-hook 'after-init-hook 'highlight-tail-mode))
(use-package highlight-tail
  :defer t
  :init
  (require-package 'highlight-tail)
  (add-hook 'after-init-hook (lambda nil
                               (highlight-tail-mode)
                               (diminish 'highlight-tail-mode)))
  :config
  (setq highlight-tail-timer 0.01))

(blink-cursor-mode 1)
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
(add-hook 'after-init-hook 'global-yascroll-bar-mode)
(use-package yascroll
  :defer t
  :init
  (require-package 'yascroll))
(scroll-bar-mode -1)

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
(add-hook 'after-init-hook 'sml/setup)
(use-package smart-mode-line
  :defer t
  :init
  (require-package 'smart-mode-line)
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/mode-width 5)
  (add-to-list 'sml/replacer-regexp-list '("^:ED:gtd/" ":GTD:") t)

  (use-package smart-mode-line-powerline-theme
    :defer t
    :init
    (require-package 'smart-mode-line-powerline-theme)
    (sml/apply-theme 'powerline)
    :config
    (setq powerline-default-separator 'arrow-fade))
  
  (use-package spacemacs-theme
    :defer t
    :init
    (require-package 'spacemacs-theme))
  
  (use-package spaceline
    :defer t
    :init
    (require-package 'spaceline) 
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    (add-hook 'spaceline-pre-hook (lambda nil
				    (set-face-attribute 'mode-line nil  :height 100)
				    (set-face-attribute 'sml/filename nil :background (face-attribute 'powerline-active1 :background))
				    (set-face-attribute 'sml/vc nil :background (face-attribute 'mode-line :background))
				    (set-face-attribute 'sml/vc nil :foreground "lawn green")
				    (set-face-attribute 'sml/vc-edited nil :background (face-attribute 'mode-line :background))
				    (set-face-attribute 'sml/vc-edited nil :foreground "red")
				    ))
    :config
    (setq spaceline-minor-modes-separator nil)
    )
  )

(use-package diminish
  :init
  (require-package 'diminish)
  :config
  (eval-after-load "aggressive-indent" '(diminish 'aggressive-indent-mode "ⅈ"))
  (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
  (eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
  (eval-after-load "hideshow" '(diminish 'hs-minor-mode "🅢"))
  (eval-after-load "paredit" '(diminish 'paredit-mode))
  (eval-after-load "which-key" '(diminish 'which-key-mode))
  (eval-after-load "helm-mode" '(diminish 'helm-mode))
  (eval-after-load "buffer-face-mode" '(diminish 'buffer-face-mode))
  (eval-after-load "eldoc" '(diminish 'eldoc-mode)))

;;; ------------------------------------------------------------
;;;
;;; window
;;;
;;; ------------------------------------------------------------
(add-hook 'after-init-hook 'popwin-mode)
(use-package popwin
  ;; | Key    | Command                               |
  ;; |--------+---------------------------------------|
  ;; | b      | popwin:popup-buffer                   |
  ;; | l      | popwin:popup-last-buffer              |
  ;; | o      | popwin:display-buffer                 |
  ;; | C-b    | popwin:switch-to-last-buffer          |
  ;; | C-p    | popwin:original-pop-to-last-buffer    |
  ;; | C-o    | popwin:original-display-last-buffer   |
  ;; | SPC    | popwin:select-popup-window            |
  ;; | s      | popwin:stick-popup-window             |
  ;; | 0      | popwin:close-popup-window             |
  ;; | f, C-f | popwin:find-file                      |
  ;; | e      | popwin:messages                       |
  ;; | C-u    | popwin:universal-display              |
  ;; | 1      | popwin:one-window                     |
  :init
  (require-package 'popwin)
  :config
  (push '("*Backtrace*" :height 15) popwin:special-display-config)
  (push '("*Python*" :position bottom :height 20) popwin:special-display-config)
  (push '("*jedi:doc*" :position bottom :height 20) popwin:special-display-config)
  (push '("*Warnings*" :position bottom :height 20) popwin:special-display-config)
  (push '("*Org Agenda*" :position bottom :height 20) popwin:special-display-config)
  (push '("* Agenda Commands*" :position bottom :height 20) popwin:special-display-config)
  (push '("*GEBEN<127.0.0.1:9000> output*" :position bottom :height 20) popwin:special-display-config)
  (push '("*GEBEN<127.0.0.1:9000> context*" :position bottom :width 20) popwin:special-display-config)
  (push '("*buffer selection*" :position bottom :width 20) popwin:special-display-config)
  (push '("*SPEEDBAR*" :position left :width 20) popwin:special-display-config)
  (push '("*Help*" :position bottom :width 20) popwin:special-display-config)
  (push '("*js*" :position bottom :width 20) popwin:special-display-config)
  (bind-key "C-z p" popwin:keymap)
  )

;;; http://www.emacswiki.org/emacs/TransposeWindows
(defun swap-window-positions (&optional arg)         ; Stephen Gildea
  "*Swap the positions of this window and the next one."
  (interactive "p")
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
          (other-window-hscroll (window-hscroll other-window))
          (other-window-point (window-point other-window))
          (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    ;;(select-window other-window)
    (if (= 4 arg)
        (select-window other-window))
    )
  )

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
(defun bibo/timestamp-format-setting ()
  (set (make-local-variable 'system-time-locale) "C")
  (set (make-local-variable 'system-messages-locale) "C")
  )

;;; ------------------------------------------------------------
;;;
;;; persp-mode
;;;
;;; ------------------------------------------------------------
(use-package persp-mode
  :bind (("C-x k" . persp-kill-buffer)
         ("C-x b" . persp-switch-to-buffer)) ; may overwrite by helm loading, workaround needed in helm config
  :init
  (require-package 'persp-mode)
  (setq persp-save-dir (concat (bibo/get-runtimes-dir) "persp-confs/"))
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))
  :config
  ;; eshell
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))
  )

(provide 'init-ui)
