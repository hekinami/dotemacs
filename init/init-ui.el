;;; ------------------------------------------------------------
;;;
;;; theme
;;;
;;; ------------------------------------------------------------
(use-package molokai-theme
  :ensure t
  :config
  (load-theme 'molokai t)
  (setq z/current-theme-name "molokai"))

;;; ------------------------------------------------------------
;;;
;;; fonts
;;;
;;; ------------------------------------------------------------
(use-package cnfonts
  :ensure t
  :config
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

(use-package frame
  :bind (("C-x C-a f" . toggle-frame-fullscreen)
         ("C-x C-a m" . toggle-frame-maximized)))

(use-package z-ui-extension
  :bind (("C-x C-a t" . z/toggle-transparency)
         ("C-x \\" . z/swap-window-positions)
         ("C-x |" . z/toggle-window-split)))

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
  :ensure t
  :bind ("C-z s" . sr-speedbar-toggle))

;;; ------------------------------------------------------------
;;;
;;; cursor
;;;
;;; ------------------------------------------------------------
;; (use-package highlight-tail
;;   :ensure t
;;   :config
;;   (highlight-tail-mode)
;;   (setq highlight-tail-timer 0.01)
;;   (diminish 'highlight-tail-mode))

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
(use-package yascroll
  :ensure t
  :config
  (scroll-bar-mode -1)
  (global-yascroll-bar-mode))


;;; ------------------------------------------------------------
;;;
;;; assistant
;;;
;;; ------------------------------------------------------------
(use-package on-screen
  :ensure t
  :config
  (on-screen-global-mode +1))

(use-package linum
  :defer t
  :config
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (set-face-foreground 'linum "#4B8DF8"))))

;;; ------------------------------------------------------------
;;;
;;; modeline
;;;
;;; ------------------------------------------------------------
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (setq sml/mode-width 5)
  (add-to-list 'sml/replacer-regexp-list '("^:ED:gtd/" ":GTD:") t))

(use-package smart-mode-line-powerline-theme
  :ensure t
  :config
  (sml/apply-theme 'powerline)
  (setq powerline-default-separator 'arrow-fade))

(require-package 'spacemacs-theme)      ;use-package don't work, why?

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (add-hook
   'spaceline-pre-hook
   (lambda nil
     (set-face-attribute 'mode-line nil  :height 100)
     (set-face-attribute 'sml/filename nil :background (face-attribute 'powerline-active1 :background))
     (set-face-attribute 'sml/vc nil :background (face-attribute 'mode-line :background))
     (set-face-attribute 'sml/vc nil :foreground "lawn green")
     (set-face-attribute 'sml/vc-edited nil :background (face-attribute 'mode-line :background))
     (set-face-attribute 'sml/vc-edited nil :foreground "red")
     ))
  (setq spaceline-minor-modes-separator nil))

(use-package diminish
  :ensure t
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

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode))
;;; ------------------------------------------------------------
;;;
;;; window
;;;
;;; ------------------------------------------------------------
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
  :ensure t
  :bind 
  :config
  (popwin-mode)
  (bind-key "C-z p" popwin:keymap)
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
  (push '("*js*" :position bottom :width 20) popwin:special-display-config))


;;; ------------------------------------------------------------
;;;
;;; utility
;;;
;;; ------------------------------------------------------------
(defun z/timestamp-format-setting ()
  (set (make-local-variable 'system-time-locale) "C")
  (set (make-local-variable 'system-messages-locale) "C")
  )

;;; ------------------------------------------------------------
;;;
;;; persp-mode
;;;
;;; ------------------------------------------------------------
(use-package persp-mode
  :ensure t
  :bind (("C-x k" . persp-kill-buffer)
         ("C-x b" . persp-switch-to-buffer)) ; may overwrite by helm loading, workaround needed in helm config
  :init
  (setq persp-save-dir (concat (z/get-runtimes-dir) "persp-confs/")) 
  (persp-mode 1)
  ;; :config
  ;; eshell
  ;; (persp-def-buffer-save/load
  ;;  :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
  ;;  :save-vars '(major-mode default-directory))
  )

(provide 'init-ui)
