;;; ------------------------------------------------------------
;;;
;;; theme
;;;
;;; ------------------------------------------------------------
(require-package 'molokai-theme)
(load-theme 'molokai t)

;;; ------------------------------------------------------------
;;;
;;; frame
;;;
;;; ------------------------------------------------------------
(setq frame-title-format "[%F]")

(when *is-windows*
  (set-frame-font "Courier New-12")
  (let ((fontset (frame-parameter nil 'font)))
    (set-fontset-font fontset 'gb18030 (font-spec :family "YaHei Consolas Hybrid"))
    (set-fontset-font fontset 'kana (font-spec :family "MS Gothic"))
    ))

(when *is-linux*
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

;;; ------------------------------------------------------------
;;;
;;; miscellaneous
;;;
;;; ------------------------------------------------------------
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
(column-number-mode 1)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(global-set-key (kbd "<f10>") 'menu-bar-mode)

;;; ------------------------------------------------------------
;;;
;;; cursor
;;;
;;; ------------------------------------------------------------
(require-package 'highlight-tail)
(setq highlight-tail-timer 0.01)
(highlight-tail-mode 1)

(setq blink-cursor-blinks 0)

;;; ------------------------------------------------------------
;;;
;;; scrollbar
;;;
;;; ------------------------------------------------------------
(require-package 'yascroll)
(scroll-bar-mode -1)
(global-yascroll-bar-mode)

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
(require-package 'spacemacs-theme)
(require-package 'spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(setq spaceline-minor-modes-separator nil)

(require-package 'smart-mode-line)
(require-package 'smart-mode-line-powerline-theme)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'powerline)
(setq powerline-default-separator 'wave)
(setq sml/mode-width 5)
(add-to-list 'sml/replacer-regexp-list '("^:ED:gtd/" ":GTD:") t)

(require-package 'diminish)
(eval-after-load "highlight-tail" '(diminish 'highlight-tail-mode))
(eval-after-load "aggressive-indent" '(diminish 'aggressive-indent-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "hideshow" '(diminish 'hs-minor-mode "ⓗs"))
(eval-after-load "paredit" '(diminish 'paredit-mode "ⓟe"))
(eval-after-load "eldoc" '(diminish 'eldoc-mode "ⓔl"))

;;; ------------------------------------------------------------
;;;
;;; window
;;;
;;; ------------------------------------------------------------
(require-package 'popwin)
(require 'popwin)
(popwin-mode 1)
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

(provide 'init-ui)
