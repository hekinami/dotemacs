(setq inhibit-default-init t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq package-enable-at-startup nil)
(setq frame-title-format "[%F]")

;; (setq init-frame-alist
;;       (append
;;        `((height . 25)
;;          (width . 100)) default-frame-alist))

(setq default-frame-alist
      (append
       `((height . 25)
         (width . 100)) default-frame-alist))
