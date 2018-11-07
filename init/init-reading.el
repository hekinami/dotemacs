(use-package irfc
  :ensure t
  :defer t
  :config
  (setq irfc-directory (concat (z/get-runtimes-dir) (file-name-as-directory "RFC")))
  (setq irfc-assoc-mode t))

(use-package xkcd
  :ensure t
  :defer t
  :config
  (setq xkcd-cache-dir (concat (z/get-runtimes-dir) "xkcd"))
  (setq xkcd-cache-latest (concat (z/get-runtimes-dir) "xkcd/latest")))

(use-package hackernews
  :ensure t
  :commands (hackernews)
  :config
  (setq hackernews-visited-links-file (concat (z/get-runtimes-dir) "hackernews/visited-links.el")))

(use-package elfeed
  :ensure t
  :commands (elfeed)
  :config
  (use-package elfeed-org
    :ensure t
    :config
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))
  (use-package elfeed-goodies
    :ensure t
    :config
    (elfeed-goodies/setup)))

;;; ------------------------------------------------------------
;;;
;;; xwidget webkit
;;;
;;; ------------------------------------------------------------
(use-package xwidget
  :bind
  (:map xwidget-webkit-mode-map
        ("<mouse-5>" . xwidget-webkit-scroll-up)
        ("<mouse-4>" . xwidget-webkit-scroll-down)))

(use-package justify-kp
  :quelpa (justify-kp :fetcher github :repo "Fuco1/justify-kp"))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat (z/get-runtimes-dir) "nov-places"))
  (require 'justify-kp)
  (setq nov-text-width most-positive-fixnum)

  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.3)
    )
  (add-hook 'nov-mode-hook 'my-nov-font-setup)

  (defun my-nov-window-configuration-change-hook ()
    (my-nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
                 'my-nov-window-configuration-change-hook
                 t))

  (setq window-size-change-functions #'my-nov-window-configuration-change-hook)

  (defun my-nov-post-html-render-hook ()
    (if (get-buffer-window)
        (let ((max-width (pj-line-width))
              buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (not (looking-at "^[[:space:]]*$"))
                (goto-char (line-end-position))
                (when (> (shr-pixel-column) max-width)
                  (goto-char (line-beginning-position))
                  (pj-justify)))
              (forward-line 1))))
      ))

  (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook))

(provide 'init-reading)
