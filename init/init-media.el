(use-package emms
  :ensure t
  :defer t
  :init
  (setq default-process-coding-system '(utf-8 . utf-8))
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players))

(provide 'init-media)
