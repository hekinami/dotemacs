(use-package emms
  :defer t
  :init
  (require-package 'emms)
  (setq default-process-coding-system '(utf-8 . utf-8))
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  )

(provide 'init-media)
