(use-package cobalt
  :ensure t
  :config
  (setq cobalt-posts-org-source (locate-contents-file "earl/posts.amiunique.net"))
  (setq cobalt-source (locate-contents-file "earl/cobalt.amiunique.net"))
  (setq cobalt-dest-base (locate-contents-file "earl/hekinami.gitlab.io"))
  (setq cobalt-site-paths '(cobalt-source))
  (setq cobalt--current-site cobalt-source)

  (defun cobalt-generate-posts-source-from-org ()
    ""
    (interactive)
    (let* ((org-publish-project-alist
            `(("cobalt-posts"
               :base-directory ,cobalt-posts-org-source
               :publishing-directory ,(concat cobalt-source "/posts")
               :publishing-function org-html-publish-to-html
               :section-numbers nil
               :with-toc nil
               :body-only t
               )
              ("cobalt-post-images"
               :base-directory ,(concat cobalt-posts-org-source "/images")
               :base-extension "jpg\\|gif\\|png"
               :publishing-directory ,(concat cobalt-source "/posts/images")
               :publishing-function org-publish-attachment)
              ("cobalt" :components ("cobalt-posts" "cobalt-post-images"))
              ))
           )

      (org-publish-project "cobalt")
      )
    )

  (defun cobalt-build-with-posts-from-org ()
    ""
    (interactive)
    (cobalt-generate-posts-source-from-org)
    (cobalt-build nil)
    )

  (defun cobalt-deploy ()
    ""
    (interactive)
    (cobalt-build-with-posts-from-org)
    (magit-status cobalt-dest-base)
    )
  )


(provide 'init-writing)
