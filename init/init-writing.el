(use-package cobalt
  :ensure t)

(defun z/publish-blog nil
  (interactive)
  (let* ((blog-path "~/git/cobalt.amiunique.net/")
         (cobalt-site-paths '(blog-path))
         (cobalt--current-site blog-path)
         (org-publish-project-alist
          `(("cobalt-posts"
             :base-directory ,(concat blog-path "_org")
             :publishing-directory ,(concat blog-path "posts")
             :publishing-function org-html-publish-to-html
             :section-numbers nil
             :with-toc nil
             :body-only t
             )
            ("cobalt-images"
             :base-directory ,(concat blog-path "_org/images")
             :base-extension "jpg\\|gif\\|png"
             :publishing-directory ,(concat blog-path "posts/images")
             :publishing-function org-publish-attachment)
            ("cobalt" :components ("cobalt-posts" "cobalt-images"))
            ))
         )

    (org-publish-project "cobalt")
    (cobalt-build nil)
    (magit-status blog-path)
    )
  )

(provide 'init-writing)
