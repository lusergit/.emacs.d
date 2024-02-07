
(setq org-publish-project-alist
      '(("index"
         :base-directory "~/src/sito/"
         :base-extension "org"
         :publishing-directory "~/src/lusergit.github.io/"
         :publishing-function org-html-publish-to-html
	 :html-head-include-default-style nil
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/>"
         :html-preamble t)

        ("images"
         :base-directory "~/src/sito/posts/imgs/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "~/src/lusergit.github.io/posts/imgs"
         :publishing-function org-publish-attachment)

        ("posts"
         :base-directory "~/src/sito/posts/"
         :base-extension "org"
         :publishing-directory "~/src/lusergit.github.io/posts/"
         :publishing-function org-html-publish-to-html
	 :html-head-include-default-style nil
	 :section-numbers nil
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\"/>"
	 :html-preamble "<nav><a id=\"navbar-home\" href=\"../index.html\">🏡 Home</a></nav>")
	
	("style"
         :base-directory "~/src/sito/"
         :base-extension "css"
         :publishing-directory "~/src/lusergit.github.io/"
         :publishing-function org-publish-attachment)
	
        ("website" :components ("index" "images" "posts" "style"))))

(provide 'site)
