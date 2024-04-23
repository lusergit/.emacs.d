;;; package -- Site

;;; Commentary:
;; The following code configures org mode to publish the files in
;; ~/src/sito/ in ~/src/lusergit.github.io/

;;; Code:
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
	 :html-home/up-format "<nav id=\"org-div-home-and-up\"><a href=\"%s\">up</a>&nbsp|&nbsp<a href=\"%s\">home</a></nav>"
	 :html-link-home "../index.html"
	 :html-link-up "index.html"
	 :section-numbers nil
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\"/>")
	
	("style"
         :base-directory "~/src/sito/"
         :base-extension "css"
         :publishing-directory "~/src/lusergit.github.io/"
         :publishing-function org-publish-attachment)

	("pdfs"
	 :base-directory "~/src/sito/"
	 :base-extension "pdf"
         :publishing-directory "~/src/lusergit.github.io/"
	 :publishing-function org-publish-attachment)
	
        ("website" :components ("index" "images" "posts" "style" "pdfs"))))

(provide 'site)
;;; site.el ends here
