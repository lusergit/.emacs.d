;;; Org-custom -- org mode configurations
;;; Commentary:
;; pdf tools and agenda settings mostly

;;; Code:
(use-package pdf-tools
  :ensure
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook #'(lambda () (display-line-numbers-mode -1)))
  (setq-default pdf-view-display-size 'fit-page))

(use-package org-pdftools
  :ensure t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter
  :ensure t
  :config
  (require 'org-noter-pdftools)
  (setq org-noter-auto-save-last-location t))

(use-package org-noter-pdftools
  :after org-noter
  :ensure t
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))
  
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location
		      (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org
  :custom
  (org-cite-global-bibliography
   '("/home/luca/globib.bib"))
  (org-cite-export-processors
   '((t basic)))
  :config
  (add-to-list 'org-src-lang-modes '("latex-macros" . latex))

  (defvar org-babel-default-header-args:latex-macros
    '((:results . "raw")
      (:exports . "results")))

  (defun prefix-all-lines (pre body)
    (with-temp-buffer
      (insert body)
      (string-insert-rectangle (point-min) (point-max) pre)
      (buffer-string)))

  (defun org-babel-execute:latex-macros (body _params)
    (concat
     (prefix-all-lines "#+LATEX_HEADER: " body)
     "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
     (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
     "\n#+HTML_HEAD_EXTRA: \\)</div>\n")))

(use-package org-tree-slide :ensure)

;; ORG ROAM, studio
(use-package org-roam
  :ensure
  :config

  (defcustom
    lz/biblio-dir
    "~/biblio"
    "Directory della bibliografia globale."
    :type 'string
    :group 'lz-custom)

  (setq org-roam-directory (file-truename lz/biblio-dir))
  (org-roam-db-autosync-mode)
  (org-roam-setup)

  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)))

;; Org configs
(setq org-format-latex-options
      '( :foreground default
	 :background default
	 :scale 2.0
	 :html-foreground "Black"
	 :html-background "Transparent"
	 :html-scale 1.0
	 :matchers
	 ("begin" "$1" "$" "$$" "\\(" "\\[")))

(setq org-startup-truncated nil
      org-hide-leading-stars t
      org-adapt-indentation t
      org-log-done 'time
      org-image-actual-width nil
      org-latex-caption-above nil
      org-agenda-tags-column -80
      org-list-allow-alphabetical t
      org-latex-create-formula-image-program 'imagemagick
      ;; org-agenda-view-columns-initially nil
      org-highlight-latex-and-related '(latex script entities)
      org-agenda-remove-tags t
      org-latex-listings 'minted)

;; org-latex-classes defined in ox-latex
(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("letter"
               "\\documentclass{letter}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;; site setup
(setf org-html-doctype "html5"
      org-html-head-include-default-style nil)

;; Org babel languages (adding shell)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((eshell . t)
   (lisp . t)))

;; Org agenda
(setq org-agenda-span 'day)

(defun lz/save-export-scompile ()
  "Macro function to save the currently working org document.
It will export it to latex and compile it to pdf"
  (interactive)
  (save-buffer)
  (org-latex-export-to-latex)
  (lz/silent-compile))

(defun lz/save-beamer-scompile ()
  "Macro function to save the currently working org document.
It will export it to latex and compile it to pdf"
  (interactive)
  (save-buffer)
  (org-beamer-export-to-latex)
  (lz/silent-compile))

(define-key org-mode-map (kbd "M-s M-s") 'lz/save-export-scompile)
(define-key org-mode-map (kbd "M-s M-b") 'lz/save-beamer-scompile)

(set-default 'preview-scale-function 2)

(defvar lz/task-files
  '("~/uni/interval_detail.org")
  "Files with the TODO tasks for a specific topic.")

(setq org-agenda-custom-commands
      '(("w" "This Week's Tasks"
	 ((agenda "" ((org-agenda-span 7)
		      (org-agenda-overriding-header "This Week")))
	  (tags-todo
	   "tesi+active+PRIORITY=\"A\""
	   ((org-agenda-files '("~/uni/interval_detail.org"))
	    (org-agenda-overriding-header "Ora")))
	  (tags-todo
	   "tesi+active+PRIORITY=\"C\""
	   ((org-agenda-files '("~/uni/interval_detail.org"))
	    (org-agenda-overriding-header "Poi")))
	  ))))

(add-hook 'server-after-make-frame-hook #'(lambda () (org-agenda nil "w")))
(add-hook 'after-init-hook #'(lambda () (org-agenda nil "w")))
(setq initial-buffer-choice #'(lambda () (get-buffer "*Org Agenda*")))

(provide 'org-custom)
;;; org-custom.el ends here
