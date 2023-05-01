(setq user-email-address "lucazanny@gmail.com") ; me

;; Set custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; use-package init
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "https" "http")))
  (when no-ssl (warn "No ssl! MITM possibili!"))
  (add-to-list 'package-archives
               (cons 
                "melpa" (concat proto "://melpa.org/packages/")) 
               t))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Behaviour
(global-set-key (kbd "C-x C-b") 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)

(ido-mode 1)

(if (daemonp)
    (global-set-key (kbd "C-x C-c") 'delete-frame)
  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Themes and visual components
(global-hl-line-mode 1)
(display-battery-mode 1)
(blink-cursor-mode 0)

(if (not (version< emacs-version "26.0"))
    (progn
      (global-display-line-numbers-mode t)
      (setq display-line-numbers-type 'relative))
  (global-linum-mode t))

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

(setq lz/frame-settings
      '((width . 100)
	(height . 30)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
	(font . "Space Mono-14")))

(setf initial-frame-alist lz/frame-settings)
(setf default-frame-alist lz/frame-settings)


(show-paren-mode 1)
(setq show-paren-style 'mixed)
(display-time-mode 1)

(add-to-list 'custom-theme-load-path
	     (concat user-emacs-directory "themes/everforest-theme"))

(use-package ef-themes :ensure)

(use-package all-the-icons :ensure)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solo-jazz-theme :ensure)

(use-package base16-theme :ensure)

;; AGDA
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(use-package auto-dark
  :ensure
  :config
  (customize-set-variable 'auto-dark-dark-theme 'modus-vivendi)
  (customize-set-variable 'auto-dark-light-theme 'modus-operandi)
  (auto-dark-mode))

(use-package htmlize :ensure)

(use-package magit :ensure)

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-c C-SPC") 'er/expand-region))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t)

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
   '((t basic))))

;; Language packs
(use-package clojure-mode :ensure)
(use-package rust-mode
  :ensure
  :config
  (setq rust-format-on-save t))
(use-package typescript-mode :ensure)
(use-package markdown-mode :ensure)
(use-package erlang :ensure)
(use-package haskell-mode :ensure)

(use-package go-mode
  :ensure
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))


(use-package slime
  :ensure
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package yaml-mode
  :ensure
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Language server pack support
;; (use-package lsp-mode
;;   :ensure t 
;;   :commands (lsp lsp-deferred)
;;   :config
;;   (lsp-enable-which-key-integration t)
;;   (setq gc-cons-threshold 100000000)	; to make the gc run
;; 					; sporadically
;;   (setq read-process-output-max (* 1024 1024)) 
;;   :hook
;;   ((go-mode) . lsp))


;; ORG ROAM, studio
(defvar lz/biblio-dir "~/biblio")
(use-package org-roam
  :ensure
  :config
  (setq org-roam-directory (file-truename lz/biblio-dir))
  (org-roam-db-autosync-mode)
  (org-roam-setup)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)))

;; HELM
(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-b") #'helm-buffers-list)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-j") 'helm-next-line)

  (setq helm-multi-edit-save t)
  (setq helm-split-with-multiple-windows t)
  (setq helm-split-direction 'split-window-vertically)
  (setq helm-speed-or-color t)
  (setq helm-move-to-line-cycle t)
  (setq helm-use-line-number-face t)
  (setq helm-use-fuzzy-match t)
  (setq helm-ff-default-directory (getenv "HOME")))

;; tra i keybind di helm-swoop (define-key evil-motion-state-map (kbd
;; "C-c C-s") 'helm-swoop-from-evil-search). Also tolto perchè isearch
;; è più flessibile/leggero
;; 
(use-package helm-swoop
  :ensure t
  :config
  ;; (global-set-key (kbd "C-s") 'helm-swoop)
  (global-set-key (kbd "M-S") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c C-s") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-s") 'helm-multi-swoop-all)

  (define-key helm-swoop-map (kbd "C-s") 'helm-multi-swoop-all-from-helm-swoop)
  (define-key helm-swoop-map (kbd "C-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  (define-key helm-swoop-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-j") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-j") 'helm-next-line)

  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-speed-or-color t)
  (setq helm-swoop-move-to-line-cycle t)
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-use-fuzzy-match t))

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
      org-latex-listings 'minted)

;; site setup
(setf org-html-doctype "html5"
      org-html-head-include-default-style nil)

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
	 :html-preamble "<nav><a id=\"navbar-home\" href=\"../index.html\">🏡 Home</a></nav>"
	 :html-link-home "../index.html")
	
	("style"
         :base-directory "~/src/sito/"
         :base-extension "css"
         :publishing-directory "~/src/lusergit.github.io/"
         :publishing-function org-publish-attachment)
	
        ("website" :components ("index" "images" "posts" "style"))))

;; Org babel languages (adding shell)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((eshell . t)))

;; Org agenda
;; Cartelle in cui guardare i file: uni e src
(setq org-agenda-files '("~/uni"))
(setq org-agenda-span 'month)


;; Functions
(defun lz/open-configs ()
  "Funzione per aprire il file di configurazione"
  (interactive)
  (find-file 
   (expand-file-name 
    (concat user-emacs-directory "init.el")))) 

(defun lz/silent-compile ()
  "Run compile in a silent buffer, not displaying it"
  (interactive)
  (let* ((bname "*compilation*"))
    (progn
      (if (get-buffer bname)
  	  (progn
  	    (delete-windows-on (get-buffer bname))
  	    (kill-buffer bname)))
      (let* ((other-win (split-window-vertically))
	     (get-buffer-create bname))
	(call-interactively 'compile)
	(delete-window other-win)
	(delete-windows-on (get-buffer bname))
	(message "Compiling")))))

(defun lz/latex-save-and-compile ()
  "A macro function to save and compile with one step"
  (interactive)
  (save-buffer)
  (lz/silent-compile))

(require 'tex-mode)
(define-key latex-mode-map (kbd "M-s M-s") 'lz/latex-save-and-compile)

(defun lz/save-export-scompile ()
  " Macro function to save the currently working org document,
export it to latex and compile it to pdf
"
  (interactive)
  (save-buffer)
  (org-latex-export-to-latex)
  (lz/silent-compile))

(defun lz/save-beamer-scompile ()
  " Macro function to save the currently working org document,
export it to latex and compile it to pdf
"
  (interactive)
  (save-buffer)
  (org-beamer-export-to-latex)
  (lz/silent-compile))

(define-key org-mode-map (kbd "M-s M-s") 'lz/save-export-scompile)
(define-key org-mode-map (kbd "M-s M-b") 'lz/save-beamer-scompile)

(set-default 'preview-scale-function 2)

(defun lz/next-buffer-other-window (&optional arg interactive)
  "In other window switch to ARGth next buffer.
Call `switch-to-next-buffer' unless the selected window is the
minibuffer window or is dedicated to its buffer."
  (interactive "p\np")
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (select-window other)
    (next-buffer arg interactive)
    (select-window current)))

(defun lz/previous-buffer-other-window (&optional arg interactive)
  "In other window switch to ARGth previous buffer.
Call `switch-to-prev-buffer' unless the selected window is the
minibuffer window or is dedicated to its buffer."
  (interactive "p\np")
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (select-window other)
    (previous-buffer arg interactive)
    (select-window current)))

(defun lz/ff-other-window ()
  "Find file in other window."
  (interactive)
  (cond
   ((one-window-p t)
    (call-interactively #'find-file-other-window))
   (t
    (let ((other (other-window-for-scrolling))
          (current (selected-window)))
      (select-window other)
      (call-interactively #'find-file)
      (select-window current)))))

(defun lz/kill-buffer-other-window ()
  "Kills buffer in other window."
  (interactive)
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (select-window other)
    (kill-buffer)
    (select-window current)))

(defun lz/shell-vertically-split (&optional shell-type)
  "Opens a shell in a vertically split window, if given 'eshell
as argument starts a new eshell, 'term starts a new term and
'ansi-term a new ansi-term"
  (interactive)
  (let ((other (split-window-below))
	(current (selected-window))
	(term-here (lambda ()
		     (let* ((current (selected-window))
			    (shell-buffer (shell)))
		       (delete-window (get-buffer-window shell-buffer))
		       (select-window current)
		       (switch-to-buffer shell-buffer)))))
    (select-window other)
    (if (boundp shell-type)
	(cond ((= shell-type 'eshell) (eshell))
	      ((= shell-type 'term) (term))
	      ((= shell-type 'ansi-term) (ansi-term))
	      (t (funcall term-here)))
      (funcall term-here))
    (select-window current)))

;; Color mode for terms
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

;; My modules
(add-to-list 'load-path (concat user-emacs-directory "modules/"))
(require 'lofi)
;; (require 'yt-play)
;; (require 'splash)
;; (require 'pomo)
;; (require 'themess)

;; (setq initial-buffer-choice #'lz/splash-screen)
;; (add-hook 'server-after-make-frame-hook #'lz/populate-splash-screen)
(add-hook 'server-after-make-frame-hook #'org-agenda-list)
(add-hook 'after-init-hook #'org-agenda-list)
(setq initial-buffer-choice #'(lambda () (get-buffer "*Org Agenda*")))
