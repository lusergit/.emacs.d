;;; Init -- emacs init

;;; Commentary:
;; Standard Emacs init file, initializing package.el and then loading
;; modules (elisp code)

;;; Code:

;; Set custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; use-package init
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
	    (not (gnutls-available-p))))
(proto (if no-ssl "https" "http")))
(when no-ssl (warn "No ssl!"))
(add-to-list 'package-archives
       (cons
	"melpa"
	(concat proto "://melpa.org/packages/")) t))
(package-initialize)
(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

;; also quelpa
(use-package quelpa :ensure t)

(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' EXECPATH and PATH environment.
Set the two in order to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(defgroup lz-group nil
  "Customization group for my custom settings."
  :group 'convenience)

(use-package
 emacs
 :ensure nil
 :custom

 ;; Should use:
 ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
 ;; at least once per installation or while changing this list
 (treesit-language-source-alist
  '((heex "https://github.com/phoenixframework/tree-sitter-heex")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir"))))

;; Make emacs use ambient variables, otherwise everything breaks!
(use-package exec-path-from-shell 
  :ensure t
  :config 
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; MODULES
(add-to-list 'load-path (concat user-emacs-directory "modules/"))
(require 'window-custom)
(require 'modeline)
(require 'lofi)
(require 'proverif)
(require 'music-chord)
(require 'git-custom)
(require 'themess)
(require 'org-custom)
(require 'site)
(require 'languages)
(require 'latex-custom)
;; (require 'splash)
;; (splash-quotes-mode t)
(require 'treesit)
(require 'interactions)
(require 'site)
(require 'tesi)
(require 'snippets-custom)
(require 'metalang)
(require 'dired-custom)
;; (require 'tabline)

;; more
;; (require 'telegram-custom)
(require 'matrix-custom)
(require 'comics)

(provide 'init)
;;; init.el ends here

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
