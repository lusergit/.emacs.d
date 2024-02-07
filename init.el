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

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; MODULES
(add-to-list 'load-path (concat user-emacs-directory "modules/"))
(require 'window)
(require 'modeline)
(require 'lofi)
(require 'proverif)
(require 'music-chord)
(require 'webdev)
(require 'git)
(require 'themess)
(require 'org-custom)
(require 'languages)
(require 'site)
(require 'latex-custom)
(require 'splash)
(require 'treesit)

;; AGDA (aggiunto da agda stesso quindi non sposto)
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
