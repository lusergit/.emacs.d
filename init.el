;;; Init -- emacs init

;;; Commentary:
;; Standard Emacs init file, initializing package.el and then loading
;; modules (elisp code)

;;; Code:
(setq user-email-address "lucazanny@gmail.com")

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
               (cons "melpa" (concat proto "://melpa.org/packages/"))
               t))
(package-initialize)
(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
(require 'languages)
(require 'site)
(require 'latex-custom)
(require 'splash)
(require 'treesit)
(require 'interactions)
(require 'site)
(require 'tesi)
(require 'snippets-custom)
(require 'metalang)

(provide 'init)
;;; init.el ends here
