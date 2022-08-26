; me
(setq user-email-address "lucazanny@gmail.com")

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
	(font . "Monospace 14")))

(setq initial-frame-alist lz/frame-settings)
(setq default-frame-alist lz/frame-settings)

(show-paren-mode 1)
(setq show-paren-style 'mixed)

(use-package ef-themes :ensure)
(use-package modus-themes :ensure
  :config
  (load-theme 'modus-vivendi t))

;; Packs
(use-package magit :ensure)
;; (use-package evil :ensure :config (evil-mode 1))

;; Customs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(chart-face-color-list
   '("#b52c2c" "#4fd100" "#f1e00a" "#2fafef" "#bf94fe" "#47dfea" "#702020" "#007800" "#b08600" "#1f2f8f" "#5f509f" "#00808f"))
 '(custom-safe-themes
   '("90a6936b8c8f709825a0165928ef95f24f79486805be787630e7fe46bc5c9c7f" "47772b7cb9a4d978fa72eb1e895b0d84ecfc0baa670ab539e64c43135eeec79c" default))
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil)
 '(package-selected-packages '(evil magit modus-themes ef-themes use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
