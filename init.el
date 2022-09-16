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
(setq display-time-day-and-date t)
(display-time-mode 1)

;;;###autoload
(defun lz/get-sys-theme ()
  "Get system theme variant, :dark if dark mode is preferred, :light otherwise"
  (let* ((command "gsettings get org.gnome.desktop.interface color-scheme")
	 (variant (shell-command-to-string command))
	 (is-dark (string-match-p ".prefer-dark." variant)))
    (if is-dark :dark :light)))

;;;###autoload
(defun lz/set-theme (theme)
  "sets a theme and disable all the other ones, avoiding strange glitches"
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme :no-confirm))

;;;###autoload
(defun lz/set-theme-dark-light (variant dark-theme light-theme)
  "Check variant, if :dark sets dark-theme, set :light otherwise"
  (if (eq :dark variant)
      (lz/set-theme dark-theme)
    (lz/set-theme light-theme)))

;; Temi condizionali in base all'avvio
(cond
 ((member "-nano" command-line-args)
  (progn
    (add-to-list 'load-path (concat user-emacs-directory "nano-emacs/"))
    (require 'nano)))
 (t
  (let ((theme-light 'ef-day)
	(theme-dark 'ef-night))
    (use-package ef-themes
      :ensure
      :config
      (setq ef-themes-to-toggle '(ef-spring ef-winter ef-autumn))
      (if (eq :dark (lz/get-sys-theme))
	  (lz/set-theme theme-dark)
	(lz/set-theme theme-light)))
    (add-hook 'before-make-frame-hook (lambda ()
					(if (eq :dark (lz/get-sys-theme))
					    (lz/set-theme 'ef-night)
					  (lz/set-theme 'ef-day)))))))

;; Packs
(use-package magit :ensure)

(use-package rust-mode :ensure)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (customize-set-variable 'evil-default-state 'emacs)
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'latex-mode 'normal)
  (evil-set-initial-state 'org-mode 'normal))

(use-package go-mode :ensure)

(use-package slime
  :ensure
  :init
  (setq inferior-lisp-program "clisp"))

(use-package markdown-mode :ensure)

;; Set custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Functions
(defun lz/open-configs ()
  "Funzione per aprire il file di configurazione"
  (interactive)
  (find-file 
   (expand-file-name 
    (concat user-emacs-directory "init.el")))) 

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

(defun lz/koans-setup ()
  "Starts the setup for the next koan to do"
  (interactive)
  (let*
      ((dir "/home/luca/gitgets/lisp-koans/")
       (out (shell-command-to-string
	     "cd /home/luca/gitgets/lisp-koans/; clisp -q -norc -ansi contemplate.lisp | grep \"File\""))
       (file-raw (cadr (split-string out)))
       (filename (concat dir (cadr (split-string file-raw "\""))))
       (command (concat "cd " dir " && sh meditate-linux.sh clisp"))
       (current (selected-window))
       (other (get-buffer-window (shell))))
    (select-window current)
    (find-file filename)
    (select-window other)
    (insert command)
    (comint-send-input)
    (select-window current)))

(add-to-list 'load-path (concat user-emacs-directory "modules/"))
(require 'lofi)
