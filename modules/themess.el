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
  (let ((theme-light 'ef-spring)
	(theme-dark 'ef-autumn))
    (use-package ef-themes
      :ensure
      :config
      (setq ef-themes-to-toggle '(ef-spring ef-winter ef-autumn))
      (if (eq :dark (lz/get-sys-theme))
	  (lz/set-theme theme-dark)
	(lz/set-theme theme-light)))
    (add-hook 'before-make-frame-hook (lambda ()
					(let ((variant (lz/get-sys-theme)))
					  (if (eq :dark variant)
					      (lz/set-theme 'ef-autumn)
					    (lz/set-theme 'ef-spring))))))))

(provide 'themess)
