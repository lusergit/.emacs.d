;;; LaTeX -- LaTeX configurations

;;; Commentary:
;; Custom configurations for latex and all its derivative options
;; (e.g., working with latex preview in org mode)

;;; Code:

(defun lz/latex-silent-compile ()
  "Run compile in a silent buffer, not displaying it."
  (interactive)
  (let* ((bname "*compilation*")
	 (other-win (other-window-for-scrolling))
	 (other-buf (window-buffer other-win)))
    (if (get-buffer bname)
  	(progn
  	  (delete-windows-on (get-buffer bname))
  	  (kill-buffer bname))
      (get-buffer-create bname t))
    (call-interactively 'projectile-compile-project)
    (set-window-buffer other-win other-buf)
    (delete-windows-on (get-buffer bname))
    (message "Compiling...")))

(defun lz/latex-save-and-compile ()
  "A macro function to save and compile with one step."
  (interactive)
  (save-buffer)
  (lz/latex-silent-compile))

(use-package latex
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (if (boundp 'LaTeX-mode-map)
      (define-key LaTeX-mode-map (kbd "M-s M-s") 'lz/latex-save-and-compile)
    (define-key latex-mode-map (kbd "M-s M-s") 'lz/latex-save-and-compile)))

(provide 'latex-custom)
;;; latex-custom.el ends here
