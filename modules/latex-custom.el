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

(defun lz/switch-buffers-2-win ()
  "When in split window view switch the buffers"
  (interactive)
  (let* ((other-window (other-window-for-scrolling))
	 (this-window (selected-window))
	 (other-buffer (window-buffer other-window))
	 (this-buffer (window-buffer this-window)))
    (set-window-buffer other-window this-buffer)
    (set-window-buffer this-window other-buffer)))
(global-set-key (kbd "C-x w i") 'lz/switch-buffers-2-win)

(defun lz/latex-save-and-compile ()
  "A macro function to save and compile with one step"
  (interactive)
  (save-buffer)
  (lz/silent-compile))

(use-package latex
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (if (boundp 'LaTeX-mode-map)
      (define-key LaTeX-mode-map (kbd "M-s M-s") 'lz/latex-save-and-compile)
    (define-key latex-mode-map (kbd "M-s M-s") 'lz/latex-save-and-compile)))


(provide 'latex-custom)
