;;; LaTeX -- LaTeX configurations

;;; Commentary:
;; Custom configurations for latex and all its derivative options
;; (e.g., working with latex preview in org mode)

;;; Code:

(require 'org-custom)

(use-package latex
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (if (boundp 'LaTeX-mode-map)
      (define-key LaTeX-mode-map (kbd "M-s M-s") #'lz/save-compile)
    (define-key latex-mode-map (kbd "M-s M-s") #'lz/save-compile)))

(provide 'latex-custom)
;;; latex-custom.el ends here
