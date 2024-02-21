;;; Snippets -- custom snippets settings

;;; Commentary:
;; This file provides custom definitions and settings for the
;; yasnippets package

;;; Code:
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :config
  (yas-reload-all))

(provide 'snippets-custom)
;;; snippets-custom.el ends here
