;;; Modeline -- Modeline customizations
;;; Commentary:
;; The following code customizes the modeline

;;; Code:
(use-package minions :ensure)

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

(provide 'modeline)
;;; modeline.el ends here
