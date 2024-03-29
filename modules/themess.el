;;; Themes -- package containing visual settings

;;; Commentary:
;; Modus themes builtin

;;; Code:
(use-package auto-dark
  :ensure t
  :config
  (customize-set-variable 'auto-dark-dark-theme 'modus-vivendi)
  (customize-set-variable 'auto-dark-light-theme 'modus-operandi)
  (auto-dark-mode))

;; Color mode for terms
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

;; protesilaos spacious padding
(use-package spacious-padding
  :ensure t
  :config
  (add-hook 'server-after-make-frame-hook #'spacious-padding-mode))

(provide 'themess)
;;; themess.el ends here
