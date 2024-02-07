;; LANGUAGES
(use-package clojure-mode :ensure)
(use-package cider
  :ensure t)
(use-package rust-mode
  :ensure
  :config
  (setq rust-format-on-save t)
  (add-to-list 'exec-path "~/.cargo/bin"))
(use-package typescript-mode :ensure)
(use-package markdown-mode :ensure)
(use-package erlang :ensure)
(use-package haskell-mode :ensure)
(use-package nix-mode :ensure)
(use-package move-mode :ensure)
(use-package web :ensure)

(use-package go-mode
  :ensure
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package slime
  :ensure
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package yaml-mode
  :ensure
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; HTMLIZE
(use-package htmlize :ensure)

(provide 'languages)
