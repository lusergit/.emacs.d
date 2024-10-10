;;; package -- Languages

;;; Commentary:
;; provides support for every language

;;; Code:
(use-package clojure-mode)

(use-package gleam-ts-mode
  :mode (rx ".gleam" eos))

(use-package elixir-ts-mode
  :ensure t
  :hook
  (elixir-ts-mode . (lambda () (add-hook 'before-save-hook #'elixir-format t t)))
  :custom
  (lsp-elixir-server-command '("/home/luserworks/gitgets/lexical/_build/dev/package/lexical/bin/start_lexical.sh")))

(use-package mix
  :config
  (add-hook 'elixir-mode-hook 'mix-minor-mode))

(use-package cider)

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (add-to-list 'exec-path "~/.cargo/bin"))

(use-package typescript-mode)

(use-package markdown-mode
  :custom (markdown-command "/usr/bin/pandoc"))

(use-package erlang)

(use-package haskell-mode)

(use-package nix-mode)

(use-package move-mode)

(use-package web)

(use-package go-mode
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package htmlize)

;; AGDA
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(use-package zig-mode
  :config
  (if (>= emacs-major-version 28)
      (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
    (progn
      (defun colorize-compilation-buffer ()
	(let ((inhibit-read-only t))
          (ansi-color-apply-on-region compilation-filter-start (point))))
      (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))))

(use-package kotlin-mode)

(provide 'languages)
;;; languages.el ends here
