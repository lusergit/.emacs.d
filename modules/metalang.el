;;; metalang.el --- Support for lsp and error signaling -*- lexical-binding: t -*-

;; Author: Luca
;; Maintainer: Luca
;; Version: 0.1
;; Package-Requires: (helm )

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides support for the lsp and error signaling inside of code.

;;; Code:

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (rust-mode . lsp)
	 (zig-mode . lsp)
	 (tex-mode . lsp)
	 (latex-mode . lsp))
  :commands lsp)

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-backends (delete 'company-semantic company-backends))
  (use-package company-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-c-headers)))

(require 'helm)
(use-package helm-gtags
  :ensure t
  :init
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t)
  :config
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))

(require 'cc-mode)
(use-package semantic
  :ensure t
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1))

;; auto matching parenthesis
(electric-pair-mode t)

(setq treesit-extra-load-path "~/gitgets/tree-sitter-module/dist/")

(provide 'metalang)

;;; metalang.el ends here
