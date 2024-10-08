;;; vterm-custom.el --- VTerm init and customization -*- lexical-binding: t -*-

;; Author: Luca
;; Maintainer: Luca
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


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

;; Vterm Settings and initialization.

;;; Code:

(use-package vterm
  :config
  (dolist (mode '(term-mode-hook
                  vterm-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode
	      (lambda()
		(display-line-numbers-mode 0)
		(hl-line-mode 0)))))

(provide 'vterm-custom)

;;; vterm-custom.el ends here