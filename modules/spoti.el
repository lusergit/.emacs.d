;; ;;; spoti.el --- Smudge configurations -*- lexical-binding: t -*-

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

;; Smudge package configuration and bindings, getting secrets from the shell!

;;; Code:

(use-package smudge
  :bind-keymap ("C-c s" . smudge-command-map)
  :config
  (setq smudge-oauth2-client-secret (getenv "SPOTIFY_SECRET")
        smudge-oauth2-client-id (getenv "SPOTIFY_CLIENT_ID")))

(provide 'spoti)

;;; spoti.el ends here
