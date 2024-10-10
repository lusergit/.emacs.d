;;; elixir-project-backend.el --- Elixir projects backend for project.el -*- lexical-binding: t -*-

;; Author: Luca
;; Maintainer: Luca
;; Version: v0.01
;; Package-Requires: (nil)


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

;; commentary

;;; Code:

;; - Choose the format of the value that represents a project for your
;; backend (we call it project instance).  Don't use any of the
;; formats from other backends.  The format can be arbitrary, as long
;; as the datatype is something `cl-defmethod' (custom-autoload symbol
;; load noset)n dispatch on.  The value should be stable (when
;; compared with `equal') across invocations, meaning calls to that
;; function from buffers belonging to the same project should return
;; equal values.

(require 'project)

(cl-defmethod project-root ((project (head elixir)))
  "Return the PROJECT of an 'elixir PROJECT type."
  (car (cdr project)))

(defun project-elixir-try (dir)
  "Return the 'elixir project structure.

If DIR is in an elixir project return the project structure."
  (let ((dominant (locate-dominating-file dir "mix.exs")))
    (let ((igns nil) (extr nil) (dotfile (concat dominating ".project")))
      (when (and (file-exists-p dotfile) (file-readable-p dotfile))
	(let ((linep t)
	      (line ""))
	  (with-temp-buffer
	    (insert-file-contents-literally (concat dominating ".project"))
	    (goto-char (point-min))
	    (while linep
	      (setq line (buffer-substring-no-properties
			  (line-beginning-position)
			  (line-end-position)))
	      (let ((split (split-string line ":")))
		(when (= 2 (length split))
		  (if (string= (car split) "#")
		      (setq igns (cons (car (cdr split)) igns))
		    (setq extr (cons (car (cdr split)) extr)))))
	      (setq linep (= 0 (forward-line 1)))))))))

  (provide 'elixir-project-backend)

;;; elixir-project-backend.el ends here
