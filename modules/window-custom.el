;; Themes and visual components
(global-hl-line-mode 1)
;; (display-battery-mode 1)
(blink-cursor-mode 0)

(if (not (version< emacs-version "26.0"))
    (progn
      (global-display-line-numbers-mode t)
      (setq display-line-numbers-type 'relative))
  (global-linum-mode t))

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)
;; tabs settings
(setq tab-bar-select-tab-modifiers '(control shift)
      tab-bar-tab-hints t)
(tab-bar-history-mode 1)

(setq lz/frame-settings
      '((width . 100)
	(height . 30)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
	;; (font . "Iosevka Comfy Fixed-16")
	(font . "Space Mono-14")))

(setf initial-frame-alist lz/frame-settings
      default-frame-alist lz/frame-settings)

(show-paren-mode 1)
(setq show-paren-style 'mixed)
;; (display-time-mode 1)

(defun lz/next-buffer-other-window (&optional arg interactive)
  "In other window switch to ARGth next buffer.
Call `switch-to-next-buffer' unless the selected window is the
minibuffer window or is dedicated to its buffer."
  (interactive "p\np")
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (select-window other)
    (next-buffer arg interactive)
    (select-window current)))

(defun lz/previous-buffer-other-window (&optional arg interactive)
  "In other window switch to ARGth previous buffer.
Call `switch-to-prev-buffer' unless the selected window is the
minibuffer window or is dedicated to its buffer."
  (interactive "p\np")
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (select-window other)
    (previous-buffer arg interactive)
    (select-window current)))

(defun lz/ff-other-window ()
  "Find file in other window."
  (interactive)
  (cond
   ((one-window-p t)
    (call-interactively #'find-file-other-window))
   (t
    (let ((other (other-window-for-scrolling))
          (current (selected-window)))
      (select-window other)
      (call-interactively #'find-file)
      (select-window current)))))

(defun lz/kill-buffer-other-window ()
  "Kills buffer in other window."
  (interactive)
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (select-window other)
    (kill-buffer)
    (select-window current)))

(defun lz/shell-vertically-split (&optional shell-type)
  "Opens a shell in a vertically split window, if given 'eshell
as argument starts a new eshell, 'term starts a new term and
'ansi-term a new ansi-term"
  (interactive)
  (let ((other (split-window-below))
	(current (selected-window))
	(term-here (lambda ()
		     (let* ((current (selected-window))
			    (shell-buffer (shell)))
		       (delete-window (get-buffer-window shell-buffer))
		       (select-window current)
		       (switch-to-buffer shell-buffer)))))
    (select-window other)
    (if (boundp shell-type)
	(cond ((= shell-type 'eshell) (eshell))
	      ((= shell-type 'term) (term))
	      ((= shell-type 'ansi-term) (ansi-term))
	      (t (funcall term-here)))
      (funcall term-here))
    (select-window current)))

(provide 'window-custom)
