;;; Splash -- splash screen with quotes

;;; Commentary:
;; This package sets as initial screen a quote from a file, centered.
;; Each time a new frame is built the quote changes.

;;; Code:

;; Vars
(defcustom
  lz/quotes-file
  "~/.emacs.d/quotes"
  "File to read the quotes from."
  :type 'string
  :group 'lz-custom)

(defcustom
  lz/splash-buffer-name
  "*lz/start*"
  "Custom initial splash buffer."
  :type 'string
  :group 'lz-custom)

(defvar lz/original-start-buffer initial-buffer-choice
  "The old initialbuffer choice before chainging it.")

;; Functions
(defun lz/get-quote (file)
  "Get a quote from a FILE."
  (let* ((fcontents (with-temp-buffer
		      (insert-file-contents file)
		      (buffer-string)))
	 (splitted (split-string fcontents "\n\n"))
	 (rnd (random (length splitted))))
    (nth rnd splitted)))

(defun lz/display-centered (text)
  "Display the TEXT centered in the curent buffer."
  (let* ((buffer (current-buffer))
         (window (display-buffer buffer)))
    (with-current-buffer buffer
      (with-selected-window window
        (let ((inhibit-read-only t)
              (window-height (window-body-height window t))
              content-height)
          (delete-region (point-min) (point-max))
	  (face-remap-add-relative 'default :height 200)
          (insert (propertize text 'face 'bold))
	  (center-region (point-min) (point-max))
          (set-window-start window (point-min))
          (unless (looking-back "\n$" 5)
            (insert "\n"))
          (setq content-height (cdr (posn-x-y (posn-at-point))))
          (goto-char (point-min))
          (insert (propertize "\n" 'line-height
                              (/ (- window-height content-height) 2))))))))

(defun lz/populate-splash-screen (buffer)
  "Create the splash screen in BUFFER *start* and switch to it."
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
	     (fancy-splash-text (lz/get-quote lz/quotes-file))
	     (splitted (split-string fancy-splash-text "\n"))
	     (start (point)))
	(erase-buffer)
	(make-local-variable 'startup-screen-inhibit-startup-screen)
	(insert (propertize "\n" 'display `(newline :center (top-margin))))
	(lz/display-centered fancy-splash-text))
      (setq buffer-read-only t))
    buffer)

(defun lz/get-splash-screen ()
  "Return the bare start buffer."
  (get-buffer-create lz/splash-buffer-name))

(defun lz/splash-screen ()
  "Create the start buffer and populate it."
  (lz/populate-splash-screen (lz/get-splash-screen)))

(setq initial-buffer-choice #'lz/get-splash-screen)
(add-hook 'server-after-make-frame-hook
	  #'(lambda ()
	      (lz/populate-splash-screen (lz/get-splash-screen))
	      (with-current-buffer (lz/get-splash-screen)
		(setq cursor-type nil))))

(defun lz/splash-change-buffer-contents ()
  "Populate the \\[lz-get-splash-screen] buffer."
  (lz/populate-splash-screen (lz/get-splash-screen))
  (with-current-buffer (lz/get-splash-screen)
    (setq cursor-type nil)))

(define-minor-mode splash-quotes-mode
  "Splash quotes mode.
Minor mode to display a different quote every time a new frame is
created."
  :global t
  :group 'lz-custom
  :lighter nil
  (if splash-quotes-mode
      (progn
	(setq initial-buffer-choice #'lz/get-splash-screen)
	(add-hook 'server-after-make-frame-hook
		  #'lz/splash-change-buffer-contents))
    (setq initial-buffer-choice lz/original-start-buffer)
    (remove-hook 'server-after-make-frame-hook
		 #'lz/splash-change-buffer-contents)))

(provide 'splash)
;;; splash.el ends here
