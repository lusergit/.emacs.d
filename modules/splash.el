(defun lz/get-quote (file)
  "Get a quote from a file"
  (let* ((fcontents (with-temp-buffer
		      (insert-file-contents file)
		      (buffer-string)))
	 (splitted (split-string fcontents "\n\n"))
	 (rnd (random (length splitted))))
    (nth rnd splitted)))

(defvar lz/quotes-file "~/.emacs.d/quotes"
  "File to read the quotes from")

(defun lz/display-centered (text)
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
          (unless (looking-back "\n$")
            (insert "\n"))
          (setq content-height (cdr (posn-x-y (posn-at-point))))
          (goto-char (point-min))
          (insert (propertize "\n" 'line-height
                              (/ (- window-height content-height) 2))))))))

(defconst lz/splash-buffer-name "*start*")

(defun lz/populate-splash-screen (buffer)
  "Create the splash screen in buffer *start* and switch to it"
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
  "returns the bare start buffer"
  (get-buffer-create lz/splash-buffer-name))

(defun lz/splash-screen ()
  "create the start buffer and populate it"
  (lz/populate-splash-screen (lz/get-splash-screen)))

(setq initial-buffer-choice #'lz/get-splash-screen)
(add-hook 'server-after-make-frame-hook
	  '(lambda ()
	     (lz/populate-splash-screen (lz/get-splash-screen))
	     (with-current-buffer (lz/get-splash-screen)
	       (setq cursor-type nil))))

(provide 'splash)
