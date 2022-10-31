;; Basic minor mode to play lofi hi hop while coding

(defvar lofi-process nil
  "The current lofi process playing lofi music, if non nil
  contains the process that is playing lofi at the moment")

;;;###autoload
(defun lz/new-lofi-process ()
  "creates a new raw lofi process"
  (make-process
   :name "lofi-process"
   :buffer "*chillin*"
   :command
   '("/bin/sh"
     "-c"
     "link=\`yt-dlp -g \"https://www.youtube.com/watch?v=jfKfPfyJRdk\"\` \
&& ffplay -nodisp -hide_banner -loglevel error $link")))

;;;###autoload
(defun lz/play-lofi ()
  "Starts the lofi reproduction with yt-dlp"
  (setq lofi-process (lz/new-lofi-process))
  (message "Chillin rn"))

;;;###autoload
(defun lz/stop-lofi ()
  "Stops the current lofi process if active, does nothing otherwise"
  (if lofi-process (progn
		     (interrupt-process lofi-process)
		     (message "Stopped chillin, getting serious")
		     (setq lofi-process nil))))

(defcustom lofi-keymap-prefix "C-c ."
  "The prefix for lofi-mode key bindings."
  :type 'string
  :group 'lofi)

;;;###autoload
(defun lofi--key (key)
  (kbd (concat lofi-keymap-prefix  " " key)))

;;;###autoload
(defun lz/lofi-toggle ()
  "Toggle between active and inactive state of lofi"
  (if lofi-mode
      (lz/play-lofi)
    (lz/stop-lofi)))

(define-minor-mode lofi-mode
  "Toggles global lofi mode"
  nil   ; Initial value, nil for disabled
  :global t
  :lighter " chilling"

  (lz/lofi-toggle))

(global-set-key (kbd (lofi--key "c")) #'lofi-mode)
(provide 'lofi)
