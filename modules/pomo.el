(require 'org)

(defvar lz/pomo-work-time 45
  "Work time in (minutes) pomodoro cycles")

(defvar lz/pomo-pause-time 15
  "Pause time in (minutes) pomodoro cycles")

(defvar lz/pomo-pause-audio (concat user-emacs-directory "sounds/coffee_break.wav")
  "Audio to play after each work cycle")

(defvar lz/pomo-work-audio (concat user-emacs-directory "sounds/bell.wav")
  "Audio to play after each pause cycle")

(defun lz/pomo-start ()
  "start pomodoro timer with work-pause cycle defined in
lz/pomo-durations"
  (lz/pomo-start-raw lz/pomo-work-time
		     lz/pomo-pause-time
		     lz/pomo-pause-audio
		     lz/pomo-work-audio))

(defun lz/pomo-start-raw (t1 t2 a1 a2)
  "Starts a timer and registers"
  (setq org-clock-sound a1)
  (org-timer-set-timer t1)
  (setq org-timer-done-hook `(lambda () (lz/pomo-start-raw ,t2 ,t1 ,a2 ,a1))))
  

(defun lz/pomo-stop ()
  "stop pomodoro timer"
  (setq org-timer-done-hook nil)
  (org-timer-stop))

(defun lz/pomo-toggle ()
  "toggle between pause and continue current active pomo timer"
  (interactive)
  (org-timer-pause-or-continue))

(define-minor-mode pomo-mode
  nil
  :global t
  :lighter " Lavora schiavo!"
  (if pomo-mode (lz/pomo-start) (lz/pomo-stop)))

(provide 'pomo)
