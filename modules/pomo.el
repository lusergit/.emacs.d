;;; Pomo -- Pomodoro timer with intervals
;;; Commentary:
;;

;;; Code:
(require 'org)

(defgroup
  lz/pomo
  nil
  "The lz/pomo package configs."
  :group 'lz-custom)

(defcustom
  lz/pomo-work-time
  45
  "Work time in (minutes) pomodoro cycles."
  :type 'integer
  :group 'lz/pomo)

(defcustom
  lz/pomo-pause-time
  15
  "Pause time in (minutes) pomodoro cycles."
  :type 'integer
  :group 'lz/pomo)

(defcustom
  lz/pomo-pause-audio
  (concat user-emacs-directory "sounds/coffee_break.wav")
  "Audio to play after each work cycle."
  :type 'string
  :group 'lz/pomo)

(defcustom
  lz/pomo-work-audio
  (concat user-emacs-directory "sounds/bell.wav")
  "Audio to play after each pause cycle."
  :type 'string
  :group 'lz/pomo)

(defun lz/pomo-start ()
  "Start pomodoro timer.
The custom work-pause cycle is defined in lz/pomo-durations"
  (lz/pomo-start-raw lz/pomo-work-time
		     lz/pomo-pause-time
		     lz/pomo-pause-audio
		     lz/pomo-work-audio))

(defun lz/pomo-start-raw (t1 t2 a1 a2)
  "Start the timer.
T1 is the `work' cycle, T2 the `pause' cycle which trigger `A1'
and `A2' audio's respectively."
  (setq org-clock-sound a1)
  (org-timer-set-timer t1)
  (setq org-timer-done-hook
	`(lambda ()
	   (lz/pomo-start-raw ,t2 ,t1 ,a2 ,a1))))
  

(defun lz/pomo-stop ()
  "Stop pomodoro timer."
  (setq org-timer-done-hook nil)
  (org-timer-stop))

(defun lz/pomo-toggle ()
  "Toggle between pause and continue current active pomo timer."
  (interactive)
  (org-timer-pause-or-continue))

(define-minor-mode pomo-mode
  nil
  :global t
  :group 'lz/pomo
  :lighter " Lavora schiavo!"
  (if pomo-mode (lz/pomo-start) (lz/pomo-stop)))

(provide 'pomo)
;;; pomo.el ends here
