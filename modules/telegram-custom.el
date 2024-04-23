;;; Telegram -- Telegram configurations
;;; COmmentary:
;; Telegram has an official client which I personally dislike.

;;; Code:
(defun online-p ()
  "Check if the host is online.
Naive approach, try to connect to google and say t/nil"
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"  "www.google.com")))

(use-package telega
  :ensure t
  :init
  (setq telega-use-images t
	telega-emoji-use-images t
	telega-online-status-function #'online-p))

;;; telegram-custom.el ends here
