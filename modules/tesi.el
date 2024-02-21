;;; Tesi -- Utility per la scrittura della tesi

;;; Commentary:
;; Il pacchetto non serve a nulla, ha solo delle funzioni che aiutano
;; a impostare il lavoro su più finestre per scrivere la tesi

;;; Code:
(defun lz/tesi-setup ()
  "Apre un tex della tesi a sinistra (fa scegliere il file) e il compilato a destra."
  (interactive)
  (let ((right (split-window-right))
	(left (selected-window)))
    (select-window right)
    (find-file "~/uni/tesi/git/out/thesis.pdf")
    (select-window left)
    (find-file "~/uni/tesi/git/.")))

(defun lz/tesi-setup1 ()
  "Apre un tex della tesi in alto (fa scegliere il file) e il compilato in alto."
  (interactive)
  (let ((window (selected-window)))
    (select-window window)
    (find-file "~/uni/tesi/git/out/thesis.pdf")
    (pdf-view-fit-width-to-window)))

(provide 'tesi)
;;; tesi.el ends here
