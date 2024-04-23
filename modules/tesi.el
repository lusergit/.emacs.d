;;; Tesi -- Utility per la scrittura della tesi

;;; Commentary:
;; Il pacchetto non serve a nulla, ha solo delle funzioni che aiutano
;; a impostare il lavoro su più finestre per scrivere la tesi

;;; Code:
(defcustom
  thesis-pdf-file
  "~/tesi/out/thesis.pdf"
  "Pdf file output della tesi."
  :type 'string
  :group 'lz-group)

(defcustom
  thesis-tex-file
  "~/tesi/thesis.tex"
  "Main TeX file della tesi."
  :type 'string
  :group 'lz-custom)

(defun lz/tesi-setup ()
  "Apre un tex della tesi.
In questo case a sinistra (fa scegliere il file) e il compilato a
destra."
  (interactive)
  (let ((right (split-window-right))
	(left (selected-window)))
    (select-window right)
    (find-file thesis-pdf-file)
    (select-window left)
    (find-file thesis-tex-file)))

(defun lz/tesi-setup1 ()
  "Apre il compilato della tesi.
L'idea è di aprirlo in grande così da poterlo editare cliccandoci
sopra e distribuendo lo schermo secondo le regole di composizione
di default."
  (interactive)
  (let ((window (selected-window)))
    (select-window window)
    (find-file thesis-pdf-file)
    (pdf-view-fit-width-to-window)))

(provide 'tesi)
;;; tesi.el ends here
