(defun lz/tesi-setup ()
  "Apre un tex della tesi a sinistra (fa scegliere il file) e il compilato a destra"
  (interactive)
  (let ((right (split-window-right))
	(left (selected-window)))
    (select-window right)
    (find-file "~/uni/tesi/git/out/thesis.pdf")
    (select-window left)
    (find-file "~/uni/tesi/git/.")))

(defun lz/tesi-setup1 ()
  "Apre un tex della tesi in alto (fa scegliere il file) e il compilato in alto"
  (interactive)
  (let ((top (split-window))
	(bot (selected-window)))
    (select-window bot)
    (find-file "~/uni/tesi/git/out/thesis.pdf")
    (select-window top)
    (find-file "~/uni/tesi/git/.")))

(provide 'tesi)
