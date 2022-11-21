(defun lz/read-file (fname)
  "Return the contents of a file as a string"
  (with-temp-buffer
    (insert-file-contents fname)
    (buffer-string)))

(provide 'commodity)
