;;; -*- lexical-binding: t; -*-

;;; run with 'emacs -batch -l repl.el'

(message " ")
(message "GERMANY -- the GNU Emacs REPL with MANY bugs, V 0.1")
(message "Copyright 2018 programmbauer")
(message " ")
(while t
  (with-demoted-errors
      "Error: %s"
    (message
     (format
      "%s"
      (eval
       (car
	(read-from-string
	 (read-from-minibuffer "> "))))))))
