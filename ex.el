#!emacs --script
(require 'viper)

;;;; function (re)definitions
(defun ex-print-display-lines (lines)
  (princ lines)
  )
(defun exel-insert ()
  "Replacement for the i command"
  (let ((text nil)
	(continue t)
	(current-line ""))
    (while continue
      (setq current-line (read-from-minibuffer ""))
      (cond ((equal current-line ".")
	     (setq continue nil)
	     (setq text (concat text "\n")))
	    (t
	     (setq text (concat text current-line "\n")))))
    (insert text)
    (backward-delete-char 1) ;remove trailing newline
    ))

;;;; print error/welcome messages
(when (< (length argv) 1)
  (message "ex.el -- ex in Emacs Lisp")
  (message "Should be called like this: ./ex.el [-c command]... file...")
  (message "Args given: %s" argv))


;;;; handle command line options
(defvar initial-commands nil)
(defvar files nil)
(defvar batch nil)
(defvar prompt ":")
(defvar show-prompt nil)
(defvar args argv)
(while (not (eq args nil))
  (let ((arg (car args)))
    (cond
     ((equal arg "-c")
      (setq initial-commands (cons (cadr args) initial-commands))
      (setq args (cddr args)))
     ((equal arg "-s")
      (message "[Warning: Batch mode is not fully implemented yet!]")
      (setq batch t)
      (setq args (cdr args)))
     ((equal arg "-p")
      (setq show-prompt t)
      (setq prompt (cadr args))
      (setq args (cddr args)))
     ((equal arg "-v")
      (message "Error: Visual mode is not supported. Use viper-mode!")
      (kill-emacs)
      (setq args (cdr args)))
     (t (setq files args)
	(setq args nil)))))
(setq argv nil)

;;;; load file and apply initial commands
(when (not (eq files nil))
  (mapcar #'find-file files) ;TODO: Figure out what vipers "n" command actually does
  (find-file (car files)))
(when initial-commands
  (setq initial-commands (nreverse initial-commands))
  (mapcar (lambda(x) (viper-ex nil x)) initial-commands))

;;;; main loop
(while (not batch)
  (let ((command (read-from-minibuffer (if show-prompt prompt ""))))
    (cond
     ((equal command "a")
      (end-of-line)
      (exel-insert))
     ((equal command "c")
      (beginning-of-line)
      (kill-line)
      (exel-insert))
     ((equal command "i")
      (exel-insert))
     ((equal command "P")
      (setq show-prompt (not show-prompt)))
     (t
        (with-demoted-errors
	    "Error: %s"
	  (viper-ex nil command))))))
  

