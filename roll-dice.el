(defun --roll-XdY+Z (x y op z)
  (let ((res (if (string= op "-") (- z) z))
	(str (concat "Rolling " str ": "))
	(i 0))
    (while (< i x)
      (let ((die (1+ (random y))))
	(setq str (concat str (format "[%i] " die)))
	(setq res (+ res die))
	(setq i (1+ i))))
    (setq str (concat str (format "%s %i -> %i" op z res)))
    (message str)
    res))

(defun roll-dice (str)
  (interactive "sRoll dice (XdY+Z or XdY-Z): ")
  (if (string-match (rx (group (one-or-more digit))  ;group 1: number of dice
			"d"
			(group (one-or-more digit))  ;group 2: type of dice
			(group (or "+" "-"))         ;group 3: add or subtract
			(group (one-or-more digit))) ;group 4: constant
		    str)
      (--roll-XdY+Z (string-to-number (match-string 1 str))
		  (string-to-number (match-string 2 str))
		  (match-string 3 str)
		  (string-to-number (match-string 4 str)))
    (error "roll-dice: Invalid input. Try something like 2d8+3 or 1d20-4")))


