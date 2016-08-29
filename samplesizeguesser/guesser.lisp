(defun percentage (x)
  (/ x 100))

;;; Unreasonable approach, lcm of a real-world example ended up at
;;; 1519392, on a small poll. Need to check different data simultaneusly.
(defun closest-rational-help (input numerator-guess denominator-guess tolerance)
  (let ((this-rat (/ numerator-guess denominator-guess))
        (one-less-rat (/ (1- numerator-guess) denominator-guess)))
    (cond
      ((< (abs (- input this-rat)) tolerance)
       (rational (/ numerator-guess denominator-guess)))
      ((< input this-rat)
       (if (< one-less-rat input)
           (closest-rational-help input (1- numerator-guess) (1+ denominator-guess) tolerance)
           (closest-rational-help input (1- numerator-guess) denominator-guess tolerance)))
      (T
       (closest-rational-help input (1+ numerator-guess) denominator-guess tolerance)))))
(defun closest-rational (input)
  (closest-rational-help input 1 2 0.001))
(defun closest-rational-of-percentage (input)
  (closest-rational (percentage input)))
