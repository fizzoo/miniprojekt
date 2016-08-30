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


;;; Seems to only differ on cases with 0.5 error, i.e. nothing that
;;; will matter.
(defun closest-numerator-help (input test-numerator denominator)
  (let ((guess-low (/ (1- test-numerator) denominator))
        (guess-high (/ test-numerator denominator)))
    (if (<= input guess-high)
        (if (<= guess-low input)
            (if (< (- guess-high input) (- input guess-low)) test-numerator (1- test-numerator))
            (closest-numerator-help input (1- test-numerator) denominator))
        (closest-numerator-help input (1+ test-numerator) denominator))))
(defun closest-numerator-round-easy (input denominator)
  (round (* input denominator)))
(defun closest-numerator-with-help (input denominator)
  (closest-numerator-help input (closest-numerator-round-easy input denominator) denominator))
(defun closest-numerator-round-easy-same? (input denominator)
  (let ((correct (= (closest-numerator-round-easy input denominator) (closest-numerator-with-help input denominator))))
    (if (not correct)
        (format t "error on ~a, ~a~%" input denominator)
        correct)))

(loop for i from 1 to 10000
   do (closest-numerator-round-easy-same? (/ (random 1000) (1+ (random 1000))) (1+ (random 1000))))


(defun closest-numerator (input denominator)
  (round (* input denominator)))

(defun error-of-denom-list (list denominator)
  (loop for i in list summing
       (abs (- i (/ (closest-numerator i denominator) denominator)))))

(defun close-lcm (list &optional (denominator-guess 1) (tolerance 0.001))
  (if (< (error-of-denom-list list denominator-guess) tolerance)
      denominator-guess
      (close-lcm list (1+ denominator-guess) tolerance)))

(defun list-good-lcms (list)
  (loop
     with lasterr = 100000
     for denom from 1 to 10000
     do (cond ((< (error-of-denom-list list denom) lasterr)
               (format t "denominator ~a, error ~a~%" denom (error-of-denom-list list denom))
               (setf lasterr (error-of-denom-list list denom))))))

;;; Could make a "lowest with all digits correct" sort of thing. TODO.
