(defun closest-numerator (input denominator)
  "Closest numerator of a fraction, in terms of the denominator."
  (round (* input denominator)))

(defun square (x) (* x x))
(defun mse-of-denom-list (list denominator)
  "Mean square error of values in list vs its integer
numerator/denominator form."
  (/ (loop for i in list summing
          (square (- i (/ (closest-numerator i denominator)
                          denominator))))
     (length list)))

(defun close-lcm (list &optional (denominator-guess 1) (tolerance 0.00001))
  "Guesses a lcm that has lower error than tolerance."
  (if (< (mse-of-denom-list list denominator-guess) tolerance)
      denominator-guess
      (close-lcm list (1+ denominator-guess) tolerance)))

(defun list-good-lcms (list)
  (loop
     with lasterr = 100000
     for denom from 1 to 10000
     do (cond ((< (mse-of-denom-list list denom) lasterr)
               (format t "denominator ~a, error ~a~%" denom (mse-of-denom-list list denom))
               (setf lasterr (mse-of-denom-list list denom))))))

;;; Could make a "lowest with all digits correct" sort of thing. TODO.
