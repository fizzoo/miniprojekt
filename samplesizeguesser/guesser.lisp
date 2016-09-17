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

(defun list-of-good-lcms (list &optional (upto 10000))
  "Creates a list of upto elements, containing the MSE of using the
denominator corresponding to that index."
  (loop
     for denom from 1 to upto
     collecting (mse-of-denom-list list denom)))

(defun find-valleys (list &key (cur-index 1))
  "Finds valleys in values in list, returning pairs of an index and
value of those valleys"
  (cond ((or (not list) (not (cdr list)) (not (cddr list)))
         nil)
        ((and (> (car list) (cadr list))
              (> (caddr list) (cadr list)))
         (cons (cons cur-index (cadr list))
               (find-valleys (cdr list)
                             :cur-index (1+ cur-index))))
        (t
         (find-valleys (cdr list)
                       :cur-index (1+ cur-index)))))

(defun print-good-lcms (list)
  (loop
     for v in (find-valleys (list-of-good-lcms list))
     do (format t "denominator ~a, error ~a~%" (car v) (cdr v))))


;;; Could make a "lowest with all digits correct" sort of thing. TODO.

;; collecting (cond ((< (mse-of-denom-list list denom) lasterr)
;;           (format t "denominator ~a, error ~a~%" denom (mse-of-denom-list list denom))
;;           (setf lasterr (mse-of-denom-list list denom))))))
