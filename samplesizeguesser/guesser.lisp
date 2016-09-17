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

(defun list-of-good-lcms (list &optional (upto 256))
  "Creates a list of upto elements, containing the MSE of using the
denominator corresponding to that index."
  (loop
     for denom from 1 to upto
     collecting (mse-of-denom-list list denom)))

(defun find-valleys (list &key (cur-index 3))
  "Finds valleys in values in list, returning pairs of an index and
value of those valleys"
  (cond ((not (nth 4 list))
         nil)
        ((and (> (nth 0 list) (nth 2 list)) ;look two steps prev/ahead
              (> (nth 1 list) (nth 2 list))
              (> (nth 3 list) (nth 2 list))
              (> (nth 4 list) (nth 2 list)))
         (cons (cons cur-index (nth 2 list))
               (find-valleys (cdr list)
                             :cur-index (1+ cur-index))))
        (t
         (find-valleys (cdr list)
                       :cur-index (1+ cur-index)))))

(defun find-first-zero (list)
  "Finds the first zero in a list, returning its index or nil"
  (loop
     for x in list
     for i from 0
     do (if (= x 0.0) (return i))))

(defun print-good-lcms (list &optional (upto 256))
  "Prints a number of good valleys of the lcms of the list"
  (let* ((good-lcms (list-of-good-lcms list upto))
         (valley-list (find-valleys good-lcms))
         (first-zero (find-first-zero good-lcms)))
    (if first-zero
        (format t "First zero: ~a~%" (1+ first-zero))
        (format t "Found no zeroes~%"))
    (loop
       for v in valley-list
       do (format t "Denominator ~a, error ~a~%" (car v) (cdr v)))
    (format t "~%")))


;; (defun percentage (x)
;;   (/ x 100))
;; (defparameter *jap* (map 'list #'percentage '(5.0 5.0 4.0 5.0 20.0 1.0 0.0 14.0 0.0 0.0 20.0 5.0)))
;; (defparameter *usa* (map 'list #'percentage '(78.9 27.4 16.4 15.6 5.4 7.5 3.8 2.8 1.9 1.5 1.6 1.0)))
;; (defparameter *eur* (map 'list #'percentage '(40.6 17.0 15.1 8.2 5.1 3.6 2.8 2.2 1.9 1.5 1.0 0.9)))
;; (print-good-lcms *jap* 100000)
;; (print-good-lcms *usa* 100000)
;; (print-good-lcms *eur* 100000)

;;; Could make a "lowest with all digits correct" sort of thing. TODO.
