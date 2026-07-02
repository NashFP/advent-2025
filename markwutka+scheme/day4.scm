(use-modules (mwlib util) (srfi srfi-1))

(define offsets '((-1 . -1) (0 . -1) (1 . -1)
		  (-1 . 0) (1 . 0)
		  (-1 . 1) (0 . 1) (1 . 1)))

(define (grid-char grid x y offset)
  (let ((x (+ x (car offset)))
	(y (+ y (cdr offset))))
    (if (or (< y 0) (>= y (vector-length grid))
	    (< x 0) (>= x (string-length (vector-ref grid 0))))
	#\.
	(string-ref (vector-ref grid y) x))))

(define (adv-value grid x y off)
  (if (char=? (grid-char grid x y off) #\@) 1 0))

(define (num-adj grid x y)
  (apply + (map ($ (adv-value grid x y &)) offsets)))

(define (count-rolls remove curr-sum grid x y)
  (if (char=? (grid-char grid x y '(0 . 0)) #\@)      
      (if (< (num-adj grid x y) 4)
	  (begin
	    (when remove (string-set! (vector-ref grid y) x #\.))
	    (1+ curr-sum))
	  curr-sum)
      curr-sum))

(define (count-a curr-sum grid x y)
  (count-rolls #f curr-sum grid x y))

(define (count-b curr-sum grid x y)
  (count-rolls #t curr-sum grid x y))

(define (keep-removing grid sum)
  (let ((curr-count (string-vector-fold count-b 0 grid)))
    (if (= 0 curr-count) sum
	(keep-removing grid (+ sum curr-count)))))

(define (day4)
  (let* ((grid (list->vector (read-file "data/day4.txt")))
	 (height (vector-length grid))
	 (width (string-length (vector-ref grid 0))))
    (format #t "day4a = ~a~%" (string-vector-fold count-a 0 grid))
    (format #t "day4b = ~a~%" (keep-removing grid 0))))
