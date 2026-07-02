
(use-modules (mwlib util) (srfi srfi-1))

(define (repeats? part rest mult)
  (if (= rest 0) #t
      (if (= part (remainder rest mult))
	  (repeats? part (quotient rest mult) mult)
	  #f)))

(define (repeat-count n)
  (let ((num-digits (1+ (inexact->exact (floor (log10 n))))))
    (let loop ((repeat-count 2))
      (if (>= repeat-count 10) 0
	  (if (> (modulo num-digits repeat-count) 0)
	      (loop (1+ repeat-count))
	      (let* ((repeat-length (quotient num-digits repeat-count))
		     (mult (expt 10 repeat-length))
		     (first-part (remainder n mult))
		     (rest (quotient n mult)))
		(if (= mult 1) 0
		    (if (repeats? first-part rest mult)
			repeat-count
			(loop (1+ repeat-count))))))))))

(define (num-repeats range)
  (let ((from (string->number (car range)))
	(to (string->number (cadr range))))
    (let loop ((n from)
	       (repeats2 0)
	       (repeatsn 0))
      (if (<= n to)
	  (let ((repeats (repeat-count n)))
	    (cond
	     ((= repeats 0) (loop (1+ n) repeats2 repeatsn))
	     ((= repeats 2) (loop (1+ n) (+ repeats2 n) repeatsn))
	     (#t (loop (1+ n) repeats2 (+ repeatsn n)))))
	  (cons repeats2 repeatsn)))))

(define (sum-repeats vals sums)
  (cons (+ (car vals) (car sums))
	(+ (cdr vals) (cdr sums))))

(define (day2)
  (let* ((pairs (map ($ (string-split & #\-))
		     (string-split (car (read-file "data/day2.txt")) #\,)))
	 (sums
	  (fold sum-repeats (cons 0 0)
		(map num-repeats pairs))))
    (format #t "day 2a = ~a~%" (car sums))
    (format #t "day 2b = ~a~%" (+ (car sums) (cdr sums)))))
