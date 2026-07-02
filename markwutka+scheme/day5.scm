(use-modules (mwlib util) (srfi srfi-1))

(define (parse-range r)
  (let ((parts (string-split r #\-)))
    (cons (string->number (car parts))
	  (string->number (cadr parts)))))

(define (in-range n ranges)
  (if (null? ranges) #f
      (if (and (>= n (car (car ranges)))
	       (<= n (cdr (car ranges)))) #t
	       (in-range n (cdr ranges)))))

(define (range<? r1 r2)
  (if (< (car r1) (car r2)) #t
      (if (= (car r1) (car r2))
	  (< (cdr r1) (cdr r2))
	  #f)))

(define (merge-ranges ranges)
  (if (null? ranges) ranges
      (if (null? (cdr ranges)) ranges
	  (let* ((curr (car ranges))
		 (next (cadr ranges))
		 (r1-low (car curr))
		 (r1-high (cdr curr))
		 (r2-low (car next))
		 (r2-high (cdr next)))
	    (cond
	     ((> r2-low r1-high) (cons curr (merge-ranges (cdr ranges))))
	     ((and (>= r2-low r1-low)
		   (<= r2-high r1-high))
	      (merge-ranges (cons curr
				  (cddr ranges))))
	     (#t
	      (merge-ranges (cons (cons r1-low r2-high)
				  (cddr ranges)))))))))

(define (range-size range)
  (1+ (- (cdr range) (car range))))

(define (day5)
  (let* ((lines (read-file "data/day5.txt"))
	 (ranges (map parse-range
		      (take-while ($ (not (string-null? &))) lines)))
	 (ranges-sorted (sort ranges range<?))
	 (ids (map string->number (cdr (drop-while
					($ (not (string-null? &))) lines)))))
    (format #t "day5a = ~a~%" (length
			       (filter (lambda (n) (in-range n ranges))
				       ids)))
    (format #t "day5b = ~a~%"
	    (apply + (map range-size (merge-ranges ranges-sorted))))))
	 
