(use-modules (mwlib util) (srfi srfi-1))

(define (make-operator-list ops)
  (let loop ((op-and-pos (filter (lambda (p) (not (char=? (cdr p) #\ )))
				 (map-index (lambda (n c) (cons n c))
				       (string->list ops))))
	     (acc '()))
    (if (null? op-and-pos) (reverse acc)
	(if (null? (cdr op-and-pos))
	    (loop (cdr op-and-pos)
		  (cons (list (caar op-and-pos) (cdar op-and-pos)
			      (- (string-length ops) (caar op-and-pos))) acc))
	    (loop (cdr op-and-pos)
		  (cons (list (caar op-and-pos) (cdar op-and-pos)
			      (1- (- (caadr op-and-pos) (caar op-and-pos))))
			acc))))))

(define (do-op op numbers)
  (if (char=? op #\+) (apply + numbers) (apply * numbers)))

(define (do-op-a numbers op)
  (let ((start (car op))
	(which-op (cadr op))
	(num-len (caddr op)))
    (do-op which-op
	   (map
	    (lambda (s) (string->number
			 (string-trim-both
			  (substring s start (+ start num-len)))))
		numbers))))

(define (do-op-b numbers op)
  (let ((start (car op))
	(which-op (cadr op))
	(num-len (caddr op)))
    (do-op
     which-op
     (map (lambda (n)
	    (string->number
	     (string-trim-both (list->string
				(map (lambda (s) (string-ref s (+ start n)))
				     numbers)))))
	  (range 0 num-len)))))
(define (day6)
  (let* ((lines (read-file "data/day6.txt"))
	 (operators (make-operator-list (list-ref lines 4)))
	 (numbers (take lines 4)))
    (format #t "day6a = ~a~%" (apply + (map (lambda (op) (do-op-a numbers op))
				    operators)))
    (format #t "day6b = ~a~%" (apply + (map (lambda (op) (do-op-b numbers op))
				    operators)))))
