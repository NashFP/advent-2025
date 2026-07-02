(use-modules (mwlib util))

(define (max-in-range start end str best best-pos)
  (if (= start end) best-pos
      (if (char>? (string-ref str start) best)
	  (max-in-range (1+ start) end str (string-ref str start) start)
	  (max-in-range (1+ start) end str best best-pos))))

(define (best-num num-digits start digits)
  (if (= num-digits 0) ""
      (let ((best-pos (max-in-range start
				     (1+ (- (string-length digits) num-digits))
				     digits (string-ref digits start) start)))
	(string-append (substring digits best-pos (1+ best-pos))
		       (best-num (1- num-digits) (1+ best-pos) digits)))))

(define (day3)
  (let ((numbers (read-file "data/day3.txt")))
    (format #t "day3a = ~a~%"
	    (apply + (map string->number (map ($ (best-num 2 0 &)) numbers))))
    (format #t "day3b = ~a~%"
	    (apply + (map string->number (map ($ (best-num 12 0 &)) numbers))))
    ))
