(use-modules (mwlib util))

(define (parse-turn s)
  (if (char=? (string-ref s 0) #\L)
    (- (string->number (substring s 1)))
    (string->number (substring s 1))))

(define (sum-a dial old-a-sum)
  (if (= dial 0) (+ old-a-sum 1) old-a-sum))

(define (sum-b dial turn old-b-sum)
  (let* ((full-turns (quotient (abs turn) 100))
         (turn (remainder turn 100)))
    (if (or (and (< turn 0) (> dial 0) (>= (abs turn) dial))
	    (and (> turn 0) (>= (+ dial turn) 100)))
        (+ old-b-sum 1 full-turns)
        (+ old-b-sum full-turns))))

(define (turn-dial dial turn)
  (euclidean-remainder (+ dial turn) 100))

(define (add-turn turn-str curr-vals)
  (let* ((turn (parse-turn turn-str))
         (dial-pos (car curr-vals))
	 (b-sum (sum-b dial-pos turn (caddr curr-vals)))
         (next-dial (turn-dial dial-pos turn))
	 (a-sum (sum-a next-dial (cadr curr-vals))))
    (list next-dial a-sum b-sum)))

(define (day1)
  (let ((result (fold-file add-turn '(50 0 0) "data/day1.txt")))
    (format #t "day 1a = ~a~%" (cadr result))
    (format #t "day 1b = ~a~%" (caddr result))))


