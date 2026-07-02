(define-module (mwlib binheap)
  #:use-module (srfi srfi-43)
  #:export (make-binheap binheap-insert! binheap-pop! binheap-empty?
			 binheap-length binheap->list! list->binheap)
  )

(define* (make-binheap #:key (capacity 1000) (compare <))
  (let ((v (make-vtable "pwpwpw")))
    (make-struct/no-tail v (make-vector capacity) 0 compare)))

(define* (list->binheap l #:key (capacity 1000) (compare <))
  (let ((new-binheap (make-binheap #:capacity capacity #:compare compare)))
    (for-each (lambda (item) (binheap-insert! new-binheap item))
	      l)
    new-binheap))

(define (binheap-length binheap)
  (struct-ref binheap 1))

(define (binheap->list! binheap)
  (let loop ((acc '()))
    (if (binheap-empty? binheap) (reverse acc)
	(loop (cons (binheap-pop! binheap) acc)))))

(define (binheap-empty? binheap)
  (= (struct-ref binheap 1) 0))

(define (binheap-insert! binheap item)
  (when (= (struct-ref binheap 1) (vector-length (struct-ref binheap 0)))
      (let* ((padding (make-vector (vector-length (struct-ref binheap 0))))
	     (new-vector (vector-append (struct-ref binheap 0) padding)))
	(struct-set! binheap 0 new-vector)))
  (vector-set! (struct-ref binheap 0) (struct-ref binheap 1) item)
  (struct-set! binheap 1 (1+ (struct-ref binheap 1)))
  (rebalance-up! binheap (1- (struct-ref binheap 1))))

(define (parent-loc pos)
  (if (= 0 pos) 0
      (quotient (1- pos) 2)))

(define (left-child pos)
  (1- (* 2 (1+ pos))))

(define (right-child pos)
  (* 2 (1+ pos)))

(define (rebalance-up! binheap pos)
  (let ((parent (parent-loc pos))
	(comp (struct-ref binheap 2))
	(vec (struct-ref binheap 0)))
    (when (and (not (= parent pos))
	       (comp (vector-ref vec pos)
		  (vector-ref vec parent)))
      (binheap-swap! binheap parent pos)
      (rebalance-up! binheap parent))))

(define (binheap-pop! binheap)
  (if (= 0 (struct-ref binheap 1))
      (error "tried to pop from empty binheap")
      (let* ((vec (struct-ref binheap 0))
	     (result (vector-ref vec 0)))
	(struct-set! binheap 1 (1- (struct-ref binheap 1)))
	(when (> (struct-ref binheap 1) 0)
	  (vector-set! vec 0 (vector-ref vec (struct-ref binheap 1)))
	  (rebalance-down! binheap 0))
	result)))

(define (binheap-swap! binheap pos1 pos2)
  (let* ((vec (struct-ref binheap 0))
	 (temp (vector-ref vec pos1)))
    (vector-set! vec pos1 (vector-ref vec pos2))
    (vector-set! vec pos2 temp)))

(define (rebalance-down! binheap pos)
  (when (< (* 2 pos) (1- (struct-ref binheap 1)))
    (let ((left (left-child pos))
	  (right (right-child pos))
	  (last-pos (1- (struct-ref binheap 1)))
	  (vec (struct-ref binheap 0))
	  (comp (struct-ref binheap 2)))
      (if (and (<= left last-pos)
	       (<= right last-pos))
	  (if (comp (vector-ref vec left) (vector-ref vec pos))
	      (if (comp (vector-ref vec right) (vector-ref vec left))
		  (begin
		    (binheap-swap! binheap right pos)
		    (rebalance-down! binheap right))
		  (begin
		    (binheap-swap! binheap left pos)
		    (rebalance-down! binheap left)))
	      (when (comp (vector-ref vec right) (vector-ref vec pos))
		(binheap-swap! binheap right pos)
		(rebalance-down! binheap right)))
	  (when (and (<= left last-pos)
		     (comp (vector-ref vec left) (vector-ref vec pos)))
	    (binheap-swap! binheap left pos)
	    (rebalance-down! binheap left))))))



