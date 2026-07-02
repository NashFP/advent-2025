(define-module (mwlib util)
  #:autoload (ice-9 rdelim) (read-line)
  #:autoload (language tree-il) (tree-il->scheme)
  #:autoload (srfi srfi-1) (fold append-map!)
  #:export (read-file map-file fold-file split-groups $ expand-macro
		      string-vector-fold map-index range
		      cartesian-product))

(define (read-file filename)
  (let ((p (open-input-file filename)))
    (let loop ((line (read-line p)) (lines '()))
      (if (eof-object? line)
	  (begin
	    (close-input-port p)
	    (reverse lines))
	  (loop (read-line p) (cons line lines))))))

(define (map-file fn filename)
  (let ((p (open-input-file filename)))
    (let loop ((line (read-line p)) (lines '()))
      (if (eof-object? line)
	      (begin
	        (close-input-port p)
	        (reverse lines))
	  (loop (read-line p) (cons (fn line) lines))))))

(define (fold-file fn init filename)
  (let ((p (open-input-file filename)))
    (let loop ((line (read-line p)) (curr-val init))
      (if (eof-object? line)
	  (begin
	    (close-input-port p)
	    curr-val)
	  (loop (read-line p) (fn line curr-val))))))

(define (map-index f lst)
  (let loop ((l lst)
	     (n 0)
	     (acc '()))
    (if (null? l) (reverse acc)
	(loop (cdr l) (1+ n) (cons (f n (car l)) acc)))))

(define (split-groups lines groups group)
  (cond ((null? lines)
	 (reverse (if (null? group) groups (cons (reverse group) groups))))
	((= (string-length (car lines)) 0)
	 (split-groups (cdr lines) (cons (reverse group) groups) '()))
	(#t (split-groups (cdr lines) groups (cons (car lines) group)))))

(define (string-vector-fold fn init grid)
  (let ((height (vector-length grid))
	(width (string-length (vector-ref grid 0))))
    (let y-loop ((y 0)
		 (curr-val init))
      (if (>= y height) curr-val
	  (let x-loop ((x 0)
		       (curr-val curr-val))
	    (if (>= x width) (y-loop (1+ y) curr-val)
		(x-loop (1+ x) (fn curr-val grid x y))))))))

(define (range start end)
  (let loop ((curr start)
	     (acc '()))
    (if (= curr end) (reverse acc)
	(loop (1+ curr) (cons curr acc)))))

(define* (cartesian-product #:rest lists)
  (define (product-2 a b)
    (append-map!
     (lambda (bx) (map (lambda (ax)
			 (if (list? bx)
			     (cons ax bx)
			     (list ax bx))) a)) b))
  (let ((rev-list (reverse lists)))
    (fold product-2 (car rev-list) (cdr rev-list))))

(define (replace-amp-in-tree s l)
  (if (list? l)
      (map (lambda (x) (replace-amp-in-tree s x)) l)
      (if (equal? l '&) s l)))

(define-syntax $
  (lambda (x)
    (syntax-case x ()
      ((k args ...)
       (let ((s (gensym)))
	 (datum->syntax
	  #'k
	  (cons
	   'lambda
	   (cons
	    (cons s '())
	    (replace-amp-in-tree s (syntax->datum #'(args ...)))))))))))

(define (expand-macro m)
  (tree-il->scheme (macroexpand m)))
