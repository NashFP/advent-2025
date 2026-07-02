(define-module (mwlib regex)
  #:autoload (ice-9 regex) (regexp-match? string-match match:end match:start match:substring)
  #:export (regex-split-string make-regex-splitter regex-group-bind))

;;; Use string-match to split a string based on a regex describing the separator
;;; For example:
;;; (regex-split-string ",\\s+" "moe,      larry,    curly")
;;; Returns: ("moe" "larry" "curly")
(define (regex-split-string pattern target)
  (let loop ((s (string-match pattern target))
	     (matches '())
	     (start 0))
    (if (not (regexp-match? s))
	(if (< start (string-length target))
	    (reverse (cons (substring target start) matches))
	    (reverse matches))
	(loop (string-match pattern target (match:end s))
	      (if (< start (match:start s))
		  (cons (substring target start (match:start s)) matches)
		  matches)
	      (match:end s)))))

;;; Create a regex splitter (split a string based on a regex pattern) using
;;; a compiled regex. It returns a function that takes a string and returns
;;; a list of the items
;;; Example:
;;; (define comma-space-splitter (make-regex-splitter ",\\s+")
;;; (comma-space-splitter "moe,      larry,      curly")
;;; Returns: ("moe" "larry" "curly")
(define (make-regex-splitter pattern)
  (let ((splitter (make-regexp pattern)))
    (lambda (target)
      (let loop ((s (regexp-exec splitter target))
		 (matches '())
		 (start 0))
	(if (not (regexp-match? s))
	    (if (< start (string-length target))
		(reverse (cons (substring target start) matches))
		(reverse matches))
	    (loop (regexp-exec splitter target (match:end s))
		  (if (< start (match:start s))
		      (cons (substring target start (match:start s)) matches)
		      matches)
		  (match:end s)))))))


;;; regex-group-bind is similar to cl-ppcre:register-groups-bind in Common Lisp,
;;; but instead of providing the regex and target, you pass in the match result.
;;; This way it works with both string-match and regexp-exec
;;;
;;; Thanks to Shawn on Stack Overflow for showing me how to do this with
;;; syntax-case
;;;
;;; Examples:
;;; (regex-group-bind (foo bar baz) (string-match "(\\w+) (\\w+) (\\w+)" "moe larry curly")
;;;   (list baz bar foo))
;;; gives ("curly" "larry" "moe")
;;;
;;; (regex-group-bind (n1 n2 s1 s2) (string-match "([0-9]+) ([0-9]+) (\\w+) (\\w+)" "123 789 foo bar")
;;;    (list s1 n1 s2 n2))
;;; gives ("foo" "123" "bar" "789")
;;;
;;; You can supply a function to apply on one or more variables:
;;; (regex-group-bind ((string->number n1 n2) s1 s2) (string-match "([0-9]+) ([0-9]+) (\\w+) (\\w+)" "123 789 foo bar")
;;;    (list s1 n1 s2 n2))
;;; gives ("foo" 123 "bar" 789)
;;;
;;; You can use #f to skip a match group and not bind it:
;;; (regex-group-bind (s1 #f s2 #f s3) (string-match "(\\w+) (\\w+) (\\w+) (\\w+) (\\w+)"
;;;      "one two three four five") (list s1 s2 s3))
;;; gives ("one" "three" "five")

(define-syntax regex-bind-helper
  (lambda (stx)
    (syntax-case stx ()
      ((_ () (let-bindings ...) match counter body ...)
       #'(if match
	     (let (let-bindings ...) body ...)))
      ((_ ((fn) var-bindings ...) (let-bindings ...) match counter body ...)
       #'(regex-bind-helper (var-bindings ...) (let-bindings ...) match counter body ...))
      ((_ ((fn #f vars ...) var-bindings ...) (let-bindings ...) match counter body ...)
       #`(regex-bind-helper ((fn vars ...) var-bindings ...)
			    (let-bindings ...)
			    match
			    #,(+ (syntax->datum #'counter) 1) body ...))
      ((_ ((fn var vars ...) var-bindings ...) (let-bindings ...) match counter body ...)
       #`(regex-bind-helper ((fn vars ...) var-bindings ...)
			    ((var (fn (match:substring match counter))) let-bindings ...)
			    match
			    #,(+ (syntax->datum #'counter) 1) body ...))
      ((_ (#f var-bindings ...) (let-bindings ...) match counter body ...)
       #`(regex-bind-helper (var-bindings ...) (let-bindings ...) match
			    #,(+ (syntax->datum #'counter) 1) body ...))
      ((_ (var var-bindings ...) (let-bindings ...) match counter body ...)
       #`(regex-bind-helper (var-bindings ...)
			    ((var (match:substring match counter)) let-bindings ...)
			    match
			    #,(+ (syntax->datum #'counter) 1) body ...)))))

(define-syntax regex-group-bind
  (lambda (stx)
    (syntax-case stx ()
      ((_ (bindings ...) re-match body ...)
       #'(regex-bind-helper (bindings ...) () re-match 1 body ...)))))
