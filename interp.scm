#! /usr/bin/env chibi-scheme -r -A ./

(import (chibi) (chibi match)
	(lib misc)
	(scheme cxr)
	)

;; Very interesting thing I discovered about Chibi while
;; coding this: a procedure defined within a procedure is
;; not accessible to (eval).
(define (zero? a) (= a 0))
(define (inc a) (+ a 1))
(define (dec a) (- a 1))
(define (opr? o)
  (and (symbol? o)
       (or (eq? 'zero? o) (eq? 'inc o) (eq? 'dec o)
	   (eq? '* o) (eq? 'if o) )) )
(define (op o x e)
  (match `(,o ,@x)
    (('if a b c) (if (evil a e) (evil b e) (evil c e)) )
    ((o a b)   ((eval o) (evil a e) (evil b e)) )
    ((o a)     ((eval o) (evil a e)) )
    (else (error "op says: bleh") )) )

(define empty-env '())
(define (lookup x e)
  (cond
   ((null? e) (error "lookup bleh") )
   ((eq? (caar e) x) (cdar e) )
   (else (lookup x (cdr e)) )) )

(define (evil x e)
  (match x
    ((? number?) x )
    (((? opr? o) . x) (op o x e) )
    ((? symbol? x) (lookup x e) )
    (('λ ((? symbol? p)) b) `(closure ,e ,p ,b) )
    ((x y) (appl (evil x e) (evil y e)) )
    (else (error "bleh") )) )

(define (appl p v)
  (match p
    (('closure e p b) (evil b `((,p . ,v) . ,e)) )
    (else (error "appl bleh") )) )

(define (main args)
  (c-e (evil '(* 3 5) empty-env) 15)
  (c-e (evil '(if (zero? 0) 3 4) empty-env) 3)
  (c-e (evil '(if (zero? 1) 3 4) empty-env) 4)
  (c-e (evil '(λ (x) y) empty-env) '(closure () x y))
  (c-e (evil '(λ (y) (* y y)) (cons `(z . 17) empty-env))
       '(closure ((z . 17)) y (* y y)))
  (c-e (evil '((λ (y) (* y y)) (inc 5)) (cons `(z . 17) empty-env)) 36)
  (c-e (evil '((λ (y) (* y z)) (inc 5)) (cons `(z . 17) empty-env)) 102)
  (c-e (evil '(((λ (y) (λ (z) (* y z))) (inc 4)) (dec 7)) empty-env) 30)
  (c-e (evil 'y (cons '(y . 5) empty-env)) 5)
  (c-e (evil '(inc y) (cons '(y . 5) empty-env)) 6)
  (c-e (evil '5 empty-env) 5)

  ;; Factorial, an important test for interpreter environments, or so I'm told
  (c-e (evil '(((λ (!)
		  (λ (n)
		    ((! !) n) ) )
		(λ (!)
		  (λ (n)
		    (if (zero? n)
			1
			(* n ((! !) (dec n)))) ) ))
	       5)
	     empty-env) 120)
  (dsp ""))
