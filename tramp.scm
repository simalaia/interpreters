#! /usr/bin/env -S chibi-scheme -r -A ./ -I $modules/self/schemeR7RS

(import (chibi) (chibi match)
	(lib misc)
	(srfi 9)
	(scheme cxr)
	)

;; Trampoline experiments

(define-record-type :done (return val) done? (val done))
(define-record-type :doing (bounce th) doing? (th doing))

(define-syntax bnc
	(syntax-rules ()
		((bnc body ...) (bounce (lambda () body ...)) )) )



(define (fac n acc)
		(if (zero? n)
			(return acc)
			(bnc (fac (- n 1) (* acc n)) )) )

(define (mem? n l)
		(cond
			((null? l) (return #f) )
			((= (car l) n) (return #t) )
			(else (bnc (mem? n (cdr l)) ) )) )

(define (facc acc)
	(lambda (n)
		(if (zero? n)
			(return acc)
			(bnc ((facc (* acc n)) (- n 1)))) ))
(define (memc? l)
	(lambda (n)
		(cond
			((null? l) (return #f) )
			((= (car l) n) (return #t) )
			(else (bnc ((memc? (cdr l)) n)) )) ))





(define (pogo-stick thrd)
	(match thrd
		(($ :done v) v )
		(($ :doing t) (pogo-stick (t)) )) )

(define (seesaw down up)
	(match down
		(($ :done v) v )
		(($ :doing t) (seesaw up (t)) )) )

(define (trampoline q)
	(match (car q)
		(($ :done v) v )
		(($ :doing t) (trampoline (append (cdr q) `(,(t)))) )) )

(define (sequence f t)
	(match t
		(($ :done v) (f v) )
		(($ :doing t) (bnc (sequence f (t))) )) )

(define (seq-comp f g) (lambda (x) (sequence f (g x))) )


(define (main args)
	(dspl (pogo-stick (fac 5 1)))
	(dspl (seesaw (fac -1 1) (mem? 120 '(100 110 120 130))))
	(dspl (trampoline
		`(,(fac -1 1)
		,(fac -1 1)
		,(mem? 120 '(100 110 120 130)))))
	(dspl (pogo-stick (mem? (pogo-stick (fac 5 1)) '(100 110 120 130))))
	(dspl (pogo-stick ((seq-comp (memc? '(100 110 120 130)) (facc 1)) 5)) )
	(dsp ""))
