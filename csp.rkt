#lang lazy

(define (STOP _)
  'BLEEP)

(define (prefix e P)
  (lambda (x)
    (if
     (eq? e x)
     P
     'BLEEP)))


(define (choice2 c P d Q)
  (lambda (x)
    (cond
      [(eq? c x) P]
      [(eq? d x) Q]
      [#t        'BLEEP])))

(define (in x A)
  (cond
    [(not (cons? A)) #f]
    [(eq? x (car A)) #t]
    [#t              (in x (cdr A))]))

(define (choice B P)
  (lambda (x)
    (if
     (in x B)
     (P x)
     'BLEEP)))

(define (fix f)
  (letrec
      ([x (f x)])
    x))


(define VM
  (prefix 'coin (prefix 'choc STOP)))

(define VMS
  (fix (lambda (X) (prefix 'coin (prefix 'choc X)))))

(define CT
  (choice2 'up STOP 'right (prefix 'right (prefix 'right (prefix 'up STOP)))))

(define CLOCK
  (fix (lambda (X) (prefix 'tick X))))