(define (cons x y)
  (lambda (m) (m x y))) ; this expect fn must have 2 arg x & y

(define (car z)
  (z (lambda (p q) p))) ; put this lambda into m in the cons fn

(define (cdr z)
 (z (lambda (p q) q)))

