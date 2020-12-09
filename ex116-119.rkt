(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt (square b) (/ n 2)))
        (else     (* b (fast-expt b (- n 1))))))

(define (fast-expt-with-state b n)
  (define(expt a b n)
    (cond ((= n 0) a) ; if n = 0 then a*b^0
          ((even? n) (expt a (square b) (/ n 2))) ; if even retain a
          (else (expt (* a b) b (- n 1))))) ; if not a=a*b
  (expt 1 b n))

(define (double a) (+ a a))
(define (halve a) (/ a 2))

(define (* a b)
 (cond ((= b 0) 0)
       ((even? b) (* (double a) (halve b)))
       (else (* (+ a b) (- b 1)))))

(define (fast-mult b n)
  (define(iter a b n)
    (cond ((= n 0) a) ; if n = 0 then a*b^0
          ((even? n) (iter a (double b) (halve n))) ; if even retain a
          (else (iter (+ a b) b (- n 1))))) ; if not a=a*b
  (iter 0 b n))

(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count) (fib-iter a
                                   b
                                   (+ (square q) (square p)) ;compute p'
                                   (+ (square q) (* 2 p q))  ;compute q'
                                   (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))



