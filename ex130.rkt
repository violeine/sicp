; iterative process sum
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;product in term of iterative process
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a )(* result (term a)))))
  (iter a 1))

;product in term of recursive tree
(define (product2 term a next b)
  (if (> a b)
      1
      (* (term a)
         (product2 term (next a) next b))))

(define (inc n) (+ n 1))

(define (identity n) n)

(define (factorial n)
  (product2 identity 1 inc n))

(define (plus2 n) (+ n 2))

(define (pi-term n)
 (* (/ (- n 1.0) n) (/ (+ n 1.0) n)))

(define (calcPi n)
 (* (product pi-term 3 plus2 n)
    4.0))
;recursive
(define (accumulate combinator base-value term a next b)
  (if (> a b)
    base-value
    (combinator (term a)
                (accumulate combinator base-value term (next a) next b))))
;iterative
(define (accumulate combinator base-value term a next b)
  (define (iter a res)
    (if (> a b) res
        (iter (next a) (combinator res (term a)))))
  (iter a base-value))


