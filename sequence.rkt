(define (map f seq)
  (if (null? seq)
      nil
      (cons (f (car seq))
            (map f (cdr seq)))))

(define (filter pred? seq)
 (cond ((null? seq) nil)
       ((pred? (car seq)) (cons (car seq)
                                (filter pred? (cdr seq))))
       (else (filter pred? (cdr seq)))))

(define (accumulate f base seq)
  (if (null? seq)
      base
      (f (car seq) (accumulate f base (cdr seq)))))

(define (map-as-acc p seq)
  (accumulate (lambda (x y) (cons (p x)
                                  y))
              nil
              seq))

(define (enumerate-interval start end)
  (if (> start end) nil
                    (cons start (enumerate-interval (+ start 1) end))))

(enumerate-interval 1 5)

(accumulate
  append nil (map (lambda (i)
                    (map (lambda (j) (list i j))
                         (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 3)))

(mcons (mcons 2 (mcons 1 '())) (mcons (mcons 3 (mcons 1 '())) (mcons (mcons 3 (mcons 2 '())) '())))

