(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

(define (reverse l)
  (define (iter items result)
   (if (null? items)
      result
      (iter (cdr items) (cons (car items) result))))
  (iter l nil))
