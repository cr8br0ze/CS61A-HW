(define (my-filter pred s) 
                          (cond 
                          ((null? s) nil)
                          ((pred (car s)) (cons (car s) (my-filter pred (cdr s))))
                          (else (my-filter pred (cdr s)))
  )
)

(define (interleave lst1 lst2)
                              (cond
                              ((or (null? lst1) (null? lst2)) (append lst1 lst2))
                              (else (cons (car lst1) (cons (car lst2) (interleave (cdr lst1) (cdr lst2)))))
  )
)

(define (accumulate joiner start n term)
                                        (cond 
                                        ((= n 0) start)
                                        (else (accumulate joiner (joiner (term n) start) (- n 1) term))
  )
)
(define (no-repeats lst)
                        (cond
                        ((null? lst) nil)
                        (else (cons (car lst) (no-repeats (my-filter (lambda (x) (not (= x (car lst)))) (cdr lst)))))
  )
)
