(define (ascending? s) 
   (if (or (null? s) (null? (cdr s)))
       #t
       (and (or (= (car s) (car (cdr s))) (< (car s) (car (cdr s)))) (ascending? (cdr s)))
   )
)

(define (my-filter pred s) 
    (if (null? s)
        s
        (if (pred (car s))
            (append (cons (car s) (my-filter pred (cdr s))))
            (my-filter pred (cdr s))
        )
    )
)

(define (interleave lst1 lst2) 
    (if (null? lst1)
        (if (null? lst2)
            ()
            lst2
        )
        (cons (car lst1) (interleave lst2 (cdr lst1)))
    )
)

(define (no-repeats s) 
    (define (in-rest-of x m)
        (if (null? m)
            #f
            (or (= x (car m)) (in-rest-of x (cdr m))) 
        ) 
    )
    (if (null? s) 
        s
        (if (in-rest-of (car s) (cdr s))
            (no-repeats (cdr s))
            (cons (car s) (no-repeats (cdr s)))
        )
    )
)
