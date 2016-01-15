#;(define foo
  (lambda(f . s)
    (if (null? (car s))
      '()
      (map_simple cdr s))))
(define map2
  (lambda(f)
        (cons (apply f '(1)) (apply f '(2)))))

;(eq? 'aaa (string->symbol (make-string 3 #\a)))


(integer->char 48)

(define q 1)
q
((lambda(x) 
   (set! x 'bilal)
   x) 7)
(define qwop
  (lambda(x y z)
    (lambda()
      (list (lambda() (set! x 1)) (lambda() x)  y z))))

(define l1 (qwop #\a #\b #\c))
(define foo (l1))


((car foo))

((car (cdr foo)))

(div_fracs 7/9 1 2 3 4 5 6 7 8 ) 
;(div_fracs 1/2  1/2)
(define p (cons 1 2))
(set_cdr p "bilal")
(define b "nilal")
(string_set b 0 #\b )
b
(define v (vector 1 2 3 4 5 6 7 8))
(vector_set v 5 "abcdefghijklmnopqrstuvwxyz")
v
