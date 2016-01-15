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
      (list (lambda() (set! x 1))   y z))))

(define l1 (qwop #\a #\b #\c))
(l1)
