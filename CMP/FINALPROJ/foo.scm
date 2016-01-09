

(define ls ((lambda s s) 1 2 3 4 5))
(define kl ((lambda s s)))
(define list (lambda s s))
(define map
  (lambda (ls f)
    (if (is_null ls)
          '()
          (cons (f (car ls)) (map (cdr ls) f)))))
(define sum
  (lambda(n)
    (if (is_zero n)
        n
        (plus n (sum (minus n 1))))))
;(map ls (lambda(x) (list x (plus x 1))))
(define (fact n)
  (if (is_zero n)
      1
      (mul n (fact (minus n 1)))))
(fact 5) 

(define fold
  (lambda(lst op init)
    (if (is_null lst)
        init
        (fold (cdr lst) op (op init (car lst)))))) 
(fold ls mul 1)

(define +
  (lambda s
    (fold s plus 0)))

(+ 1 2 3 4 5 6 7 8 9 10)
(define bin_eq
  (lambda (a b)
    (is_zero(minus a b))))
(bin_eq 1 4)
`(1 2 ,(fact 5)) 

(define foo
  (lambda(e)
    (if (is_null e)
         '()
         `("cons" ,(foo (car e)) ,(foo (cdr e))))))
(define p (cons 1 (cons 2 '())))

`("cons" ,(car p) ,(cdr p))
