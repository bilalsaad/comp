(define foo
  (lambda(a b c . r) r))
(foo 1 2 3 4 5 6 7 (foo #\a #\b #\c))

(apply + (map * '(1 2 3) '(4 5 6) '(8 9 10)))

(define goo 
 (lambda(a b c)
  (lambda(c d e)
    (lambda(f g h)
      (lambda(i j k)
        (lambda(l m n)
          (lambda(o p q)
            (lambda(r s t)
              (lambda(u v w)
                (lambda(x y z)
                  
                  (list a b c d e f g h 
                        i j k l m n o p q r s t u v w x y z)))))))))))
(((((((((goo 1 2 3) 
        4 5 6) 6 7 8) 10 11 12) 13 14 15) 16 17 18) 
   19 20 21) 22 23 24) 25 26 27)
(number? 5)
