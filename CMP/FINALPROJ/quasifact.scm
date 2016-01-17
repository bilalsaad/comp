
(define factorial
    (lambda(n)
      (if (zero? n)
          1
          `(* ,n ,(factorial (- n 1))))))
#;(define f
  (lambda(n)
    (if (zero? n)
      1
      (* n (f (- n 1))))))

#;(define (g x) (+ x 1))
#;(* (g 1) (g 2) (g 3))
(define fa
  (lambda(n)
    (if (zero? n)
      "nananna" 
      (fa (+ n  -1)))))

(factorial 4)

#;(* 5 (+ 5 -1) (+ 5 -2) (+ 5 -3) (+ 5 -4) 1)
(/ 1 2 3 4 5 6 7 8 9 10 11)
'(* 4 (* 3 ( * 2 (* 1 1))))
