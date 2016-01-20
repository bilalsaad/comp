(define fact
  (lambda(n)
    (if (zero? n)
      1
      `(* ,n ,(fact (- n 1))))))


(fact 5)
(+ 1/2 1/4 1/8 1/16 1/32 1/64 1/128)
