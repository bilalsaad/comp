Printf.printf("\n\n\nRegression tests. The last test message before an exception indicates which test failed.\n\n\n");;

Printf.printf "-----Test 01: single nested define-----\n\nScheme:\n%s\n\n" "(define foo (lambda (x) (define y 2) (+ x y)))";;
Semantics.run_semantics (Tag_Parser.read_expression "(define foo (lambda (x) (define y 2) (+ x y)))");;

Printf.printf "-----Test 02: some nested define in implicit sequence-----\n\nScheme:\n%s\n\n" "(define (foo x y z) (define t 2) (define r 9) (+ x y z t r))";;
Semantics.run_semantics (Tag_Parser.read_expression "(define (foo x y z) (define t 2) (define r 9) (+ x y z t r))");;

Printf.printf "-----Test 03: nested define in explicit sequence-----\n\nScheme:\n%s\n\n" "(define (foo x y z) (begin (define t 2) (define r 9)) (+ x y z t r))";;
Semantics.run_semantics (Tag_Parser.read_expression "(define (foo x y z) (begin (define t 2) (define r 9)) (+ x y z t r))");;

Printf.printf "-----Test 04: twice-nested define in explicit sequence-----\n\nScheme:\n%s\n\n" "(define (goo a b c . d) (begin (begin (define t1 1) (define t2 2)) (+ a b c c
(car d) t2 t1)))";;
Semantics.run_semantics (Tag_Parser.read_expression "(define (goo a b c . d) (begin (begin (define t1 1) (define t2 2)) (+ a b c c
(car d) t2 t1)))");;

Printf.printf "-----Test 05: tree-nesting define-----\n\nScheme:\n%s\n\n" "(define (goo . d) (begin (begin (begin (define t0 0) (define t1 1)) (define t2
 2)) (+ a b c c (car d) t2 t1)))";;
Semantics.run_semantics (Tag_Parser.read_expression "(define (goo . d) (begin (begin (begin (define t0 0) (define t1 1)) (define t2
 2)) (+ a b c c (car d) t2 t1)))");;

Printf.printf "-----Test 06: more complex tree-nesting define-----\n\nScheme:\n%s\n\n" "
((lambda (a b c)
   (begin
    (define t 1)
     (begin
       (begin
         (define + *)
         (define r -5/5)
         (define hex 0xffff))))
   (+ r hex r r t))
  list car cdr)
";;
Semantics.run_semantics (Tag_Parser.read_expression "
((lambda (a b c)
   (begin
    (define t 1)
     (begin
       (begin
         (define + *)
         (define r -5/5)
         (define hex 0xffff))))
   (+ r hex r r t))
  list car cdr)
");;

Printf.printf "-----Test 07: shouldn't pass - nested define after non-define expression-----\n\nScheme:\n%s\n\n" "
  ((lambda (a b c)
     (begin
      (define t 1)
       (begin
         (begin
           (define + *)
           (define r -5/5)
           (define hex 0xffff))))
     (+ r hex r r t)
     (define k 8))
   list car cdr)";;
(* should raise X_syntax error *)
(* Semantics.run_semantics (Tag_Parser.read_expression
  "((lambda (a b c)
     (begin
      (define t 1)
       (begin
         (begin
           (define + *)
           (define r -5/5)
           (define hex 0xffff))))
     (+ r hex r r t)
     (define k 8))
   list car cdr)");; *)

Printf.printf "-----Test 08: box craziness, explicit get/set/bound occurrences-----\n\nScheme:\n%s\n\n" "
(lambda (x)
    (x (lambda (x)
         (x (lambda (x y)
              (x y (lambda (z)
                      x
                      y
                     (set! z (/ z 2))
                     (set! y 1)
                     (set! x 3))))))
       (set! x 5)))
";;
Semantics.run_semantics (Tag_Parser.read_expression "
(lambda (x)
    (x (lambda (x)
         (x (lambda (x y)
              (x y (lambda (z)
                      x
                      y
                     (set! z (/ z 2))
                     (set! y 1)
                     (set! x 3))))))
       (set! x 5)))
");;

Printf.printf "-----Test 09: box craziness, implicit get/set/bound occurrences-----\n\nScheme:\n%s\n\n" "
(lambda (x)
    (x (lambda (x)
         (x (lambda (x y)
              (x y (lambda (z)
                     (set! z (/ z 2))
                     (set! y 1)
                     (set! x 3))))))
       (set! x 5)))
";;
Semantics.run_semantics (Tag_Parser.read_expression "
(lambda (x)
    (x (lambda (x)
         (x (lambda (x y)
              (x y (lambda (z)
                     (set! z (/ z 2))
                     (set! y 1)
                     (set! x 3))))))
       (set! x 5)))
");;

Printf.printf "-----Test 10: box craziness with nested define and optional\variadic lambda-----\n\nScheme:\n%s\n\n" "
(lambda s
    (define s (car s))
     (s
       (lambda (x)
         (x (lambda (x y)
              (x y (lambda (z . r)
                     (set! z (+ z ((lambda (n) (* n n)) 5)))
                     (set! x 1)
                     (set! y 2)))))))
     (set! s 6))
";;
Semantics.run_semantics (Tag_Parser.read_expression "
(lambda s
    (define s (car s))
     (s
       (lambda (x)
         (x (lambda (x y)
              (x y (lambda (z . r)
                     (set! z (+ z ((lambda (n) (* n n)) 5)))
                     (set! x 1)
                     (set! y 2)))))))
     (set! s 6))
");;

Printf.printf "-----Test 11: going bananas!-----\n\nScheme:\n%s\n\n" "
(lambda (s . t)
    (define t (car t))
    s
    (lambda (k)
      (k
        (lambda k
          (k
            s
            (lambda (t)
              (set! t (+ t (lambda (f) (f t))))
              (set! k 1)
              (set! s (+ s 2)))))))
    (set! s 5))
";;
Semantics.run_semantics (Tag_Parser.read_expression "
(lambda (s . t)
    (define t (car t))
    s
    (lambda (k)
      (k
        (lambda k
          (k
            s
            (lambda (t)
              (set! t (+ t (lambda (f) (f t))))
              (set! k 1)
              (set! s (+ s 2)))))))
    (set! s 5))
");;

Printf.printf "-----Test 12: iddqd-----\n\nScheme:\n%s\n\n" "
(define (foo goo hoo)
    (begin
      (define t1 1)
      (define t2 2)
      (begin
        (begin
          (begin
            (begin
              (define t3 3)
              (begin
                (define t4 4)))
            (define t5 5))
          (define t6 6))
        (define t7 7))
      (define t8 8))
    (+ 1 2)
    (goo (goo hoo))
    (lambda s
      (list
        (car s)
        (cdr s)))
    (lambda (x)
      (x
        (lambda (x)
          (x
            (lambda (x y z)
              (x
                y
                z
                (lambda (z)
                  (set! z *)
                  (z (z (z y y) y) y)
                  (set! y 5)))))))))
";;
Semantics.run_semantics (Tag_Parser.read_expression "
(define (foo goo hoo)
    (begin
      (define t1 1)
      (define t2 2)
      (begin
        (begin
          (begin
            (begin
              (define t3 3)
              (begin
                (define t4 4)))
            (define t5 5))
          (define t6 6))
        (define t7 7))
      (define t8 8))
    (+ 1 2)
    (goo (goo hoo))
    (lambda s
      (list
        (car s)
        (cdr s)))
    (lambda (x)
      (x
        (lambda (x)
          (x
            (lambda (x y z)
              (x
                y
                z
                (lambda (z)
                  (set! z *)
                  (z (z (z y y) y) y)
                  (set! y 5)))))))))
");;

Printf.printf "-----Test 13: should fail, nested define expression after non-define expression in body-----\n\nScheme:\n%s\n\n" "
(define (foo goo hoo)
    (begin
      (define t1 1)
      (define t2 2)
      (begin
        (begin
          (begin
            (begin
              (define t3 3)
              (begin
                (define t4 4)))
            (define t5 5))
          (define t6 6))
        (define t7 7))
      (define t8 8))
    (+ 1 2)
    (define goo /)
    (goo (goo hoo))
    (lambda s
      (list
        (car s)
        (cdr s)))
    (lambda (x)
      (x
        (lambda (x)
          (x
            (lambda (x y z)
              (x
                y
                z
                (lambda (z)
                  (set! z *)
                  (z (z (z y y) y) y)
                  (set! y 5)))))))))
";;
(* Semantics.run_semantics (Tag_Parser.read_expression "
(define (foo goo hoo)
    (begin
      (define t1 1)
      (define t2 2)
      (begin
        (begin
          (begin
            (begin
              (define t3 3)
              (begin
                (define t4 4)))
            (define t5 5))
          (define t6 6))
        (define t7 7))
      (define t8 8))
    (+ 1 2)
    (define goo /)
    (goo (goo hoo))
    (lambda s
      (list
        (car s)
        (cdr s)))
    (lambda (x)
      (x
        (lambda (x)
          (x
            (lambda (x y z)
              (x
                y
                z
                (lambda (z)
                  (set! z *)
                  (z (z (z y y) y) y)
                  (set! y 5)))))))))
");; *)

Printf.printf "-----Test 14: should fail, nested define expression in explicit sequence after non-define expression in body-----\n\nScheme:\n%s\n\n" "
(define (foo goo hoo)
    (begin
      (define t1 1)
      (define t2 2)
      (begin
        (begin
          (begin
            (begin
              (define t3 3)
              (begin
                (define t4 4)))
            (define t5 5))
          (define t6 6))
        (define t7 7))
      (define t8 8))
    (+ 1 2)
    (goo (goo hoo))
    (lambda s
      (list
        (car s)
        (cdr s)))
    (begin (define goo 5))
    (lambda (x)
      (x
        (lambda (x)
          (x
            (lambda (x y z)
              (x
                y
                z
                (lambda (z)
                  (set! z *)
                  (z (z (z y y) y) y)
                  (set! y 5)))))))))
";;
(* Semantics.run_semantics (Tag_Parser.read_expression "
(define (foo goo hoo)
    (begin
      (define t1 1)
      (define t2 2)
      (begin
        (begin
          (begin
            (begin
              (define t3 3)
              (begin
                (define t4 4)))
            (define t5 5))
          (define t6 6))
        (define t7 7))
      (define t8 8))
    (+ 1 2)
    (goo (goo hoo))
    (lambda s
      (list
        (car s)
        (cdr s)))
    (begin (define goo 5))
    (lambda (x)
      (x
        (lambda (x)
          (x
            (lambda (x y z)
              (x
                y
                z
                (lambda (z)
                  (set! z *)
                  (z (z (z y y) y) y)
                  (set! y 5)))))))))
");; *)

Printf.printf "-----Test 15: should fail, nested define expression after non-define expression in the deepest leaf of nested-define tree-----\n\nScheme:\n%s\n\n" "
(define (foo goo hoo)
    (begin
      (define t1 1)
      (define t2 2)
      (begin
        (begin
          (begin
            (begin
              (define t3 3)
              (begin
                (define t4 4)
                (+ 1 -1)))
            (define t5 5))
          (define t6 6))
        (define t7 7))
      (define t8 8))
    (+ 1 2)
    (goo (goo hoo))
    (lambda s
      (list
        (car s)
        (cdr s)))
    (lambda (x)
      (x
        (lambda (x)
          (x
            (lambda (x y z)
              (x
                y
                z
                (lambda (z)
                  (set! z *)
                  (z (z (z y y) y) y)
                  (set! y 5)))))))))
";;
(* Semantics.run_semantics (Tag_Parser.read_expression "
(define (foo goo hoo)
    (begin
      (define t1 1)
      (define t2 2)
      (begin
        (begin
          (begin
            (begin
              (define t3 3)
              (begin
                (define t4 4)
                (+ 1 -1)))
            (define t5 5))
          (define t6 6))
        (define t7 7))
      (define t8 8))
    (+ 1 2)
    (goo (goo hoo))
    (lambda s
      (list
        (car s)
        (cdr s)))
    (lambda (x)
      (x
        (lambda (x)
          (x
            (lambda (x y z)
              (x
                y
                z
                (lambda (z)
                  (set! z *)
                  (z (z (z y y) y) y)
                  (set! y 5)))))))))
");; *)

Printf.printf "-----Test 16: should fail, nested define expression after non-define expression not in the deepest leaf of nested-define tree-----\n\nScheme:\n%s\n\n" "
(define (foo goo hoo)
    (begin
      (define t1 1)
      (define t2 2)
      (begin
        (begin
          (begin
            (begin
              (define t3 3)
              (begin
                (define t4 4)))
            (define t5 5)
            (/ 2 9))
          (define t6 6))
        (define t7 7))
      (define t8 8))
    (+ 1 2)
    (goo (goo hoo))
    (lambda s
      (list
        (car s)
        (cdr s)))
    (lambda (x)
      (x
        (lambda (x)
          (x
            (lambda (x y z)
              (x
                y
                z
                (lambda (z)
                  (set! z *)
                  (z (z (z y y) y) y)
                  (set! y 5)))))))))
";;
(* Semantics.run_semantics (Tag_Parser.read_expression "
(define (foo goo hoo)
    (begin
      (define t1 1)
      (define t2 2)
      (begin
        (begin
          (begin
            (begin
              (define t3 3)
              (begin
                (define t4 4)))
            (define t5 5)
            (/ 2 9))
          (define t6 6))
        (define t7 7))
      (define t8 8))
    (+ 1 2)
    (goo (goo hoo))
    (lambda s
      (list
        (car s)
        (cdr s)))
    (lambda (x)
      (x
        (lambda (x)
          (x
            (lambda (x y z)
              (x
                y
                z
                (lambda (z)
                  (set! z *)
                  (z (z (z y y) y) y)
                  (set! y 5)))))))))
");; *)
