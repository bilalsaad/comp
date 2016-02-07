;set-car!
(set-car! '(a b c) '(x y z))                ; #<void>, print nothing
;(set-car! '() 5)                            ; exception - not a pair
;(set-car! "set-car!" set-car!)              ; exception - not a pair
(set-car! '(1 2 3) (cdr '(1 2 3)))          ; #<void>, print nothing
;(set-car!)                                  ; excpetion - wrong number of arguments
;(set-cdr!)                                  ; excpetion - wrong number of arguments
; run the next 5 test together, as they test mutation
(define l '(1 2 3))                         ; #<void>, print nothing
(set-car! l '(4 5))                         ; #<void>, print nothing
l                                           ; ((4 . (5 . ())) . (2 . (3 . ())))
(car l)                                     ; (4 . (5 . ()))
(cdr l)                                     ; (2 . (3 . ()))



"test02.scm (revised), should return #t"

 (boolean? #t)
 (boolean? #f)
 (not (boolean? 1234))
 (not (boolean? 'a))
 (symbol? 'b)
(cons (procedure? procedure?) 1)
 (eq? (car '(a b c)) 'a)
 (= (car (cons 1 2)) 1)
 (integer? 1234)
 (char? #\a)
 (null? '())
 (string? "abc")
 (symbol? 'lambda)
 (vector? '#(1 2 3))
 (not (vector? 1234))
 (not (string? '#(a b c)))
 (not (string? 1234))
 (= 3 (vector-length '#(a #t ())))
 (pair? '(a . b))
 (not (pair? '()))
 (zero? 0)
 (not (zero? 234))
 (= 97 (char->integer (string-ref "abc" 0)))
 (let ((n 10000))
   (= n (string-length (make-string n #\a))))
 (let ((n 10000))
   (= n (vector-length (make-vector n #\a))))
 (let ((v '#(a b c)))
   (eq? 'c (vector-ref v 2)))
 (= 65 (char->integer #\A))
 (let ((string (make-string 2 #\a)))
   (string-set! string 0 (integer->char 97))
   (string-set! string 1 (integer->char 98))
   (eq? 'ab (string->symbol string)))
 (= 3 (remainder 7 4))
 (= 6 (* 1 2 3))
 (= 1 (*))
 (= 234 (* 234))
 (= 6 (+ 1 2 3))
 (zero? (+))
 (= 234 (+ 234))
 (= 1 (- 6 3 2))
 (< 1 2 3 4 5)
 (> 5 4 3 2 1)


