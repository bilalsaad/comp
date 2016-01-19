(eq? #t (number? 5))
(eq? 1 1)
(eq? (+ 1 1) (+ 1 1))
(eq? "abc" "abc")
(define a 'abc)
(eq? "abc" (symbol->string a))
(eq? #\a #\a) 
(eq? "aaa" (make-string 3 #\a))
"aaa" 
(make-string 3 #\a)
(define 3a (make-string 3 #\a))
(eq? (string-ref 3a 0) (string-ref 3a 0)) 

