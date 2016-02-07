;append (variadic)
(append '(1 2 3) '(4 5 6))                  ; (1 . (2 . (3 . (4 . (5 . (6 . ()))))))
(append '(1 2) '(3 4) '(5 6))               ; (1 . (2 . (3 . (4 . (5 . (6 . ()))))))
(append 'a)                                 ; a
(append '(1 2 3) '() '(1 2 . 3))            ; (1 . (2 . (3 . (1 . (2 . 3)))))
;
(define x '(1 2))
(define y '(3 4))
(define z (append x y))
;(set-car! x '*)
;(set-car! y '$)
z                                           ; (1 . (2 . ($ . (4 . ()))))
;
(append '(1) 2)                             ; (1 . 2)
;(append 2 1)                                ; exception - l1 not a list

;apply
(apply (lambda x x) '())                    ; ()
(apply (lambda x x) '(2))                   ; (2 . ())
(define fact-tail
    (lambda (n acc)
      (if (= 1 n)
          acc
          (fact-tail (- n 1) (* n acc)))))
(apply fact-tail '(5 1))                    ; 120
(define tail_test
 (lambda (n1)
      ((lambda (n2 n3)
            (+ n1 n3)) 10 15)))
(tail_test 1)                               ; 16
(apply tail_test '(1))                      ; 16
(apply tail_test '(2))                      ; 17
;(apply cons 1 2)                            ; exception - not a proper list
;(apply)                                     ; exception - wrong number of arguments
;(apply cons)                                ; exception - wrong number of arguments

;< (variadic)
(< 11)                                      ; #t
(< 1 2)                                     ; #t 
(< -1/2 1/121)                              ; #t ;DOES NOT WORK
(< 3 2)                                     ; #f
(< -3 -2 -1 0 1/2 2/3 3/4)                  ; #t
;(<)                                         ; exception - wrong number of arguments
;(< 'a 6)                                    ; exception - 'a is not a number

;= (variadic)
"variadic shit"
(= 999)                                     ; #t
(= 1 2)                                     ; #f
(= 3 3 3 3 -1)                              ; #f
(= 5 5 5 5 5 5 5 5 5 5 5)                   ; #t
(= 2 4/2)                                   ; #t
;(=)                                         ; exception - wrong number of arguments
;(= 1 '#(1))                                 ; exception - #(1) is not a number

;> (variadic)
(> 11)                                      ; #t
(> 1 2)                                     ; #f
(> -1/2 1/121)                              ; #f "also wrong fml"
(> 3 2)                                     ; #t
(> -3 -2 -1 0 1/2 2/3 3/4)                  ; #f
;(>)                                         ; exception - wrong number of arguments
;(> 'a 6)                                    ; exception - 'a is not a number

;+ (variadic)
(+)                                         ; 0
(+ 1 -1)                                    ; 0
(+ 3 -1/2)                                  ; 5/2
(+ 1/3 1/2)                                 ; 5/6
(+ 1/6 1/6)                                 ; 1/3
(+ 1/4 1/4 1/4 1/4)                         ; 1
(+ 1/2 1/3 1/4 1/5)                         ; 77/60
(+ 1/2 -1/3 1/4 -1/5)                       ; 13/60
;(+ 5 'a)                                    ; exception - 'a is not a number

;/ (variadic)
(/ 1 3)                                     ; 1/3
(/ 4 2)                                     ; 2
(/ 6 -2)                                    ; -3
(/ 1)                                       ; 1
(/ 7)                                       ; 1/7
(/ 1 -12)                                   ; -1/12
;(/)                                         ; exception - wrong number of arguments
;(/ 5 #t)                                    ; exception - #t is not a number

;* (variadic)
(*)                                         ; 1
(* 2)                                       ; 2
(* 5 2/7)                                   ; 10/7
(* -3 -3)                                   ; 9
(* -1/17 3/19)                              ; -3/323
;(* -1 't)                                   ; excpetion - t is not a number

;- (variadic)
;(-)                                         ; exception - wrong number of arguments
(- 3)                                       ; -3
(- 4/7)                                     ; -4/7
(- 1 -1)                                    ; 2
(- 3 -1/2)                                  ; 7/2
(- 1/3 1/2)                                 ; -1/6
(- 1/6 1/6)                                 ; 0
(- 1/4 1/4 1/4 1/4)                         ; -1/2
(- 1/2 1/3 1/4 1/5)                         ; -17/60
(- 1/2 -1/3 1/4 -1/5)                       ; 47/60
;(+ 5 'a)                                    ; exception - 'a is not a number

;boolean?
(boolean? #t)                               ; #t 1
(boolean? #f)                               ; #t 2
(boolean? (lambda (x y z) x y z y x))       ; #f 3 
(boolean? (if #f #f #f))                    ; #t 4
(boolean? (if #f #t))                       ; #f 5 
(boolean? (and 1 2))                        ; #f 6 
(boolean? (and #f #f))                      ; #t 7 
(boolean? (and))                            ; #t 8
;(boolean?)                                  ; excpetion - wrong number of arguments
(boolean? 5)                                ; #f

;car
(car '(1 2 3))                              ; 1
;(car 5)                                     ; exception - not a pair
;(car)                                       ; exception - wrong number of arguments
;(car '(1 2 3) 'a)                           ; exception - wrong number of arguments

;cdr
(cdr '(1 2 3))                              ; (2 . (3 . ()))
;(cdr 5)                                     ; exception - not a pair
;(cdr)                                       ; exception - wrong number of arguments
;(cdr '(1 2 3) 'a)                           ; exception - wrong number of arguments

;char->integer
(char->integer #\A)                         ; 65
(char->integer #\page)                      ; 12
;(char->integer 0)                           ; exception - 0 is not a char
;(char->integer)                             ; excpetion - wrong number of arguments

;char?
(char? #\5)                                 ; #t
(char? #\newline)                           ; #t
(char? -1/19)                               ; #f
;(char?)                                     ; excpetion - wrong number of arguments

;cons
(cons (lambda () #t) (if #f #t))            ; (#<procedure at...> . #<void>)
(cons 0xabc #t)                             ; (2748 . #t)
(cons 1 (cons -2 (cons 3 (cons #t '()))))   ; (1 . (-2 . (3 . (#t . ()))))
;(cons)                                      ; excpetion - wrong number of arguments
;(cons #f)                                   ; excpetion - wrong number of arguments

;denominator
;(denominator -2)                            ; 1
;(denominator 9/7)                           ; 7
;(denominator)                               ; excpetion - wrong number of arguments
;(denominator #\t)                           ; exception - #\t is not a rational number

;eq?
(eq? (if #f #f) (if #f #f))                 ; #t 1
(eq? '() '())                               ; #t 2
(eq? #f #f)                                 ; #t 3
(eq? #f #t)                                 ; #f 4
(eq? #\a #\A)                               ; #f 5 
(eq? #\newline #\newline)                   ; #t 6 
(eq? -1 -1)                                 ; #t 7 
(eq? -1 2)                                  ; #f 8 
(eq? 1/3 2/6)                               ; #t 9 
(eq? 1/3 1/4)                               ; #f 10 
(eq? "ab" "ab")                             ; #t 11 (because of constants table)
(eq? 'ab 'ab)                               ; #t 12 
(eq? 'e 'q)                                 ; #f 13
(eq? '(1 2) '(1 2))                         ; #t 14
(eq? '(1 2) '(1))                           ; #f
(eq? '#(vector) '#(vector))                 ; #t
(eq? '#(vector) '#(list))                   ; #f
;(eq?)                                       ; excpetion - wrong number of arguments
;(eq? 1 2 3)                                 ; excpetion - wrong number of arguments

;integer?
(integer? -10000000)                        ; #t
(integer? 3/14)                             ; #f
(integer? cons)                             ; #f
;(integer?)                                  ; excpetion - wrong number of arguments

;integer->char
(integer->char 10)                          ; #\newline
(integer->char 255)                         ; y with two dots above it
;(integer->char '())                         ; exception - () is not a scalar value
;(integer->char)                              ; excpetion - wrong number of arguments

;list (variadic)
(list list)                                 ; (#<procedure at ...> . ())
(list -1 2 -3 4)                            ; (-1 . (2 . (-3 . (4 . ()))))
(list #t #t #f #t)                          ; (#t . (#t . (#f . (#t . ()))))
(list)                                      ; ()

;make-string
;(make-string 3)                             ; "\000;\000;\000;"
(make-string 5 #\r)                         ; "rrrrr"
;(make-string #f)                            ; excpetion - #f is not a number
;(make-string)                               ; excpetion - wrong number of arguments

;make-vector
;(make-vector 7)                             ; #(0 0 0 0 0 0 0)
(make-vector 2 "scheme")                    ; #("scheme" "scheme")
;(make-vector "e")                           ; excpetion - e is not a number
;(make-vector)                               ; excpetion - wrong number of arguments

;map
(map (lambda (s) "batman") '("why" "so" "serious?"))  ; ("batman" . ("batman" . ("batman" . ())))
;(map #f '(1 cons list 'e))                  ; exception - #f is not a procedure
(map (lambda (x) x) '(-1 -2 -3/5))          ; (-1 . (-2 . (-3/5 . ())))
;(map (lambda (k) k))                        ; excpetion - wrong number of arguments

;not
(not #f)                                    ; #t
(not #t)                                    ; #f
(not 5)                                     ; #f
;(not)                                       ; excpetion - wrong number of arguments
(not apply)                                 ; #f

;null?
(null? '())                                 ; #t
(null? '(9 #\F))                            ; #f
;(null?)                                     ; excpetion - wrong number of arguments
(null? null?)                               ; #f

;number?
(number? -1)                                ; #t
(number? -3/14)                             ; #t
(number? #t)                                ; #f
(number? "hello, world!")                    ; #f
;(number?)                                   ; excpetion - wrong number of arguments
;(number? 1 2)                               ; excpetion - wrong number of arguments

;numerator
(numerator 4/2)                             ; 2
(numerator -7/17)                           ; -7
(numerator 3)                               ; 3
;(numerator 'const)                          ; exception - not a fraction
;(numerator)                                 ; excpetion - wrong number of arguments
;(numerator 1/2 1/3)                         ; excpetion - wrong number of arguments

;pair?
(pair? '(1 2))                              ; #t
(pair? '())                                 ; #f
;(pair?)                                     ; excpetion - wrong number of arguments
(pair? 5)                                   ; #f
;(pair? '(1 2) '(9))                         ; excpetion - wrong number of arguments

;procedure?
(procedure? procedure?)                     ; #t
(procedure? (lambda s (car s)))             ; #t
(procedure? #t)                             ; #f
;(procedure?)                                ; excpetion - wrong number of arguments
;(procedure? cons car cdr list)              ; excpetion - wrong number of arguments

;rational?
(rational? 5/2)                             ; #t
(rational? -99)                             ; #t
(rational? 0xffff)                          ; #t
(rational? "rational?")                     ; #f
;(rational?)                                 ; excpetion - wrong number of arguments
;(rational? 1/4 1/7)                         ; excpetion - wrong number of arguments

;remainder
(remainder 12 -5)                           ; 2
(remainder 70 7)                            ; 0
(remainder 0xffff 0xfffe)                   ; 1
;(remainder 2 "cookies")                     ; exception - not an integer
;(remainder 2 1/2)                           ; exception - not an integer
;(remainder)                                 ; excpetion - wrong number of arguments
;(remainder 1 2 3)                           ; excpetion - wrong number of arguments

;set-car!
;(set-car! '() 5)                            ; exception - not a pair
;(set-car! "set-car!" set-car!)              ; exception - not a pair
;(set-car!)                                  ; excpetion - wrong number of arguments
;(set-cdr!)                                  ; excpetion - wrong number of arguments
; run the next 5 test together, as they test mutation
                                   ; (4 . (5 . ()))

;string-length
(string-length "")                          ; 0
(string-length "a")                         ; 1
;(string-length 5)                           ; exception - not a string
;(string-length)                             ; excpetion - wrong number of arguments
;(string-length "a" "a")                     ; excpetion - wrong number of arguments

;string-ref
(string-ref "string" 1)                     ; #\t
;(string-ref 'string 1)                      ; exception - not a string
;(string-ref "string" 99)                    ; exception - out of bounds
;(string-ref "string" -1)                    ; exception - out of bounds
;(string-ref "string" 'a)                    ; exception - not an integer
;(string-ref)                                ; excpetion - wrong number of arguments
;(string-ref "string")                       ; excpetion - wrong number of arguments
;(string-ref "string" 1 2)                   ; excpetion - wrong number of arguments

;string-set!
(string-set! "string" 1 #\p)                ; #<void>, print nothing
;(string-set! "string" 95 #\o)               ; exception - out of bounds
;(string-set! "string" -1 #\o)               ; exception - out of bounds
;(string-set! "string" 1 "kk")               ; exception - not a char
;(string-set! 'string 1 #\p)                 ; exception - not a string
;(string-set! "string" 1/2 #\p)              ; exception - not an integer
;(string-set!)                               ; excpetion - wrong number of arguments
;(string-set! "wstring" 1)                   ; excpetion - wrong number of arguments
;(string-set! "wwstring" 0 #\r #\l)          ; excpetion - wrong number of arguments

;string->symbol
;'sym
(string->symbol "sym")                      ; sym
(string->symbol "freshwithsymbolsinprogram")   ; freshwithsymbolsinprogram

(eq? (string->symbol (make-string 1 #\a)) 'a)        ; strings should be comparisoned deeply when examining the symbol's string representation

(define str (symbol->string 'test))
(eq? (string->symbol str) 'test)            ; #t

(string->symbol "fresh")  ; fresh

;(string->symbol 'sym)                       ; exception - not a string
;(string->symbol)                            ; excpetion - wrong number of arguments
;(string->symbol "str" "sym")                ; excpetion - wrong number of arguments

;string?
(string? "string")                          ; #t
(string? 'string)                           ; #f
(string? 2)                                 ; #f
;(string? "yes" "no")                        ; excpetion - wrong number of arguments
;(string?)                                   ; excpetion - wrong number of arguments

;symbol?
(symbol? 'symbol)                           ; #t
(symbol? "symbol")                          ; #f
(symbol? 2)                                 ; #f
;(symbol? 'yes 'no)                          ; excpetion - wrong number of arguments
;(symbol?)                                   ; excpetion - wrong number of arguments

;symbol->string
(symbol->string 'sym)                       ; "sym"
;(symbol->string "sym")                      ; exception - not a symbol
;(symbol->string)                            ; excpetion - wrong number of arguments
;(symbol->string 'a 'b)                      ; excpetion - wrong number of arguments

;vector
(vector vector vector)                      ; #(#<procedure...> #<procedure...>)
(vector 'a 1 1/1 "uno" 'einz)               ; #(a 1 1 "uno" einz)
(vector)                                    ; #()

;vector-length
(vector-length '#())                        ; 0
(vector-length '#(vector? vector-ref))      ; 2
;(vector-length '#(1) '#(2))                 ; excpetion - wrong number of arguments
;(vector-length)                             ; excpetion - wrong number of arguments
;(vector-length '(v v v))                    ; exception - not a vector

;vector-ref
(vector-ref '#(1 2 3) 0)                    ; 1
;(vector-ref '#(1 2 3) 3)                    ; exception - out of bounds
;(vector-ref '#(1 2 3))                      ; excpetion - wrong number of arguments
;(vector-ref '#(1 2 3) 0 1)                  ; excpetion - wrong number of arguments
;(vector-ref "'#(1 2 3)" 0)                  ; exception - not a vector

;vector-set!
(vector-set! '#(4 5 6) 1 7)                 ; #<void>, print nothing
;(vector-set! '#(4 5 6) 3 7)                 ; exception - out of bounds
;(vector-set! '#(4 5 6) -1 7)                ; exception - out of bounds
;(vector-set! "vector" 1 2)                  ; exception - not a vector
;(vector-set!)                               ; excpetion - wrong number of arguments
;(vector-set! '#(4 5 6) 1)                   ; excpetion - wrong number of arguments
;(vector-set! '#(4 5 6) 1 7 8)               ; excpetion - wrong number of arguments

;vector?
(vector? "no")                              ; #f
(vector? '#(1 b 3 d 5 f))                   ; #t
;(vector?)                                   ; excpetion - wrong number of arguments
;(vector? 1 2)                               ; excpetion - wrong number of arguments

;zero?
(zero? 0)                                   ; #t
(zero? 0/3)                                 ; #t
(zero? (- 5 (+ 1 2 2)))                    ; #t
;(zero?)                                     ; excpetion - wrong number of arguments
;(zero? 0 0)                                 ; excpetion - wrong number of arguments


"-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*MAYER'S TEST-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"

"fact-x.scm, should return: 120"
(define fact
  (let ((x (lambda (x)
	     ((x (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
	      (lambda (x) (lambda (y) x)))))
	(->
	 ((lambda (x) (x x))
	  (lambda (->)
	    (lambda (n)
	      (if (zero? n)
		  (lambda (x) (lambda (y) y))
		  (let ((z ((-> ->) (- n 1))))
		    (lambda (x)
		      (lambda (y)
			(x ((z x) y)))))))))))
    (lambda (n)
      ((((((((x (x (x (x x)))) (((x (x (x (x x)))) ((x (x (x x))) (x
      (x (x (x x)))))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x x
      ))))) (x (x (x (x x))))))) ((x (x (x x))) (x (x (x x))))) ((((
      (x (x (x (x x)))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x
      (x x)))))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x x)))))
      (x (x (x (x x))))))) ((x (x (x x))) (x (x (x x))))) (((((x (x
      (x (x x)))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x (x x))
      )))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x x))))) (x (x
      (x (x x))))))) ((x (x (x x))) (x (x (x x))))) (((x (x (x (x x)
      ))) (x (x (x x)))) (x (x (x x))))) (((x (x (x(x x)))) (((((x (
      x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (x (x (x x)))
      ) (((x (x (x (x x)))) ((x (x (x x))) (((x(x (x (x x)))) (((x (
      x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (((x (x (x (x
      x)))) ((x (x (x x))) (x (x (x x))))) (x(x (x (x x))))))) ((x (
      x (x x))) (x (x (x x))))))) ((((x (x(x (x x)))) (((x (x (x (x
      x)))) ((x (x (x x))) (x (x (x (x x)))))) (((x (x (x (x x)))) (
      (x (x (x x))) (x (x (x x))))) (x(x (x (x x))))))) ((x (x (x x)
      )) (x (x (x x))))) (((x (x (x (x x)))) (x (x (x x)))) (x (x (x
      x))))))) (((((x (x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))
      ))) (x (x (x x)))) ((x (x(x (x x)))) (((x (x (x (x x)))) ((x (
      x (x x))) (x (x (x (x x)))))) (x (x (x x)))))) (((((x (x (x (x
      x)))) (((x (x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (
      ((x (x (x (x x)))) ((x (x (x x))) (x (x (x x))))) (x (x (x (x
      x))))))) ((x (x (x x))) (x (x (x x))))) (((x (x (x (x x)))) (x
      (x (x x)))) (x (x (x x))))) (x (x (x x))))))) (((x (x (x (x x)
      ))) (((((x (x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (
      x(x (x x)))) (((x(x (x (x x)))) ((x (x (x x))) (x (x (x (x x))
      )))) (x (x (x x))))) (((((x (x (x (x x)))) (((x (x (x (x x))))
      ((x (x (x x)))(x (x (x (x x)))))) (((x (x (x (x x)))) ((x (x (
      x x))) (x (x(x x))))) (x (x (x (x x))))))) ((x (x (x x))) (x (
      x (x x)))))(((x (x (x (x x)))) (x (x (x x)))) (x (x (x x)))))
      (x (x (x x)))))) (((((x (x (x (x x)))) (((x (x (x (x x)))) ((x
      (x (x x)))(x (x (x (x x)))))) (((x (x (x (x x)))) ((x (x (x x)
      )) (x (x(x x))))) (x (x (x (x x))))))) ((x (x (x x))) (x (x (x
      x)))))(((x (x (x (x x)))) (x (x (x x)))) (x (x (x x))))) ((x (
      x (x x))) (((x (x (x (x x)))) (x (x (x x)))) (x (x (x x)))))))
      )))(((((x (x (x (x x)))) ((x (x (x x))) (((x (x (x (x x)))) ((
      (x(x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (((x (x (x
      (x x)))) ((x (x (x x))) (x (x (x x))))) (x (x (x (x x)))))))((
      x (x (x x))) (x (x (x x))))))) ((((x (x (x (x x)))) (((x (x(x
      (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (((x (x (x (x x)))
      )((x (x (x x))) (x (x (x x))))) (x (x (x (x x))))))) ((x(x (x
      x))) (x (x (x x))))) (((x (x (x (x x)))) (x (x (x x))))(x (x (
      x x)))))) (((x (x (x (x x)))) (((x (x (x (x x)))) ((x (x (x x)
      ))(x (x (x (x x)))))) (x (x (x x))))) ((x (x (x x)))(((x (x (x
      (x x)))) (x (x (x x)))) (x (x (x x))))))) (((x (x(x (x x)))) (
      ((x (x (x (x x)))) ((x (x (x x))) (x (x (x (x x)))))) (x (x (x
      x))))) ((x (x (x x))) (((x (x (x (x x)))) (x(x (x x)))) (x (x
      (x x))))))))) ((x (x (x x))) (((x (x (x (x x)))) (x (x (x x)))
      )(x (x (x x))))))
	 (-> n))
	(lambda (x) (+ x 1))) 0))))

(fact 5)

"test00.scm, should return #t"
(((((lambda (a)
      (lambda (b)
        (((lambda (a) (lambda (b) ((a b) (lambda (x) (lambda (y) y)))))
	  ((lambda (n)
	     ((n (lambda (x) (lambda (x) (lambda (y) y))))
	      (lambda (x) (lambda (y) x))))
	   (((lambda (a)
	       (lambda (b)
		 ((b (lambda (n)
		       ((lambda (p) (p (lambda (a) (lambda (b) b))))
			((n (lambda (p)
			      (((lambda (a)
				  (lambda (b) (lambda (c) ((c a) b))))
				((lambda (n)
				   (lambda (s)
				     (lambda (z) (s ((n s) z)))))
				 ((lambda (p)
				    (p (lambda (a) (lambda (b) a))))
				  p)))
			       ((lambda (p)
				  (p (lambda (a) (lambda (b) a))))
				p))))
			 (((lambda (a)
			     (lambda (b) (lambda (c) ((c a) b))))
			   (lambda (x) (lambda (y) y)))
			  (lambda (x) (lambda (y) y)))))))
		  a)))
	     a)
	    b)))
	 ((lambda (n)
	    ((n (lambda (x) (lambda (x) (lambda (y) y))))
	     (lambda (x) (lambda (y) x))))
	  (((lambda (a)
	      (lambda (b)
		((b (lambda (n)
		      ((lambda (p) (p (lambda (a) (lambda (b) b))))
		       ((n (lambda (p)
			     (((lambda (a)
				 (lambda (b) (lambda (c) ((c a) b))))
			       ((lambda (n)
				  (lambda (s)
				    (lambda (z) (s ((n s) z)))))
				((lambda (p)
				   (p (lambda (a) (lambda (b) a))))
				 p)))
			      ((lambda (p)
				 (p (lambda (a) (lambda (b) a))))
			       p))))
			(((lambda (a)
			    (lambda (b) (lambda (c) ((c a) b))))
			  (lambda (x) (lambda (y) y)))
			 (lambda (x) (lambda (y) y)))))))
		 a)))
	    b)
	   a)))))
    ((lambda (n)
       ((lambda (p) (p (lambda (a) (lambda (b) b))))
	((n (lambda (p)
	      (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
		((lambda (n) (lambda (s) (lambda (z) (s ((n s) z)))))
		 ((lambda (p) (p (lambda (a) (lambda (b) a)))) p)))
	       (((lambda (a)
		   (lambda (b)
		     ((b (a (lambda (a)
			      (lambda (b)
				((a (lambda (n)
				      (lambda (s)
					(lambda (z) (s ((n s) z))))))
				 b)))))
		      (lambda (x) (lambda (y) y)))))
		 ((lambda (p) (p (lambda (a) (lambda (b) a)))) p))
		((lambda (p) (p (lambda (a) (lambda (b) b)))) p)))))
	 (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
	   (lambda (x) x))
	  (lambda (x) x)))))
     (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
   (((lambda (a)
       (lambda (b)
	 ((b (a (lambda (a)
		  (lambda (b)
		    ((a (lambda (n)
			  (lambda (s) (lambda (z) (s ((n s) z))))))
		     b)))))
	  (lambda (x) (lambda (y) y)))))
     (((lambda (a)
	 (lambda (b)
	   ((b (a (lambda (a)
		    (lambda (b)
		      ((a (lambda (n)
			    (lambda (s) (lambda (z) (s ((n s) z))))))
		       b)))))
	    (lambda (x) (lambda (y) y)))))
       ((lambda (x) (lambda (y) (x (x (x y)))))
	(lambda (x) (lambda (y) (x (x y))))))
      (lambda (x) (lambda (y) (x (x (x y)))))))
    (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
  #t)
 #f)


"test01.scm, should return #t"
((lambda (x) (x x 1000000))
 (lambda (x n)
   (if (zero? n) #t
       (x x (- n 1)))))

"test02.scm (revised), should return #t"
(and
 (boolean? #t)
 (boolean? #f)
 (not (boolean? 1234))
 (not (boolean? 'a))
 (symbol? 'b)
 (procedure? procedure?)
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
 )

"test03.scm, should return #t"
(define with (lambda (s f) (apply f s)))

(define crazy-ack
  (letrec ((ack3
                 (lambda (a b c)
                         (cond
                                  ((and (zero? a) (zero? b)) (+ c 1))
                                         ((and (zero? a) (zero? c)) (ack-x 0 (- b 1) 1))
                                                ((zero? a) (ack-z 0 (- b 1) (ack-y 0 b (- c 1))))
                                                       ((and (zero? b) (zero? c)) (ack-x (- a 1) 1 0))
                                                              ((zero? b) (ack-z (- a 1) 1 (ack-y a 0 (- c 1))))
                                                                     ((zero? c) (ack-x (- a 1) b (ack-y a (- b 1) 1)))
                                                                            (else (ack-z (- a 1) b (ack-y a (- b 1) (ack-x a b (- c 1))))))))
              (ack-x
                    (lambda (a . bcs)
                            (with bcs
                              (lambda (b c)
                                  (ack3 a b c)))))
                 (ack-y
                       (lambda (a b . cs)
                               (with cs
                                 (lambda (c)
                                     (ack3 a b c)))))
                    (ack-z
                          (lambda abcs
                                  (with abcs
                                    (lambda (a b c)
                                        (ack3 a b c))))))
    (lambda ()
      (and (= 7 (ack3 0 2 2))
              (= 61 (ack3 0 3 3))
                 (= 316 (ack3 1 1 5))
                    (= 636 (ack3 2 0 1))
                       ))))

(crazy-ack)

"test005.scm, should return #t"
(((((lambda (x) (x (x x)))
    (lambda (x)
      (lambda (y)
	(x (x y)))))
   (lambda (p)
     (p (lambda (x)
	  (lambda (y)
	    (lambda (z)
	      ((z y) x)))))))
  (lambda (x)
    ((x #t) #f)))
 (lambda (x)
   (lambda (y)
     x)))

"test07.scm, should return #t"
(let ((a 1))
  (let ((b 2) (c 3))
    (let ((d 4) (e 5) (f 6))
      (= 720 (* a b c d e f)))))

"test08.scm, should return ((#t . ()) . ((#t . ()) . ((#t . ()) . ())))"
(let ()
  ((lambda s
     (let ()
       ((lambda s s) s s s)))
   #t))

"test09.scm, should return (3628800 . (fact-1 . (fact-2 . (fact-3 . (fact-1 . (fact-2 . (fact-3 . (fact-1 . (fact-2 . (fact-3 . (fact-1 . (fact-2 . ()))))))))))))"
(define with (lambda (s f) (apply f s)))

(define fact-1
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-1)
	(with (fact-2 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-1 trail)))))))

(define fact-2
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-2)
	(with (fact-3 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-2 trail)))))))

(define fact-3
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-3)
	(with (fact-1 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-3 trail)))))))
(fact-1 10)

"unsorted.scm, should return #f, #t, #t, #f, #t, #t"
(let ((x #f))
  (let ()
    x))

(let ((x #f) (y #t))
  (let ((x #f))
    (let ((x #f) (z #f) (t #f))
      (let ((x #f) (t #f))
	y))))

;;; example 0
((((lambda (x)
     (lambda (y)
       y))
   (lambda (p)
     (p (lambda (x y)
	  (lambda (p)
	    (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))

((((lambda (x)
     (lambda (y)
       (x y)))
   (lambda (p)
     (p (lambda (x y)
	  (lambda (p)
	    (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))

;;; example 1
((((lambda (x)
     (lambda (y)
       (x (x y))))
   (lambda (p)
     (p (lambda (x y)
	  (lambda (p)
	    (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))

;;; example 2
(((((lambda (x) ((x x) (x x)))
    (lambda (x)
      (lambda (y)
	(x (x y)))))
   (lambda (p)
     (p (lambda (x y)
	  (lambda (p)
	    (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))
