;append (variadic)
(append '(1 2 3) '(4 5 6))               ; (1 . (2 . (3 . (4 . (5 . (6 . ()))))))

;apply
(apply (lambda (x) x) '("application"))     ; "application"
(apply cons 1 '(2))                         ; (1 . 2)
(apply cons cons '(cons))                   ; (#<procedure cons> . cons)
(define fact-tail
    (lambda (n acc)cd
      (if (= 1 n)
          acc
          (fact-tail (- n 1) (* n acc)))))
(apply fact-tail 5 '(1))                    ; 120

(< 1 2)                                     ; #t
(< 3 2)                                     ; #f
(< -3 -2 -1 0 1/2 2/3 3/4)                  ; #t

;= (variadic)
(= 1 2)                                     ; #f
(= 3 3 3 3 -1)                              ; #f
(= 5 5 5 5 5 5 5 5 5 5 5)                   ; #t

;> (variadic)
 (> 1 2)                                     ; #f
 (> 3 2)                                     ; #t
 (> -3 -2 -1 0 1/2 2/3 3/4)                  ; #f

;+ (variadic)
 (+)                                         ; 0
 (+ 1 -1)                                    ; 0
 (+ 3 -1/2)                                  ; 5/2
 (+ 1/3 1/2)                                 ; 5/6

;/ (variadic)
 (/ 1 3)                                     ; 1/3
 (/ 4 2)                                     ; 2
 (/ 6 -2)                                    ; -3
 (/ 1)                                       ; 1

;* (variadic)
 (*)                                         ; 1
 (* 5 2/7)                                   ; 10/7
;boolean?
 (boolean? #t)                               ; #t
 (boolean? #f)                               ; #t
 (boolean? (lambda (x y z) x y z y x))       ; #f
 (boolean? (if #f #f #f))                    ; #t
 (boolean? (if #f #t))                       ; #f
 (boolean? (and 1 2))                        ; #t
 (boolean? (and #f #f))                      ; #t
 (boolean? (and))                            ; #t
 (boolean? 5)                                ; #f

(display "mooo")
;car
 (car '(1 2 3))                              ; 1

;cdr
 (cdr '(1 2 3))                              ; (2 . (3 . ()))

;char->integer
 (char->integer #\A)                         ; 65
 (char->integer #\page)                      ; 12

(display "mooo")
;char?
 (char? #\5)                                 ; #t
 (char? #\newline)                           ; #t
 (char? -1/19)                               ; #f

(display "moooszz")
;cons
 (cons 2648 #t)                             ; (2748 . #t)
 (cons (lambda () #t) (if #f #t))            ; (#<procedure at...> . #<void>)

;denominator
 (denominator -2)                            ; 1
 (denominator 9/7)                           ; 7

;eq?
 (eq? "ab" "ab")                             ; #t (because of constants table)
 (eq? 'ab 'ab)                               ; #t
 (eq? #f #f)                                 ; #t
 (eq? -1 -1)                                 ; #t

 (integer? 3/14)                             ; #f
 (integer? cons)                             ; #f

;integer->char
 (integer->char 10)                          ; #\newline
 (integer->char 255)                         ; #\aa

;list (variadic)
 (list list)                                 ; (#<procedure at ...> . ())
 (list -1 2 -3 4)                            ; (-1 . (2 . (-3 . (4 . ()))))
 (list #t #t #f #t)                          ; (#t . (#t . (#f . (#t . ()))))
 (list)                                      ; ()
(display "asasas\n")
;make-string
 (make-string 3)                             ; "\x0;\x0;\x0;"
 (make-string 5 #\r)                         ; "rrrrr"

;make-vector
 (make-vector 7)                             ; #(0 0 0 0 0 0 0)
 (make-vector 2 "scheme")                    ; #("scheme" "scheme")

;map
 (map (lambda (s) "batman") '("why" "so" "serious?"))  
 (map (lambda (x) x) '(-1 -2 -3/5))          ; (-1 . (-2 . (-3/5 . ())))

;not
 (not #f)                                    ; #t
 (not #t)                                    ; #f
 (not 5)                                     ; #f
 (not apply)                                 ; #f

;null?
 (null? '())                                 ; #t
 (null? '(9 #\F))                            ; #f
 (null? null?)                               ; #f

;number?
 (number? -1)                                ; #t
 (number? -3/14)                             ; #t
 (number? #t)                                ; #f
 (number? "hello, world!")                    ; #f

;numerator
 (numerator 4/2)                             ; 2
 (numerator -7/17)                           ; -7
 (numerator 3)                               ; 3

;pair?
 (pair? '(1 2))                              ; #t
 (pair? '())                                 ; #f
 (pair? 5)                                   ; #f

;procedure?
 (procedure? procedure?)                     ; #t
 (procedure? (lambda s (car s)))             ; #t
 (procedure? #t)                             ; #f

;rational?
 (rational? 5/2)                             ; #t
 (rational? -99)                             ; #t
 (rational? 0xffff)                          ; #t
 (rational? "rational?")                     ; #f

;remainder
 (remainder 12 -5)                           ; 2
 (remainder 70 7)                            ; 0
 (remainder 0xffff 0xfffe)                   ; 1

 (string->symbol 'sym)                       ; exception - not a string
;set-car!
 (set-car! '(a b c) '(x y z))                ; #<void>, print nothing
 (set-car! '(1 2 3) (cdr '(1 2 3)))          ; #<void>, print nothing

;set-cdr!
 (set-cdr! '(a b c) '(x y z))                ; #<void>, print nothing
 (set-cdr! '(1 2 3) (cdr '(1 2 3)))          ; #<void>, print nothing

;string-length
 (string-length "")                          ; 0
 (string-length "a")                         ; 1

;string-ref
 (string-ref "string" 1)                     ; #\t

;string-set!
 (string-set! "string" 1 #\p)                ; #<void>

;string->symbol
 (string->symbol "sym")                      ; sym

;string?
 (string? "string")                          ; #t
 (string? 'string)                           ; #f
 (string? 2)                                 ; #f

;symbol?
 (symbol? 'symbol)                           ; #t
 (symbol? "symbol")                          ; #f
 (symbol? 2)                                 ; #f

;symbol->string
 (symbol->string 'sym)                       ; "sym"
;vector
 (vector vector vector)                      ; #(#<procedure...> #<procedure...>)
 (vector 'a 1 1/1 "uno" 'einz)               ; #(a 1 1 "uno" einz)
 (vector)                                    ; #()

(display "mooddo")
;vector-length
 (vector-length '#())                        ; 0
 (vector-length '#(vector? vector-ref))      ; 2
;vector-ref
 (vector-ref '#(1 2 3) 0)                    ; 1
;vector-set!
 (vector-set! '#(4 5 6) 1 7)                 ; #<void>
;vector?
 (vector? '#(some-vector...))                ; #t
 (vector? "no")                              ; #f
 (vector? '#(1 b 3 d 5 f))                   ; #t

(zero? 0)                                   ; #t
(zero? #f)                                  ; #f
(zero? (- 5 (+ 1 2 2)))                     ; #t

(display "mooofin")
