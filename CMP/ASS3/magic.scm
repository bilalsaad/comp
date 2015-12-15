(define scheme-structure-to-ocaml
  (letrec ((run
            (lambda (e)
              (cond
               ((boolean? e)
                (if e "(Bool true)" "(Bool false)"))
               ((char? e) (format "Char ~c" e) )
               ((null? e) "Nil")
               ((integer? e) (format "(Number (Int ~a))" e))
               ((rational? e)
                (format "(Number (Fraction {numerator = ~a; denominator = ~a}))"
                  (numerator e)
                  (denominator e)))
               ((pair? e)
                (let ((res-on-car (run (car e)))
                      (res-on-cdr (run (cdr e))))
                  (format "(Pair(~a, ~a))" res-on-car res-on-cdr)))
               ((symbol? e) (format "(Symbol(\"~a\"))" e))
               ((string? e) (format "(String(~s))" e))
               ((vector? e)
                (format "(Vector [~a])"
                  (apply string-append
                         (cdr
                          (apply append
                                 (map (lambda (e) `("; " ,(run e)))
                                   (vector->list e)))))))
               ((is-void? e) "Void")
               ((procedure? e) (e))
               (else (error 'scheme-structure-to-ocaml
                       (format "Unsupported type: ~a" e)))))))
    run))

(define var
  (lambda (name)
    (let ((name-string (symbol->string name)))
      (lambda () name-string))))

(define is-void?
  (let ((v (if #f #f)))
    (lambda (e)
      (eq? e v))))

(define magic 
  (lambda (sexpr) 
    (display (format "~a~%" (scheme-structure-to-ocaml sexpr)))))
