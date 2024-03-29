(define + v_plus)
(define - v_minus)
(define * v_mult)
(define / v_div)
(define < v_lt) 
(define > v_gt)
(define = v_eq)
;vector exists as its own name
(define make-string make_string) ;this probably doesn't work
(define make-vector make_vector)
(define vector-ref vector_ref)
(define string-ref string_ref)
(define string-length string_len)
(define vector-length vec_len)
;apply exists on its own
;cons car and cdr exists on their own 
(define eq? is_eq)
(define zero? is_zero)
(define null? is_null)
(define pair? is_pair)
(define list? is_list)
(define symbol? is_symbol)
(define string? is_string)
(define vector? is_vector)
(define rational? is_rational)
(define procedure? is_proc)
(define number? is_number)
(define integer? is_int)
(define char? is_char)
(define boolean? is_bool)

(define set-cdr! set_cdr)
(define set-car! set_car)
(define vector-set! vector_set)
(define string-set! string_set)


(define char->integer char_to_integer)
(define integer->char integer_to_char)
(define symbol->string symbol_to_string)
(define string->symbol string_to_symbol)



;scheme implementations
(define list (lambda s s))
(define foldl
  (lambda(f init lst)
    (if (null? lst)
        init
        (foldl f (f (car lst) init) (cdr lst)))))
(define foldr
  (lambda(f init lst)
    (if (null? lst)
      init
      (foldr f (f init (car lst)) (cdr lst)))))
(define reverse 
  (lambda(lst acc)
    (cond((null? lst) acc)
          ((not(list? lst)) lst)
      (else (reverse (cdr lst) (cons (car lst) acc))))))
(define bin_append
  (lambda(a b)
    (cond ((null? a) b) 
          ((not (pair? a)) (cons a b))
          (else
          (bin_append (cdr a) (cons (car a) b))))))
(define append
  (lambda lst 
    (let ((helper (lambda(a b) (bin_append (reverse a '()) b))))
      (foldr helper '() lst))))

(define not (lambda(e) (if e #f #t))) 

(define map_simple
  (lambda(f ls)
    (let ((ls (reverse ls '())))
     (foldl (lambda(a b) (cons (f a) b)) '() ls))))

(define map
  (lambda(f . s)
    (if (null? (car s))
        '()
        (let ((f_on_cars (apply f (map_simple car s)))
              (rest (map_simple cdr s)))
          (cons f_on_cars (apply map (cons f rest)))))))


