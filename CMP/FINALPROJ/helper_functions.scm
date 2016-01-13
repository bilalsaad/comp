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
;apply exists on its own
;cons car and cdr exists on their own 
(define eq? is_eq)
(define zero? is_zero)
(define null? is_null)
(define pairs? is_pair)
(define list? is_list)


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
          ((not (list? a)) (cons a b))
          (else
          (bin_append (cdr a) (cons (car a) b))))))
(define append
  (lambda lst 
    (let ((helper (lambda(a b) (bin_append (reverse a '()) b))))
      (foldr helper '() lst))))

(define not (lambda(e) (if e #f #t))) 
