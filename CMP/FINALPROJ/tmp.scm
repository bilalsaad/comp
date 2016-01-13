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

