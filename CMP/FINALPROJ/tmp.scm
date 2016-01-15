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
(define map1
  (lambda(f . s)
    (if (null? (car s))
        '()
        (let (
              (f_on_cars (apply f (map car s)))
              (rest (map cdr s)))
          (cons f_on_cars (apply map1 (cons f rest)))))))
1
2
3
4
5
