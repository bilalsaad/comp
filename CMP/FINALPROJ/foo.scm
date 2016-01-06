#;(define make_pair 
  (lambda(a b)
    (lambda(sel) (sel a b))))

#;(define p1 (make_pair 1 2))

#;(define kar
  (lambda(p)
    (p (lambda(a b) a))))
#;(define kdr
  (lambda (p)
    (p (lambda (a b) b))))

(if #f 1 3)
