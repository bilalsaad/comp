(define hd car)
(define tl (lambda(x) ((cdr x))))

(define ones (cons 1 (lambda() ones)))

(hd ones)
(hd (tl (tl (tl (tl (tl (tl (tl (tl ones)))))))))

(define integers
  (lambda (n) 
    (cons n
          (lambda() (integers (+ n 1))))))

(define ints (integers 0))


(define lzl-map
  (lambda (f s)
    (if (null? s)
      s
    (cons (f (hd s)) (lambda() (lzl-map f (tl s))))))

(define (take n lzl)
  (if (zero? n)
      '()
      (cons (hd lzl) (take (- n 1) (tl lzl)))))

(define inter_leave
  (lambda (a b)
    (cond ((null? a) b)
          ((null? b) a)
          (else
            (cons (hd a) 
                  (lambda() (inter_leave b (tl a))))))))
(define vlzl-map
  (lambda (f . s)
    (let ((f_on_cars (apply f (map hd s)))
          (rest (lzl-map tl s)))
      (cons f_on_cars (lambda() (apply lzl-map (cons f rest)))))))

(define batmans (lzl-
