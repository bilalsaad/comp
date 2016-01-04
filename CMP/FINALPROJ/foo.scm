((lambda(f)
   ((lambda(x) (x x))
    (lambda(x) (f (lambda(z) ((x x) z)))))) 
 (lambda(x) "moshe"))



;((lambda(x) (x x)) (lambda(x) '()))
