#lang roulette

(require rosette/base/core/bool
         roulette/private/util)

(provide replace-vars
         set-symbolic-vars
         size)

(define/cache (replace-vars formula var-map) 
  (match formula 
    ;;OR
    [(expression (== @||)
                 e1 
                 e2)
     (@|| (replace-vars e1 var-map) (replace-vars e2 var-map))]

    ;;AND
    [(expression (== @&&)
                 e1
                 e2)
     (@&& (replace-vars e1 var-map) (replace-vars e2 var-map))]
  
    ;;NOT
    [(expression (== @!)
                 e1)
     (@! (replace-vars e1 var-map))]
    
    ;;Implies
    [(expression (== @=>)
                 e1
                 e2)
     (@=> (replace-vars e1 var-map) (replace-vars e2 var-map))] 
    
    ;;Iff
    [(expression (== @<=>)
                 e1
                 e2)
     (@<=> (replace-vars e1 var-map) (replace-vars e2 var-map))]
    

    ;;Matches any variable v  
    [v
     (hash-ref var-map v v)]))




(define (set-symbolic-vars flattened-map replacements)
  (map (lambda (x) (cons (car x) (replace-vars (cdr x) replacements))) 
       flattened-map))



(define/cache (size formula) 
  (match formula 
    ;;OR
    [(expression (== @||)
                 e1 
                 e2)
     (+ 1 (size e1) (size e2))]

    ;;AND
    [(expression (== @&&)
                 e1
                 e2)
     (+ 1 (size e1) (size e2))]
  
    ;;NOT
    [(expression (== @!)
                 e1)
     (+ 1 (size e1))]
    
    ;;Implies
    [(expression (== @=>)
                 e1
                 e2)
     (+1 (size e1) (size e2))] 
    
    ;;Iff
    [(expression (== @<=>)
                 e1
                 e2)
     (+1 (size e1) (size e2))]
    

    ;;Matches any variable v  
    [v
     1]))