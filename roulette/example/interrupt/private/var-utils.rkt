#lang roulette

(require rosette/base/core/bool
         roulette/private/util)

(provide replace-vars
         set-symbolic-vars)

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
