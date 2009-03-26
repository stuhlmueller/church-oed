#!r6rs

(library (oed-utils)
  (export euclidean-distance
          normalized-euclidean-distance
          kl-divergence
          symmetrized-divergence
          loglogistic
          quicksort)
  (import (rnrs) (church readable-scheme))
    
  (define (sigmoid x a b)
    (/ 1 (+ 1 (exp (* (- a) (- x b))))))

  (define (euclidean-distance l1 l2)
    (exact->inexact (sqrt (sum (apply map (lambda (p1 p2) (expt (- p1 p2) 2.0)) (list l1 l2))))))

  (define (normalized-euclidean-distance l1 l2)
    (display "computing distance between responses ... ")
    (let ((res (/ (euclidean-distance l1 l2) (expt (length l1) 0.5))))
      (display res)
      (display "\n")
      res))

  (define (kl-divergence dista distb)
    (if (not (equal? (length dista) (length distb)))
        (display "LENGTH MISMATCH in divergence")
        (sum (map (lambda (prob1 prob2) (* prob1 (log (/ prob1 prob2)))) dista distb))))

  (define (symmetrized-divergence dista distb)
    (/ (+ (kl-divergence dista distb) (kl-divergence distb dista)) 2))

  (define (loglogistic x a)
    (/ 1 (+ 1 (expt x (- a)))))
  
    (define pivot (lambda (l pred)
      (cond ((null? l) 'done)
    	      ((null? (rest l)) 'done)
            ((pred (first l) (first (rest l))) (pivot (rest l) pred)) 
            ((equal? (first l) (first (rest l))) (pivot (rest l) pred)) 
     	      (else (uniform-draw l)))))

  ; usage: (q-partition 4 '(6 4 2 1 7) () ()) -> returns q-partitions
  (define q-partition (lambda (piv l pred p1 p2)
    (if (null? l) (list p1 p2)
       (if (pred (first l) piv) 
         (q-partition piv (rest l) pred (pair (first l) p1) p2)
  	     (q-partition piv (rest l) pred p1 (pair (first l) p2))))))

  (define (quicksort l pred)
   (display l)
   (display "\n\n")
   (let ((piv (pivot l pred)))
     (if (equal? piv 'done) l
       (let ((parts (q-partition piv l pred '() '())))
         (append (quicksort (first parts) pred) 
                 (quicksort (second parts) pred))))))
  
)