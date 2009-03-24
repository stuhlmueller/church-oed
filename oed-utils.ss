#!r6rs

(library (oed-utils)
  (export euclidean-distance
          normalized-euclidean-distance
          kl-divergence
          symmetrized-divergence
          loglogistic)
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
  
  
)