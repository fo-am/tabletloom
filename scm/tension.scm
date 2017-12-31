#lang racket

(define (weft->dir weft)
  (cond
    ((or (eq? weft 'll) (eq? weft 'sl)) 'left)
    ((or (eq? weft 'rr) (eq? weft 'sr)) 'right)
    (else 'straight)))

(define (weft->structure weft last)
  (let ((hist (weft->dir last)))
    (cond
      ((eq? hist 'left)
       (cond
         ((eq? weft 'll) 'll)
         ((eq? weft 'rr) 'ls)))
      ((eq? hist 'right)          
       (cond
         ((eq? weft 'rr) 'rr)
         ((eq? weft 'll) 'rs)))
      (else          
       (cond
         ((eq? weft 'll) 'sl)
         ((eq? weft 'rr) 'sr))))))

(define (weft-tension weft last)
  (map
   (lambda (weft last)
     (weft->structure weft last))
   weft last))

(define (weave-tension weave last)
  (cond
    ((null? weave) '())
    (else
     (let ((weft (weft-tension (car weave) last)))
       (cons weft (weave-tension (cdr weave) weft))))))

(weave-tension '((ll rr ll rr)
                 (rr ll rr ll)
                 (ll rr ll rr)
                 (rr rr rr rr)
                 (ll rr ll rr)
                 (rr ll rr ll)
                 (ll rr ll rr)) '(ll ll ll ll))

;(define (weave-tension structure)
;  (foldl
;   (lambda (wh r)
;     (cons
;      (weft-tension (car wh) (cadr wh))
;      r))
;   (list '() (build-list (length (car structure)) (lambda (_) 'left)))
   