(load "tablet2.scm")

(define (load-threads name)
  (with-state
   (translate (vector -1000 0 0))
   (list (load-obj (string-append "../models/" name "-a.obj"))
	 (load-obj (string-append "../models/" name "-b.obj"))
	 (load-obj (string-append "../models/" name "-c.obj"))
	 (load-obj (string-append "../models/" name "-d.obj")))))

(define weft-obj (with-state (translate (vector -1000 0 0)) 
			     (load-obj "../models/weft-thick.obj")))

(define objs 
  (list 
   (list "ll" (load-threads "ll"))
   (list "rr" (load-threads "rr"))
   (list "ls" (load-threads "ls"))
   (list "rs" (load-threads "rs"))
   (list "sl" (load-threads "sl"))
   (list "sr" (load-threads "sr"))))

(define (reverse-n p) 
  (with-primitive 
   p
   (pdata-map! 
    (lambda (n) 
      (vector (- (vx n)) (- (vy n)) (- (vz n))))
    "n")))

(define (reverse-normals n)
  (reverse-n (list-ref (cadr (list-ref objs n)) 0))
  (reverse-n (list-ref (cadr (list-ref objs n)) 1))
  (reverse-n (list-ref (cadr (list-ref objs n)) 2))
  (reverse-n (list-ref (cadr (list-ref objs n)) 3)))

(define (reverse-n2 p) 
  (with-primitive 
   p
   (pdata-map! 
    (lambda (n) 
      (vector (+ (vx n)) (- (vy n)) (- (vz n))))
    "n")))

(define (reverse-normals2 n)
  (reverse-n2 (list-ref (cadr (list-ref objs n)) 0))
  (reverse-n2 (list-ref (cadr (list-ref objs n)) 1))
  (reverse-n2 (list-ref (cadr (list-ref objs n)) 2))
  (reverse-n2 (list-ref (cadr (list-ref objs n)) 3)))

(reverse-normals2 2)
(reverse-normals2 3)
(reverse-normals 4)

(define (thread->colour thread)
  (cond 
   ((eq? thread 'x) (colour (vector 0.2 0.2 0.2)))
   ((eq? thread 'y) (colour (vector 1 1 1)))
   ((eq? thread 'z) (colour (vector 1 0 1)))
   ((eq? thread 'a) (colour (vector 1 1 0)))))

(define (build-coloured element-name threads)
  (with-state
   (colour (vector 0 0 1))
   (let ((obj (cadr (assoc element-name objs))))
     (thread->colour (list-ref threads 0))
     (build-instance (list-ref obj 0))
     (thread->colour (list-ref threads 1))
     (build-instance (list-ref obj 1))
     (thread->colour (list-ref threads 2))
     (build-instance (list-ref obj 2))
     (thread->colour (list-ref threads 3))
     (build-instance (list-ref obj 3)))))

(define (build-threads element threads)
  (cond
   ((eq? element 'll) (build-coloured "ll" threads))
   ((eq? element 'rr) (build-coloured "rr" threads))
   ((eq? element 'ls) (build-coloured "ls" threads))
   ((eq? element 'rs) (build-coloured "rs" threads))
   ((eq? element 'sl) (build-coloured "sl" threads))
   ((eq? element 'sr) (build-coloured "sr" threads))
   (else (colour (vector 0 0 1)) (build-cube))))

(define (weave-render desc)
  (let ((structure (car desc))
	(threads (cadr desc)))
    (with-state
     (for-each
      (lambda (weft threads)
	(translate (vector 2 0 0))
	(with-state
	 (for-each 
	  (lambda (element threads)
	    (translate (vector 0 0 2))
	    (build-instance weft-obj)
	    (build-threads element threads))
	  weft threads)))
      structure threads))))

(define root (build-locator))

(with-state
 (parent root)
 (translate (vector -4 0 -4))
(scale (vector 0.5 0.5 0.5))
 (weave-render
  (dbg (weave
 	(list 'ccw 'ccw 'ccw 'cw 'cw 'cw 'cw 'ccw 'ccw 
 	      'ccw 'ccw 'cw 'cw 'cw 'cw 'ccw 'ccw 'ccw 'ccw )
 	'((y x x y) 
 	  (x x y y) 
 	  (x y y x) 
 	  (y y x x) 
 	  (y x x y) 
	  
 	  (x y y x) 
 	  (y y x x) 
 	  (y x x y) 	
 	  (x x y y) 	
 	  (x y y x))
	
 	'(right right left left left right right right left left))))

 ;; (weave-render
 ;;  (dbg (weave
 ;; 	(list 'ccw 'ccw 'ccw 'ccw 'cw 'cw 'cw 'cw 'ccw 'ccw 'cw 'ccw 'cw 'ccw)
 ;; 	'((x y z a) 
 ;; 	  (x y z a)
 ;; 	  ) 
	
 ;; 	'(left
 ;; 	  right))))

)

(define camera (build-locator))
(lock-camera camera)
(with-primitive 
 camera
 (rotate (vector -45 0 0))
 (translate (vector 0 0 20)))

(every-frame (with-primitive root (rotate (vector 0 0.3 0))))




