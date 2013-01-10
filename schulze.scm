;;;; DSL for the Schulze Method
;;;; c.f. http://de.wikipedia.org/wiki/Schulze-Methode
(load "graph.scm")

;;; external functions
(define choices
  (lambda (ls)
    (cond
     ((list? ls) (set! choices-set ls))
     (else
      (begin
       (error "choices without set"))))))

(define vote
  (lambda (ls)
    (cond
     ((list? ls) (set! votes (cons ls votes)))
     (else
      (begin
       (error "votes without set"))))))

(define result
  (lambda ()
    (let*
        ((norm-votes (map norm-vote votes))
         (dist-matrix (compute-distance-matrix norm-votes))
	 (strongest-path-matrix (compute-strongest-path-matrix choices-set dist-matrix))
	 (better-than-graph (compute-better-than strongest-path-matrix))
	 (minimal-graph (remove-transitive better-than-graph))
	 (ranking (calculate-levels choices-set minimal-graph)))
      (display "Ranking")
      (newline)
      (display ranking)
      (newline)
      (newline)
      (display "Graph")
      (newline)
      (display minimal-graph)
      (newline))))

;;; internal functions
(define choices-set '())

(define votes '())

(define compute-distance-matrix
  (lambda (nv)
    (let*
        ((nv-new (map compute-distance-matrix-help nv))
         (zero-m
          (append (tuples choices-set) (tuples (reverse choices-set))))
         (zero-m-with-0 (map (lambda (t) (cons t 0)) zero-m))
         (all-values (concat (cons zero-m-with-0 nv-new)))
         (together
          (combine-equal
           (lambda (t1 t2)
             (and
              (eqv? (car t1) (car t2))
              (eqv? (cdr t1) (cdr t2))))
           + all-values)))
      together)))

(define compute-distance-matrix-help
  (lambda (nv)
    (let*
        ((ts (tuples nv))
         (pairs (concat (map (lambda (v) (cross-product (car v) (cdr v))) ts)))
         (pairs-with-1 (map (lambda (t) (cons t 1)) pairs)))
      pairs-with-1)))

;; adds missing votes and wraps singletons into lists
(define norm-vote
  (lambda (vote)
    (let*
        ((vote-new (map maybe-sing vote))
         (rest (rm-lists-secure vote-new choices-set)))
      (cond
       ((null? rest) vote-new)
       (else (append vote-new (list rest)))))))

(define maybe-sing
 (lambda (x)
   (cond
    ((list? x) x)
    (else (list x)))))

(define calculate-levels
  (lambda (choi graph)
    (calculate-levels-tail '() choi graph)))

(define calculate-levels-tail
  (lambda (acc choi graph)
    (cond
     ((null? choi) (reverse acc))
     (else
      (let*
	  ((dest-nodes (map cdr graph))
	   (new-choi
	    (break
	     (lambda (c) (elem-of c dest-nodes))
	     choi))
	   (undominated (cdr new-choi))
	   (dominated (car new-choi))
	   (rest-graph (remove-starting-node undominated graph))
	   (new-acc (cons undominated acc)))
	(calculate-levels-tail new-acc dominated rest-graph))))))

(define remove-starting-node
  (lambda (nodes graph)
    (remove-starting-node-tail '() nodes graph)))

(define remove-starting-node-tail
  (lambda (acc nodes graph)
    (cond
     ((null? graph) acc)
     ((elem-of (car (car graph)) nodes)
      (remove-starting-node-tail acc nodes (cdr graph)))
     (else
      (remove-starting-node-tail (cons (car graph) acc) nodes (cdr graph))))))