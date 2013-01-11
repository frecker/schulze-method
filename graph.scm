;;;; DSL for graphs
(load "mylists.scm")

;;; external functions
;; graph is calculatet with the algorithm of Floyd and Warshall
;; choices are the remaining candidates for inner nodes
(define compute-strongest-path-matrix
  (lambda (choices graph)
    (cond
     ((null? choices) graph)
     (else
      (compute-strongest-path-matrix
       (cdr choices)
       (insert-candidate (car choices) graph))))))

(define compute-better-than
  (lambda (graph)
    (compute-better-than-tail '() graph)))

(define remove-transitive
  (lambda (graph)
    (remove-transitive-tail '() graph graph)))

;;; internal functions
(define insert-candidate
  (lambda (inner-node graph)
    (insert-candidate-tail '() inner-node graph graph)))

(define insert-candidate-tail
  (lambda (new-graph inner-node rest-graph graph)
    (cond
     ((null? rest-graph) new-graph)
     (else
      (let*
	  ((actual-node (car rest-graph))
	   (rest-rest-graph (cdr rest-graph))
	   (start-node (car (car actual-node)))
	   (dest-node (cdr (car actual-node)))
	   (actual-value (cdr actual-node))
	   (new-value
	    (cond
	     ((or (eqv? inner-node start-node) (eqv? inner-node dest-node))
	      actual-value)
	     (else
	      (let*
		  ((v1 (cdr (search-node start-node inner-node graph)))
		   (v2 (cdr (search-node inner-node dest-node graph))))
		(max actual-value (min v1 v2)))))))
	(insert-candidate-tail
	 (cons
	  (cons
	   (cons start-node dest-node) new-value)
	  new-graph)
	 inner-node
	 rest-rest-graph
	 graph))))))

(define search-node
  (lambda (x y graph)
    (let
	((first (car graph))
	 (rest (cdr graph)))
      (cond
       ((and (eqv? x (car (car first))) (eqv? y (cdr (car first)))) first)
       (else (search-node x y rest))))))

(define compute-better-than-tail
  (lambda (acc graph)
    (cond
     ((null? graph) acc)
     (else (compute-better-than-tail-tail acc '() (car graph) (cdr graph))))))

(define compute-better-than-tail-tail
  (lambda (acc acc2 candidate graph)
    (let
	((first (car graph))
	 (rest (cdr graph)))
      (cond
       ((and
	 (eqv? (starting-node first) (destination-node candidate))
	 (eqv? (destination-node first) (starting-node candidate)))
	(let
	    ((value-candidate (value candidate))
	     (value-first (value first)))
	  (cond
	   ((> value-first value-candidate)
	    (compute-better-than-tail
	     (cons
	      (cons (starting-node first) (destination-node first))
	      acc)
	     (append acc2 rest)))
	   ((< value-first value-candidate)
	    (compute-better-than-tail
	     (cons
	      (cons (starting-node candidate) (destination-node candidate))
	      acc)
	     (append acc2 rest)))
	   (else
	    (compute-better-than-tail
	     acc
	     (append acc2 rest))))))
       (else
	(compute-better-than-tail-tail
	 acc
	 (cons first acc2)
	 candidate
	 rest))))))

(define starting-node
  (lambda (node)
    (car (car node))))

(define destination-node
  (lambda (node)
    (cdr (car node))))

(define value
  (lambda (node)
    (cdr node)))

(define remove-transitive-tail
  (lambda (acc rest-graph full-graph)
    (cond
     ((null? rest-graph)
      acc)
     ((is-dominated (car rest-graph) full-graph full-graph)
      (remove-transitive-tail acc (cdr rest-graph) full-graph))
     (else
      (remove-transitive-tail
       (cons (car rest-graph) acc) (cdr rest-graph) full-graph)))))

; tests, whether the node is dominated by the transitiv graph
(define is-dominated
  (lambda (node graph full-graph)
    (cond
     ((null? graph) #f)
     ((not (eqv? (car node) (car (car graph))))
      (is-dominated node (cdr graph) full-graph))
     ((is-dominated-help (cdr (car graph)) (cdr node) full-graph) #t)
     (else (is-dominated node (cdr graph) full-graph)))))

(define is-dominated-help
  (lambda (first rest graph)
    (cond
     ((null? graph) #f)
     ((and
       (eqv? first (car (car graph)))
       (eqv? rest (cdr (car graph)))) #t)
     (else (is-dominated-help first rest (cdr graph))))))
