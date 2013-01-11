;;;; listen-DSL
;;; external functions
(define tuples
  (lambda (xs)
    (tuples-tail '() xs)))

(define tuples-tail
  (lambda (acc xs)
    (cond
     ((null? xs) acc)
     ((null? (cdr xs)) acc)
     (else
      (let
          ((y (car xs))
           (ys (cdr xs)))
        (let
            ((zs (map
                  (lambda (z)
                    (cons y z))
                  ys)))
          (tuples-tail (append zs acc) ys)))))))

(define rm-lists-secure
  (lambda (xs ys)
    (foldl (flip rm-list-secure) ys xs)))

(define rm-list-secure
  (lambda (xs ys)
    (foldl (flip rm-element-secure) ys xs)))

(define cross-product
  (lambda (xs ys)
    (concat
     (map
      (lambda (x)
        (map
         (lambda (y)
           (cons x y))
         ys))
      xs))))

;; combines cons-cells with the same prefix via f
(define combine-equal
  (lambda (p f xs)
    (combine-equal-tail '() p f xs)))

(define combine-equal-tail
  (lambda (acc p f xs)
    (cond
     ((null? xs) acc)
     (else
      (combine-equal-tail
       (combine-equal-one-element '() p f (car xs) acc)
       p f (cdr xs))))))

(define combine-equal-one-element
  (lambda (acc p f x ys)
    (cond
     ((null? ys) (cons x acc))
     (else
      (let
          ((z (car ys))
           (zs (cdr ys)))
        (cond
         ((p (car x) (car z))
          (let
              ((neu (cons (car x) (f (cdr x) (cdr z)))))
            (append (cons neu acc) zs)))
         (else (combine-equal-one-element (cons z acc) p f x zs))))))))

;;; internal functions
(define concat
  (lambda (xss)
    (concat-help '() xss)))

(define concat-help
  (lambda (acc xss)
    (cond
     ((null? xss) (reverse acc))
     (else (concat-help (append (reverse (car xss)) acc) (cdr xss))))))

(define flip
  (lambda (f)
    (lambda (x y)
      (f y x))))

(define foldl
  (lambda (f s xs)
    (cond
     ((null? xs) s)
     (else (foldl f (f s (car xs)) (cdr xs))))))

(define foldl1
  (lambda f xs
    (foldl f (car xs) (cdr xs))))

(define rm-element-secure
  (lambda (x ys)
    (cond
     ((null? ys)
      (begin
       (error "error while deleting an element:" x)))
     ((eqv? x (car ys)) (cdr ys))
     (else (cons (car ys) (rm-element-secure x (cdr ys)))))))

(define break
  (lambda (p ls)
    (break-tail '() '() p ls)))

(define break-tail
  (lambda (acc-t acc-f p ls)
    (cond
     ((null? ls)
      (cons
       (reverse acc-t)
       (reverse acc-f)))
     (else
      (let
	  ((v (car ls))
	   (h (cdr ls)))
	(cond
	 ((p v)
	  (break-tail (cons v acc-t) acc-f p h))
	 (else
	  (break-tail acc-t (cons v acc-f) p h))))))))

(define elem-of
  (lambda (x ys)
    (cond
     ((null? ys) #f)
     ((eqv? x (car ys)) #t)
     (else (elem-of x (cdr ys))))))
