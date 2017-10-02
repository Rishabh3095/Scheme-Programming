;checks is the list is empty
(define (empty? s)
  (cond
    ((null? s) #t)
    (else #f)))

;(set s):  return a list representation of a set where each element appears once.  The order is not relevant.
(define (set s)
  (cond
    ((empty? s) '())
    ((in? (car s) (cdr s)) (set (cdr s)))
    (in? (car s) (set (cdr s)) (append (set (cdr s)) (list (car s))))))

;checks if the element is in the list
(define (in? e s)
  (cond
    ((empty? s) #f)
    ((eqv? e (car s)) #t)
    (else(in? e (cdr s)))))

;add element to the list
(define (add e s)
  (if (in? e s) s (append s (list e))))

;removes element from the list
(define (discard e s)
  (cond
    ((not (in? e s)) s)
    ((empty? s) s)
    ((eqv? e (car s)) discard e (cdr s))
    (else (in? (car s) (discard e (cdr s))) (discard e (cdr s)) (append (discard e (cdr s)) (list (car s))))))

;combines the lists
(define (union s1 s2)
  (cond
    ((empty? s1) s2)
    ((empty? s2) s1)
    ((in? (car s2) s1) (union s1 (cdr s2)))
    (else (union (add (car s2) s1) (cdr s2)))))

;(intersection s1 s2):  return a new set that contains all the elements that appear in both sets.
(define (intersection s1 s2)
  (cond
    ((empty? s1) '())
    ((empty? s2) '())
    ((in? (car s1) s2) (add (car s1) (intersection (cdr s1) (discard (car s1) s2))))
    (else (intersection (discard (car s1) s1) s2))))

;removes duplicates
(define (difference s1 s2)
  (cond
    ((empty? s1) '())
    ((empty? s2) s1)
    ((in? (car s1) s2) (difference (discard (car s1) s1) s2))
    (
     (cons (car s1) (difference (cdr s1) s2)))))

;(symmetric-difference s1 s2): return a new set with elements in either s1 or s2 but not both.
(define (symmetric-difference s1 s2)
  (union (difference s1 s2) (difference s2 s1)))

;(subset? s1 s2):  return #t if every element of s1 is in s2, #f otherwise.
(define (subset? s1 s2 )
  (if
   (empty? (difference s1 s2)) #t #f))
 
;(superset? s1 s2): return #t if every element of s2 is in s1, #f otherwise.
(define (superset? s1 s2 )
  (if
   (subset? s2 s1) #t #f))

;(disjoint? s1 s2): return #t if s1 and s2 have no elements in common, #f otherwise.
(define (disjoint? s1 s2)
  (if
   (empty? (intersection s1 s2)) #t #f))

;(sameset? s1 s2): return #t if s1 and s2 have the same elements, #f otherwise.  The order is not relevant.
(define (sameset? s1 s2)
  (and
   (subset? s1 s2) (subset? s2 s1)))

; some tests
(define A (set '(1 2 7 9 7 1)))
(define B (set '(2 0 8 0 7 12)))
(define C (set '(9 7)))

(define colors (set '("yellow" "red" "green" "blue" "orange" "purple" "pink")))
(define rgb (set '("red" "green" "blue")))

(define hi (set '(#\h #\i)))

(empty? A) ; #f
(empty? rgb) ;#f
(empty? (set'())) ;#t

(in? 0 A) ; #f
(in? "red" A); #f
(in? 2 A) ; #t

(in? "green" rgb) ; #t
(in? "purple" rgb) ; #f
(in? "i" hi) ;#f
(in? #\i hi) ;#t

(add 9 A) ; (2 9 7 1)
(add 5 A) ; (5 2 9 7 1)

(discard 1 A) ; (2 9 7)
(discard 5 A) ; (2 9 7 1)
(union A B) ; (9 1 2 8 0 7 12)
(union A rgb) ; (2 9 7 1 "red" "green" "blue")

(intersection A rgb) ; ()
(intersection A B) ; (2 7)
(intersection rgb colors) ; ("red" "green" "blue")

(difference A B) ; (9 1)
(difference rgb colors) ; ()
(difference colors rgb) ; ("yellow" "orange" "purple" "pink")

(symmetric-difference A B) ; (9 1 8 0 12)
(symmetric-difference A C) ; (2 1)
(symmetric-difference colors rgb) ; ("yellow" "orange" "purple" "pink")

(subset? A B) ;#f
(subset? C A) ; #t

(subset? colors rgb) ;#f
(subset? rgb colors)  ; #t

(superset? A B) ;#f
(superset?  A C) ; #t

(superset? colors rgb) ;#t
(superset? rgb colors)  ; #f

(disjoint? B C) ;#f
(disjoint? colors A) ;#t

(sameset? (set '(9 1 2 7)) A); #t
(sameset? B A) ; #f