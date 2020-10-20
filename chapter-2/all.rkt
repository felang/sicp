#lang racket

(define one-through-four (list 1 2 3 4))
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items)
                  (- n 1))))
(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))

(define (last-pair items)
    (if (null? (cdr items))
        (car items)
        (last-pair (cdr items))))

(define (reverse items)
    (if (null? items)
        items
        (append (reverse (cdr items))
                (list (car items)))))


(define us-coins (list 50 25 10 1 5))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
            (+ (cc amount
                   (except-first-denomination coin-values))
               (cc (- amount
                      (first-denomination coin-values))
                   coin-values)))))

(define (first-denomination coins)
    (car coins))

(define (except-first-denomination coins)
    (cdr coins))

(define (no-more? coins)
    (null? coins))

(define (even? n)
    (= (remainder n 2) 0))

(define (odd? n)
    (not (= (remainder n 2) 0)))
(define (filter items f)
    (if (null? items)
        items
        (if (f (car items))
            (append (list (car items)) (filter (cdr items) f))
            (filter (cdr items) f))))

(define (same-parity first . items)
    (if (even? first)
        (append (list first) (filter items even?))
        (append (list first) (filter items odd?))))


(define nil '())
(define (scale-list items factor)
    (if (null? items)
        nil
        (cons (* (car items) factor)
              (scale-list (cdr items) factor))))

(define (square x) (* x x))


(define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
                   (count-leaves (cdr x))))))


(define (deep-reverse x)
  (if (pair? x)
      (append (deep-reverse (cdr x))
              (list (deep-reverse (car x))))
      x))

(define (fringe x)
    (define (iter x result)
        (cond ((null? x) result)
              ((not (pair? x)) (append result (list x)))
              (else (append (iter (car x) result)
                             (iter (cdr x) result)))))
  (iter x nil))



(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))

(define (branch-weight b)
    (if (not (pair? (branch-structure b)))
        (branch-structure b)
        (+ (branch-weight (left-branch (branch-structure b)))
           (branch-weight (right-branch (branch-structure b))))))
(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile))))

(define (tree-map proc tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree sub-tree)
                (proc sub-tree)))
         tree))

(define (square-tree tree) (tree-map square tree))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (map-a p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append-a seq1 seq2)
    (accumulate cons seq2 seq1))
(define (length-a sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
                0
                coefficient-sequence))
(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))
(define (count-leaves-a t)
    (accumulate (lambda (x y) (+ 1 y))
                0
                (map (lambda (x) x)
                     (enumerate-tree t))))

(count-leaves-a (list 1 (list 1 3)))