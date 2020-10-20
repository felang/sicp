(define one-through-four (list 1 2 3 4))

; (car one-through-four)

; (cdr one-through-four)

; (cons 10 one-through-four)

(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))

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

; 2.17
(define (last-pair items)
    (if (null? (cdr items))
        (car items)
        (last-pair (cdr items))))

; 2.18
(define (reverse items)
    (if (null? items)
        items
        (append (reverse (cdr items))
                (list (car items)))))

; 2.19
(define us-coins (list 50 25 10 5 1))
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
; (cc 100 us-coins)

; 2.20
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
;(same-parity 1 2 3 4 5 6 7)

(define nil '())
(define (scale-list items factor)
    (if (null? items)
        nil
        (cons (* (car items) factor)
              (scale-list (cdr items) factor))))

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))

; 2.21
(define (square x) (* x x))
(define (square-list items)
    (map square items))

; 2.22
(define (square-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons (square (car things)) answer))))
    (iter items nil))

; 2.23
(define (for-each proc items)
    (if (not (null? items))
        (proc (car items))
        (for-each proc (cdr items))))



(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
                   (count-leaves (cdr x))))))

;2.24
(list 1 (list 2 (list 3 4)))

;2.25
; (car (cdr (car (cdr (cdr x)))))
; (car (car x))

;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)

;2.27
(define (deep-reverse x)
  (if (pair? x)
      (append (deep-reverse (cdr x))
              (list (deep-reverse (car x))))
      x))
; (define x (list (list 1 2) (list 3 4)))
; (deep-reverse x)

; 2.28
(define (fringe x)
    (define (iter x result)
        (cond ((null? x) result)
              ((not (pair? x)) (append result (list x)))
              (else ((append (iter (car x))
                             (iter (cdr x)))))))
  (iter x nil))

; (define x (list (list 1 2) (list 3 4)))
; (fringe (list x x))

; 2.29
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
        (+ (branch-weight ((left-branch (branch-structure b))))
           (branch-weight ((right-branch (branch-structure b))))))
(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile))))

; (define b1 (make-branch 10 5))
; (define b2 (make-branch 6 4))

; (define m1 (make-mobile b1 b2))
; (define b3 (make-branch 2 m1))
; (define m2 (make-mobile b1 b3))
; (total-weight m1)
; (total-weight m2)
; TODO
; (define (check-balance mobile))

(define (scale-tree factor)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (* tree factor))
          (else (cons (scale-tree (car tree) factor)
                      (scale-tree (cdr tree) factor)))))

; 2.30
(define (square-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree) factor)
                      (square-tree (cdr tree) factor)))))

(define (square-tree tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree sub-tree)
                (square sub-tree)))
         tree))

; 2.31
(define (tree-map proc tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree sub-tree)
                (proc sub-tree)))
         tree))

(define (square-tree tree) (tree-map square tree))

;2.32
(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (x) (cons (car s) x)) rest)))))



(define (sum-odd-squares tree)
    (cond ((null? tree) 0)
          ((not (pair? tree))
           (if (odd? tree) (square tree) 0))
          (else (+ (sum-odd-squares (car tree))
                   (sum-odd-squares (cdr tree))))))

(define (filter f items)
    (if (null? items)
        items
        (if (f (car items))
            (append (list (car items)) (filter (cdr items) f))
            (filter (cdr items) f))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
    (accumulate +
                0
                (map square
                     (filter odds?
                             (enumerate-tree tree)))))

; 2.33
(define (map-a p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append-a seq1 seq2)
    (accumulate cons seq2 seq1))

; (append-a (list 2 3 4) (list 5 6 7))
(define (length-a sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; 2.34
(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
                0
                coefficient-sequence))


; 2.35
(define (count-leaves t)
    (accumulate (lambda (x y) (+ 1 y))
                0
                (map (lambda (x) (x))
                     (enumerate-tree t))))

; 2.36
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

; 2.37
(define (matrix-*-vector m v)
    (map (lambda (col) (dot-product v col)) m))

(define (transpose mat)
    (accumulate-n cons nil mat))

; 2.38
(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            rest
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

; 2.39
(define (reverse sequence)
    (fold-right (lambda (x y) ((append y (list x)))) nil sequence))




(define (enumerate-interval a b)
    (if (> a b)
        nil
        (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cdr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cdr pair) (+ (car pair) (cdr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum?
                 (flatmap
                    (lambda (i)
                        (map (lambda (j) (list i j))
                             (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(define (permutaions s)
    (if (null? s)
        (list nil)
        (flatmap (lambda (x)
                    (map (lambda (p) (cons x p))
                         (permutaions (remove x s))))
                 s)))

(define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
            sequence))

; 2.40
(define (add-j i)
    (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
(define (unique-pairs n)
    (flatmap add-j (enumerate-interval 1 n)))

(define (prime-sum-pairs-u n)
    (map make-pair-sum
         (filter prime-sum?
                 (unique-pair n))))

; 2.41
(define (add-j i)
    (filter (lambda (p) (not (= (car p) (cadr p))))
            (map (lambda (j) (list i j)) (enumerate-interval 1 i))))

(define (add-k p)
    )