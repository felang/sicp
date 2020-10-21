(define (memq item x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))

; 2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

; 2.54
(define (equal? list1 list2)
    (cond ((and (not (pair? list1)) (not (pair? list2)))
           (eq? list1 list2))
          ((and (pair? list1) (pair? list2))
           (and (equal? (car list1) (car list2)) (equal? (cdr list1) (cdr list2))))
          (else false)))


; 2.3.2
; (define (deriv exp var)
;     (cond ((number? exp) 0)
;           ((variable? exp)
;            (if (same-variable? exp var) 1 0))
;           ((sum? exp)
;            (make-sum (deriv (addend exp) var)
;                      (deriv (augend exp) var)))
;           ((product? exp)
;            (make-sum
;             (make-product (multiplier exp)
;                           (deriv (multiplicand exp) var))
;             (make-product (deriv (multiplier exp) var)
;                           (multiplicand exp))))
;           (else
;             (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

; (define (make-sum a1 a2) (list '+ a1 a2))
; (define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
    (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (=number? exp num)
    (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

; 2.56
(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list '** base exponent))))

(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
          ((exponentiation? exp)
           (make-product (exponent exp)
                         (make-product (make-exponentiation (base exp) (make-sum (exponent exp) -1))
                                       (deriv (base exp) var))))
          (else
            (error "unknown expression type -- DERIV" exp))))

;(deriv '(** x 3) 'x)

; 2.57
(define (binary-expression? e)
    (null? (cdddr e)))

(define (second-term e) (caddr e))
(define (all-but-first-term e) (cddr e))

(define (augend s)
    (if (binary-expression? s)
        (second-term s)
        (cons '+ (all-but-first-term s))))

(define (multiplicand p)
    (if (binary-expression? p)
        (second-term p)
        (cons '* (all-but-first-term p))))

; 2.58
; a)
(define (sum? x)
    (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
    (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (=number? exp num)
    (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list a1 '+ a2))))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2))))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
          (else
            (error "unknown expression type -- DERIV" exp))))

; b)

; 2.3.3
(define (element-of-set? x set)
    (cond ((null? set) false)
          (equal? x (car set)) true)
          (else (element-of-set? x (cdr set))))

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
           (cons (car set1)
                 (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

; 2.59
(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((element-of-set? (car set1) set2)
           (union-set (cdr set1) set2))
          (else
           (cons (car set1)
                 (union-set (cdr set1) set2)))))

; 2.60
(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
    (define (iter set1 result)
        (if (or (null? set1) (null? set2))
            result
            (if (and (element-of-set? (car set1) set2)
                     (element-of-set? (car set1) result))
                (iter (cdr set1)
                      (cons (car set1) result))
                (iter (cdr set1) result))))
    (iter set '()))


(define (element-of-set? x set)
    (cond ((null? set) false)
          ((= x (car set)) true)
          ((< x (car set) false)
          (else (element-of-set? x (cdr set))))))

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((= x1 x2)
                    (cons x1
                         (intersection-set (cdr set1)
                                           (cdr set2))))
                    ((< x1 x2)
                     (intersection-set (cdr set1) set2))
                    ((< x2 x1)
                     (intersection-set set1 (cdr set2)))))))

; 2.61
(define (adjoin-set x set)
    (if (null? set)
        (list x)
        (cond ((= x (car set)) set)
              ((> x (car set))
               (cons (car set)
                     (adjoin-set x (cdr set)))
              ((< x (car set))
               (cons x set))))))

; 2.62
(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
           (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1
                         (union-set (cdr set1)
                                    (cdr set2))))
                  ((< x1 x2)
                   (cons x1
                         (union-set (cdr set1) set2)))
                  ((< x2 x1)
                   (cons x2
                         (union-set set1 (cdr set2)))))))))


; tree-collection p106
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((= x (entry set)) set)
          ((< x (entry set))
           (element-of-set? x (left-branch set)))
          ((> x (entry set))
           (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-branch set)))
          ((> x (entry set))
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set))))))

; 2.63
(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
    (copy-to-list tree '()))

; 2.64

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
                (let ((left-tree (car left-result))
                      (non-left-elts (cdr left-result))
                      (right-size (- n (+ left-size 1))))
                    (let ((this-entry (car non-left-elts))
                          (right-result (partial-tree (cdr non-left-elts)
                                                    right-size)))
                        (let ((right-tree (car right-result))
                              (remaining-elts (cdr right-result)))
                            (cons (make-tree this-entry left-tree right-tree)
                                remaining-elts))))))))
;(list->tree (list 1 3 5)) -> '(3 (1 () ()) (5 () ()))
;(list->tree (list 1 3 5 7 9 11)) -> '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

; 2.65

; 2.66


(define (make-leaf symbol weight)
    (list 'leaf symbol weight))

(define (leaf? object)
    (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch
                  (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                          (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))
(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- CHOOSE_BRANCH" bit))))

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)
                                   (cadr pair))
                        (make-leaf-set (cdr pairs))))))

; 2.67
(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                        (make-leaf 'B 2)
                        (make-code-tree (make-leaf 'D 1)
                                        (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; 2.68

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
    (if (leaf? tree)
        (if (eq? symbol (symbol-leaf tree))
            '()
            (error "NO SUCH SYMBOL"))
        (if (memq symbol (symbols (left-branch tree)))
            (cons 0 (encode-symbol symbol (left-branch tree)))
            (cons 1 (encode-symbol symbol (right-branch tree))))))

; 2.69
(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
    (define (merge first rest)
        (if (null? rest)
            first
            (merge (make-code-tree (car rest) first)
                   (cdr rest))))
    (merge (car leafs) (cdr leafs)))
; (define tree (successive-merge '((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))))
; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; (decode sample-message tree)
; (decode sample-message sample-tree)

; 2.70
(define pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
; (define punk-tree (generate-huffman-tree pairs))
; (define message1 '(GET A JOB))
; (define message2 '(SHA NA NA NA NA NA NA NA NA))
; (define message3 '(GET A JOB))
; (define message4 '(GET A JOB))
; (encode message1 punk-tree)
; (encode message2 punk-tree)
; (encode message3 punk-tree)
; (encode message4 punk-tree)

; 2.71

; 2.72