(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (lower-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(define (print-interval name i)
    (newline)
    (display name)
    (display "[")
    (display (lower-bound i))
    (display ",")
    (display (upper-bound i))
    (display "]"))

;2.7
(define (make-interval a b) (cons a b))
(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))
; (define a (make-interval 1 10))
; (print-interval a)

;2.8
(define (sub-interval x y)
    (make-interval (- (lower-bound x) (lower-bound y))
                   (- (upper-bound x) (lower-bound y))))

;2.9
; add [1, 3] [2, 6] -> add 2 4 = 6 
; mul [1, 3] [2, 6] -> mul 2 4 = 8  in fact (2 + 18) / 2 = 10

;2.10
(define (div-interval x y)
    (if (< (* (lower-bound y) (upper-bound y)) 0)
        (error "intervals span 0" y))
        (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))
; (define i (make-interval 2 7))
; (define j (make-interval 8 3))
; (define span-0 (make-interval -1 1))

; (print-interval "i/j" (div-interval i j))
; (print-interval "i/span-0" (div-interval i span-0))


;2.11
(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

;2.12
(define (make-center-percent c p)
    (make-interval (- c (* c p))
                   (+ c (* c p))))
(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
    (let ((width (/ (- (upper-bound i) (lower-bound i)) 2)))
          (center (/ (+ (lower-bound i) (upper-bound i)) 2)))
        (/ width center))