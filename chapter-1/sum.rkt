(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))
(define (even? n)
    (= (remainder n 2) 0))

(define (modulus k)
    (cond ((= k 0) 1)
          ((even? k) 2)
          (else 4)))
; 1.29
(define (integral-simson f a b n)
    (define h (/ (- b a) n))
    (define (term k)
        (* (modulus k) (f (+ a (* k h)))))
    (* (/ h 3) (sum term 0 next n)))

(define (cube x) (* x x x))

(define (next x)
    (+ x 1))
; 1.30
(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))

; 1.31
(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b))))

(define (identity x) x)
(define (next x)
    (+ x 1))
(define (factorial n)
    (product identity 1 next n))
(define (even? n)
    (= (remainder n 2) 0))
(define (term-a x)
    (if (even? x)
        (+ x 2)
        (+ x 1)))

(define (term-b x)
    (if (even? x)
        (+ x 1)
        (+ x 2)))

(define (pi-product n)
    (* 4 (/ (product term-a 1 next n) (product term-b 1 next n))))

(define (product term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

; 1.32
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
    (accumulate + 0 term a next b))

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a)
                  (combiner result (term a)))))
    (iter a null-value))

; 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
    (if (> a b)
        null-value
        (if (filter a)
            (combiner (term a)
                      (filtered-accumulate combiner null-value term (next a) next b filter))
            (filtered-accumulate combiner null-value term (next a) next b filter))))