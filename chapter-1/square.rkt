#lang racket

(define (square x)
    (* x x))


(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (abs x)
    (if (< x 0)
        (- 0 x)
        x))

(define (sqrt x)
    (define (good-enough? guess x)
        (< (abs (- (square guess) x)) 0.001))

    (define (average x y)
        (/ (+ x y) 2))

    (define (improve guess x)
        (average guess (/ x guess)))

    (define (sqrt-iter guess x)
        (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x)))
    (sqrt-iter 1.0 x))

(define (sqrt-new x)
    (define (good-enough? guess x)
        (< (/ (abs (- (improve guess x) guess)) guess) 0.001))

    (define (average x y)
        (/ (+ x y) 2))

    (define (improve guess x)
        (average guess (/ x guess)))

    (define (sqrt-iter guess x)
        (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x)))
    (sqrt-iter 1.0 x))

(define (cube x)
    (define (good-enough? guess x)
        (< (/ (abs (- (improve guess x) guess)) guess) 0.001))

    (define (improve guess x)
        (/ (+ (/ x (square guess)) (* 2 guess)) 3))

    (define (cube-iter guess x)
        (if (good-enough? guess x)
            guess
            (cube-iter (improve guess x) x)))
    (cube-iter 1.0 x))