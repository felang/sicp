(define (fatorial n)
    ((if (= n 1)
        1
        (* n (fatorial (- n 1))))))


(define (fatorial-iter n count product)
    ((if (> count n)
        product
        (fatorial-iter n (+ count 1) (* count product)))))

(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))
(define (fib-i n)
    (define (fib-iter a b count)
        (if (= count 0)
            b
            (fib-iter (+ a b) a (- count 1))))
    (fib-iter 1 0 n))

; 1.11
(define (cal n)
    (if (< n 3)
        n
        (+ (cal (- n 1))
            (* 2 (cal (- n 2)))
            (* 3 (cal ( - n 3))))))

(define (cal-iter a b c count n)
    (if (= count n)
        c
        (cal-iter (+ a (* 2 b) (* 3 c))
                  a
                  b
                  (+ count 1)
                  n)))

(define (cal-i n)
    (cal-iter 2 1 0 0 n))

(define (pascal x y)
    (cond ((or (< x 1) (< y 1)) 0)
          ((and (= x 1) (= y 1)) 1)
          (else (+ (pascal (- x 1) y) (pascal (- x 1) (- y 1))))))