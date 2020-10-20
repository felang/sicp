(define (expt b n)
    (if (= n 1)
        b
        (* b (expt b (- n 1)))))

(define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b (- counter 1) (* b product))))

(define (expt-i b n)
        (expt-iter b n 1))

(define (even? n)
    (= (remainder n 2) 0))

(define (square x)
    (* x x))

(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ 2 n))))
          (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-i b n)
    (define (fast-expt-iter b n a)
        (cond ((= n 0) a)
              ((even? n) (fast-expt-iter (square b) (/ n 2) a))
              (else (fast-expt-iter b (- n 1) ( * a b)))))
    (fast-expt-iter b n 1))

(define (doule x)
    (* x 2))

(define (halve x)
    (/ x 2))

(define (fast-multi a b)
    (cond ((= b 0) 0)
          ((even? b) (doule (fast-multi a (halve b)))
          (else (+ a (fast-multi a (- b 1))))))

(define (fast-multi-iter a b res)
    (cond ((= b 0) res)
          ((even? b) (fast-multi-iter (doule a) (halve b) res))
          (else (fast-multi-iter a (- b 1) (+ res a)))))

