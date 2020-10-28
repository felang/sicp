; stream
(define the-empty-stream '())
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))
(define (stream-car stream) (car stream))
(define (stream-null? stream) (null? stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (memo-proc proc)
    (let ((already-run? false) (result false))
        (lambda ()
            (if (not already-run?)
                (begin (set! result (proc))
                       (set! already-run? true)
                       result)
                result))))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define (force delayed-object)
    (delayed-object))

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                     (stream-map proc (stream-cdr s)))))

(define (display-stream s)
    (stream-for-each display-line s))
(define (display-line x)
    (newline)
    (display x))
(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
         low
         (stream-enumerate-interval (+ low 1) high))))
(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((pred (stream-car stream))
           (cons-stream (stream-car stream)
                        (stream-filter pred
                                       (stream-cdr stream))))
          (else (stream-filter pred (stream-cdr stream)))))


(define (show x)
    (display-line x)
    x)

(define x (stream-map show (stream-enumerate-interval 0 100)))

(stream-ref x 5)
(stream-ref x 7)
; 3.50
(define (stream-map proc . argstreams)
    (if (null? (car argstreams))
        '()
        (cons-stream
            (apply proc
                   (map (lambda (s) (stream-car s))
                        argstreams))
            (apply stream-map
                   (cons proc (map (lambda (s) (stream-cdr s))
                                   argstreams))))))

; 3.51
(define (show x)
    (display-line x)
    x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

; 3.52
(define sum 0)
(define (accum x)
    (set! sum (+ x sum))
    sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(stream-ref y 7)
(display-stream z)


; infinity stream
(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
    (stream-filter (lambda (x) (not (divisible? x 7)))
                   integers))

(define (fibgen a b)
    (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
    (cons-stream
     (stream-car stream)
     (sieve (stream-filter
                (lambda (x)
                    (not (divisible? x (stream-car stream))))
                (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))


(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
    (cons-stream 0
                 (cons-stream 1
                              (add-streams (stream-cdr fibs)
                                           fibs))))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes (cons-stream 2
                            (stream-filter prime? (integers-starting-from 3))))


; 3.53
; 1 2 4 8

; 3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))

; 3.55
(define (partial-sums s) (cons-stream (stream-car s) (stream-map + (partial-sums s) (stream-cdr s))))

; 3.56
(define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
            (let ((s1car (stream-car s1))
                  (s2car (stream-car s2)))
                (cond ((< s1car s2car)
                       (cons-stream s1car (merge (stream-cdr s1) s2)))
                      ((> s1car s2car)
                       (cons-stream s2car (merge s1 (stream-cdr s2))))
                      (else
                        (cons-stream s1car
                                     (merge (stream-cdr s1)
                                            (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3))
                                (scale-stream S 5))))


; 3.57


; 3.58
(define (stream-head s n)
    (if (= n 0)
        '()
        (cons (stream-car s)
              (stream-head (stream-cdr s) (- n 1)))))
(define (expand num den radix)
    (cons-stream
        (quotient (* num radix) den)
        (expand (remainder (* num radix) den) den radix)))

(define e1 (expand 1 7 10))

; 3.59

; 3.60

; 3.61

; 3.62


; 3.5.3
(define (average x y) (/ (+ x y) 2))
(define (sqrt-improve guess x)
    (average guess (/ x guess)))

(define (sqrt-stream x)
    (define guesses (cons-stream 1.0
                                 (stream-map (lambda (guess)
                                                (sqrt-improve guess x))
                                             guesses)))
    guesses)

(define (pi-summands n)
    (cons-stream (/ 1.0 n)
                 (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
    (scale-stream (partial-sums (pi-summands 1)) 4))
(define (square x) (* x x))
(define (euler-transform s)
    (let ((s0 (stream-ref s 0))
          (s1 (stream-ref s 1))
          (s2 (stream-ref s 2)))
        (cons-stream (- s2 (/ (square (- s2 s1))
                              (+ s0 (* -2 s1) s2)))
                     (euler-transform (stream-cdr s)))))


(define (make-tableau transform s)
    (cons-stream s
                 (make-tableau transform
                               (transform s))))

(define (accelerated-sequence transform s)
    (stream-map stream-car
                (make-tableau transform s)))

(stream-head (accelerated-sequence euler-transform pi-stream))

; 3.63

; 3.64
(define (stream-limit stream tolerance)
    (let ((ref-n (stream-car stream))
          (ref-n+1 (stream-car (stream-cdr stream))))
        (if (close-enough? ref-n ref-n+1 tolerance)
            ref-n+1
            (stream-limit (stream-cdr stream) tolerance))))

(define (close-enough? x y tolerance)
    (< (abs (- x y))
       tolerance))

; 3.65
(define (exp-summands n)
    (cons-stream (/ 1.0 n)
                 (stream-map - (exp-summands (+ n 1)))))

(define exp-stream (partial-sums (exp-summands 1)))
(euler-transform exp-stream)
(accelerated-sequence euler-transform exp-stream)


; pair-stream
(stream-filter (lambda (pair)
                (prime? (+ (car pair) (cadr pair))))
               int-pairs)

(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x))
                        (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))

; 3.66

; 3.67
(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave (stream-map (lambda (x) (list (stream-car s) x))
                                (stream-cdr t))
                    (interleave (stream-map (lambda (x) (list x (stream-car t)))
                                            (stream-cdr s))
                                (pairs (stream-cdr s) (stream-cdr t))))))

 ; 3.68

 ; 3.69
 (define (triples s t u)
    (stream-map (lambda (x) (list (caar x) (cadar x) (cadr x)))
                (pairs (pairs s t) u)))


(define (bid? i j k)
    (= (+ (square i) (square j)) (square k)))
(define bg (stream-filter (lambda (t)
                            (bid? (car t) (cadr t) (caddr t)))
                          (triples integers integers integers)))


; 3.70
(define (merge-weighted s1 s2 weight)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
            (let ((s1car (stream-car s1))
                  (s2car (stream-car s2)))
                (cond ((< (weight s1car) (weight s2car))
                       (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                      ((> (weight s1car) (weight s2car))
                       (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                      (else
                        (cons-stream s1car
                                     (merge-weighted (stream-cdr s1)
                                                     (stream-cdr s2)
                                                     weight))))))))


(define (weighted-pairs s t weight)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (merge-weighted
            (stream-map (lambda (x) (list (stream-car s) x))
                        (stream-cdr t))
            (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
            weight)))

; a)
(define a (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))
; b)


; 3.71
(define (cube x) (* x x x))
(define (sum-cube pair) (+ (cube (car pair)) (cube (cadr pair))))
(define sr (weighted-pairs integers
                           integers
                           sum-cube))

; 3.72


; stream as signal
(define (integral integrand initial-value dt)
    (define int
        (cons-stream initial-value
                     (add-streams (scale-stream integrand dt)
                                  int)))
    int)

(define (RC ))
