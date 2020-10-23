; 3.12
(define (append! x y)
    (set-cdr! (last-pair x) y)
    x)
(define (append x y)
    (if (null? x)
        y
        (cons (car x) (append (cdr x) y))))
(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)

(define w (append! x y))
w
(cdr x)

; 3.13
(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)
(define z (make-cycle (list 'a 'b 'c)))

; 3.14
(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x))))
    (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

; 3.15

; 3.16
(define (count-pairs x)
    (if (not (pair? x))
        0
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1)))
(define x1 (cons 1 2))
(define three (cons x1 x1)) ;3
(define four (cons 1 (cons x1 x1))) ;4
(define seven (cons 1 （cons x1 x2)) ;7
; 3.17
(define (count-pairs x)
    (let ((encountered '()))
        (define (helper x)
            (if (or (not (pair? x)) (memq x encountered))
                0
                (begin
                    (set! encountered (cons x encountered))
                    (+ (helper (car x))
                       (helper (cdr x))
                       1))))
        (helper x)))
; 3.18
(define (cycle? x)
    (let ((var '()))
        (define (loop x)
            (if (not pair? x)
                false
                (if (memq x var)
                    true
                    (begin
                        (set! var (cons x var))
                        (or (loop (car x))
                            (loop (cdr x)))))))
        (loop x)))
; 3.19

; 3.20



; 3.3.2 queue
(define (front-ptr queue) (car queue))
(define (rear-prt queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue)
        (car (front-prt queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
        (cond ((empty-queue? queue)
               (set-front-ptr! queue new-pair)
               (set-rear-ptr! queue new-pair)
               queue)
              (else
               (set-cdr! (rear-ptr queue) new-pair)
               (set-rear-ptr! queue new-pair)
               queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue)
           (error "DELETE! called with an empty queue" queue))
          (else
           (set-front-ptr! queue (cdr (front-ptr queue)))
           queue)))
; 3.21
(define (print-queue queue)
    (display (front-ptr queue)))

; 3.22
(define (make-queue)
    (let ((front-ptr '())
          (rear-ptr '()))
        (define (dispatch m) (
            (cond ((eq? m 'front-queue) (car front-ptr)))
        ))
        
        dispatch))

(define (front-queue z) (z 'front-queue))
; 3.23