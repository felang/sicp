; 3.1
(define balance 100)

(define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

(define new-withdraw
    (let ((balance 100))
        (lambda (amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                       balance)
                "Insufficient funds")))))

(define (make-withdraw balance)
    (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds")))

(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch)

; (define acc (make-accout 100))
; ((acc 'withdraw) 50)

; 3.1
(define (make-accumulator int)
    (lambda (x)
        (begin (set! int (+ int x))
        int)))

; 3.2
(define (make-monitored f)
    (let ((i 0))
        (lambda (m)
            (cond ((eq? m 'how-many-calls?) i)
                  (else (begin (set! i (+ i 1))
                        (f m)))))))
; (define s (make-monitored sqrt))
; (s 100)
; (s 25)
; (s 'how-many-calls?)

; 3.3
(define (make-account balance secret)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch s m)
        (if (not (= s secret))
            "Incorrect password"
            (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))))
    dispatch)

; 3.4
(define (make-account balance secret)
    (let ((max-try 7)
          (try-times 0))
        (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                    balance)
                "Insufficient funds"))
        (define (deposit amount)
            (set! balance (+ balance amount))
            balance)
        (define (dispatch s m)
            (if (eq? s secret)
                (begin
                    (set! try-times 0)
                    (cond ((eq? m 'withdraw) withdraw)
                          ((eq? m 'deposit) deposit)
                          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
                (begin
                    (set! try-times (+ try-times 1))
                    (if (= try-times max-try)
                        (call-the-cops)
                        "Wrong Password"))))
        dispatch))

; 3.1.2
(define rand
    (let ((x random-init))
        (lambda ()
            (set! x (rand-update x))
            x)))

(define (estimate-pi trials)
    (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
    (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((experiment)
               (iter (- trials-remaining 1) (+ trials-passed 1)))
              (else
               (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

; 3.6
(define rand
    (let ((x random-init))
        (define (dispatch message)
            (cond ((eq? message 'generate)
                   (begin (set! x (rand-update x))
                          x))
                  ((eq? message 'reset)
                   (lambda (new-value) (set! x new-value)))))
        dispatch))

; 3.7
(define (make-account balance secret)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch s m)
        (if (not (= s secret))
            "Incorrect password"
            (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))))
    dispatch)

(define (make-joint account secret new-secret)
    (lambda (s m)
        (if (eq? s new-secret)
            (account secret m)
            "Incorrect password")))

; 3.8
(define f
    (lambda (fv)
        (set! f (secon-value) 0))
        first-value)