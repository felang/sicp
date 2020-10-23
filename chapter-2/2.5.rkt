; 2.5.1
(define (square x) (* x x))
(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum --- TYPE_TAG" datum)))
(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum)))
(define (attach-tag type-tag contents)
    (cons type-tag contents))
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method for these types -- APPLY_GENERIC" (list op type-tags))))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x y) (apply-generic '=zero? x y))



(define (install-rectangular-package)
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
        (sqrt (+ (square (real-part z))
                 (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))
    
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)

(define (install-polar-package)
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z)
        (* (magnitude z) (cos (angle z))))
    (define (imag-part z)
        (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y)))
              (atan y x)))
    
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))



(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'equ? '(scheme-number scheme-number)
         (lambda (x y) (= x y)))
    (put '=zero? '(scheme-number)
         (lambda (x) (= x 0)))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    'done)

(define (make-scheme-number n) ((get 'make 'scheme-number) n))

(define (install-rational-package)
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (* (numer x) (denom x))
                  (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y))))
    
    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
    (put 'equ? '(rational rational)
        (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
    (put '=zero? '(rational)
        (lambda (x) (= (numer x) 0)))
    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))
    'done)

(define (make-rational n d) ((get 'make 'rational) n d))

(define (install-complex-package)
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))
    
    (define (add-complex z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z1))
                           (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z1))
                           (- (angle z1) (angle z2))))
                           
    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'equ? '(complex complex)
        (lambda (z1 z2) (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2)))))
    (put '=zero? '(complex)
        (lambda (z1) (and (= (real-part z1) 0) (= (imag-part =)))))
    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)

(define (make-complex-from-real-imag x y)
    ((get 'make-complex-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
    ((get 'make-complex-from-mag-ang 'complex) r a))

; 2.78
(define (type-tag datum)
    (if (numer? datum)
        'scheme-number
        (if (pair? datum)
            (car datum)
            (error "Bad tagged datum --- TYPE_TAG" datum))))

(define (contents datum)
    (if (number? datum)
        datum
        (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum))))

(define (attach-tag type-tag contents)
    (if (numer? contents)
        contents
        (cons type-tag contents)))

; 2.79
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
; 2.80
(define (=zero? x y) (apply-generic '=zero? x y))


; 2.5.2
; add procedure
(define (add-complex-to-schemenum z x)
    (make-from-real-imag (+ (real-part z) x)
                         (imag-part z)))

(put 'add '(complex scheme-number)
    (lambda (z x) (tag （add-complex-to-schemenum z x)))

; transfrom type
(define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
(put-coerction 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (let ((t1->t2 (get-coercion type1 type2))
                              (t2->t1 (get-coercion type2 type1)))
                            (cond (t1->t2
                                   (apply-generic op (t1->t2 a1) a2))
                                  (t2->t1
                                   (apply-generic op a1 (t2->t1 a2)))
                                  (else
                                   (error "No method for these types"
                                          (list op type-tags))))))
                    (error "No method for these types"
                           (list op type-tags)))))))

; 2.81
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (if (eq? type1 type2)
                            (error "No method for these types"
                                          (list op type-tags))
                            (let ((t1->t2 (get-coercion type1 type2))
                                  (t2->t1 (get-coercion type2 type1)))
                                (cond (t1->t2
                                    (apply-generic op (t1->t2 a1) a2))
                                    (t2->t1
                                    (apply-generic op a1 (t2->t1 a2)))
                                    (else
                                    (error "No method for these types"
                                            (list op type-tags)))))))
                    (error "No method for these types"
                           (list op type-tags)))))))

; 2.82

; 2.83
; integer
(define (raise i) (make-rat i 1))
(define (raise r) (make-real (/ (numer x) (denom x)))
(define (raise real) (make-from-real-imag x 0))
(put 'raise 'integer raise)
(put 'raise 'rat raise)
(put 'raise 'real raise)
(define (raise x) (apply-generic 'raise x))
; 2.84
(define (level type)
    (cond ((eq? type 'integer) 0)
          ((eq? type 'rat) 1)
          ((eq? type 'real) 2)
          ((eq? type 'complex) 3)
          (else (error "Have not type -- LEVEL" type)))))

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (let ((level1 (level type1))
                              (level2 (level type2))
                            (cond ((< level1 level2)
                                   (apply-generic op (raise a1) a2))
                                  ((> level1 level2)
                                   (apply-generic op a1 (raise a2)))
                                  (else
                                    (error "No method for these types"
                                           (list op type-tags)))))))
                    (error "No method for these types"
                           (list op type-tags)))))))


; 2.85
; complex
(define (projext z) (make-real (real-part z)))

; 2.5.3
(define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

(define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p1)))
        (error "Polys not in same var -- MUL_POLY" (list p1 p2))))

(define (install-polynomial-package)
    (define (make-poly variable term-list)
        (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                    (add-terms (term-list p1)
                               (term-list p2)))
            (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                    (mul-terms (term-list p1)
                                (term-list p1)))
            (error "Polys not in same var -- MUL_POLY" (list p1 p2))))

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial)
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial
        (lambda (var terms) (tag (make-poly var terms))))
    'done)

(define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
                (cond ((> (order t1) (order t1))
                        (adjoin-term
                        t1 (add-terms (rest-terms L1) L2)))
                        ((< (order t1) (order t2))
                        (adjoin-term
                        t2 (add-terms L1 (rest-terms L2))))
                        (else
                        (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t1)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

(define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
            (adjoin-term
                (make-term (+ (order t1) (order t1))
                           (mul (coeff t1) (coeff t2)))
                (mul-term-by-all-terms t1 (rest-terms L))))))


(define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))

; 2.87
(put '=zero? '(polynomial)
    (lambda (p) (empty-termlist (term-list p))))

; 2.88
(define (negative term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist)
        (adjoin-term (make-term (order (first-term term-list))
                                (- 0 (coeff (first-term term-list))))
                     (rest-terms term-list))))

(define (sub-poly p1 p2)
    (add p1
         (make-polynomial (variable p2)
                          (negative (term-list p2)))))

(put 'sub '(polynomial polynomial)
    (lambda (p1 p2) (tag (sub-poly p1 p2))))

; 2.89


; 2.90

; 2.91