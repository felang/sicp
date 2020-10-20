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