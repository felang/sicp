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
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue)
        (car (front-ptr queue))))

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
        (define (insert-queue! item)
            (cond ((empty-queue?)
                    (let ((init-list (list item)))
                        (set! front-ptr init-list)
                        (set! rear-ptr init-list)
                        front-ptr))
                  (else
                    (let ((new-item (list item)))
                        (set-cdr! rear-ptr new-item)
                        (set! rear-ptr new-item)
                        front-ptr))))
        (define (delete-queue!)
            (cond ((empty-queue?)
                   (error "DELETE error"))
                  (else
                   (set! front-ptr (cdr front-ptr))
                   front-ptr)))
        (define (empty-queue?)
            (null? front-ptr))
        (define (dispatch m) (
            (cond ((eq? m 'front-queue) (car front-ptr))
                  ((eq? m 'rear-ptr) (car rear-ptr))
                  ((eq? m 'insert-queue!) insert-queue!)
                  ((eq? m 'delete-queue!) (delete-queue!))
                  ((eq? m 'empty-queue?) (empty-queue?)))
        ))
        
        dispatch))

(define (front-queue z) (z 'front-queue))
; 3.23
(define (make-deque) (cons '() '()))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (print-deque deque) (display (front-ptr deque)))
(define (front-insert-deque! deque item)
    (let ((new-pair (cons item '())))
        (if (empty-deque? deque)
            (begin (set-front-ptr! deque new-pair)
                   (set-rear-ptr! deque new-pair)
                   deque)
            (begin (set-cdr! new-pair (front-ptr deque))
                   (set-front-ptr! deque new-pair)
                   deque))))

(define (rear-insert-deque! deque item)
    (let ((new-pair (cons item '())))
        (cond ((empty-deque? deque)
               (set-front-ptr! deque new-pair)
               (set-rear-ptr! deque new-pair)
               deque)
              (else
               (set-cdr! (rear-ptr deque) new-pair)
               (set-rear-ptr! deque new-pair)
               deque))))

(define (front-delete-deque! deque)
    (if (empty-deque? deque)
        (error "DELETE a empty deque" deque)
        (begin (set-front-ptr! deque (cdr (front-ptr deque)))
               deque)))

(define (rear-delete-deque! deque)
    (define (iter deque list)
        (cond ((null? (cdr (cdr list)))
               (begin (set-cdr! list '())
                      (set-rear-ptr! deque list)
                      deque))
              (else
               (iter deque (cdr list)))))
    (cond ((empty-deque? deque)
           (error "DELETE a empty deque" deque))
          ((null? (cdr (front-ptr deque)))
           (begin (set-front-ptr! deque '())
                  deque))
          (else
           (iter deque (front-ptr deque)))))

; (define a (make-deque))
; (front-insert-deque! a 5)
; (front-insert-deque! a 4)
; (rear-insert-deque! a 6)
; (rear-delete-deque! a)
; (front-delete-deque! a)
; (print-deque a)


; 3.3.3
(define (lookup key table)
    (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))

(define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

(define (insert! key value table)
    (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons key value) (cdr table)))))
    'ok)

(define (make-table)
    (list '*table*))

; two-dime-table
(define (lookup key-1 key-2 table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (cdr record)
                    false))
            false)))

(define (insert! key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable
                              (cons (cons key-2 value)
                                    (cdr subtable)))))
            (set-cdr! table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr table)))))
    'ok)
 ; procedure-table
(define (make-table)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (cdr record)
                            false))
                    false)))
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                      (cons (cons key-2 value)
                                      (cdr subtable)))))
                    (set-cdr! local-table
                              (cons (list key-1 (cons key-2 value))
                                    (cdr local-table)))))
            'ok)
        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Unknown operation -- TABLE" m))))
        dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; 3.24
(define (make-table same-key?)
    (let ((local-table (list '*table*)))
        (define (assoc key records)
            (cond ((null? records) false)
                  ((same-key? key (caar records)) (car records))
                (else (assoc key (cdr records)))))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (cdr record)
                            false))
                    false)))
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                      (cons (cons key-2 value)
                                      (cdr subtable)))))
                    (set-cdr! local-table
                              (cons (list key-1 (cons key-2 value))
                                    (cdr local-table)))))
            'ok)
        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Unknown operation -- TABLE" m))))
        dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


; 3.25
(define (make-table)
    (list '*table*))
(define (lookup key-list table)
    (define (iter key-list table result)
        (if (null? key-list)
            result
            (let ((record (assoc (car key-list) (cdr table))))
                (if record
                    (iter (cdr key-list) record (car record))
                    false))))
    (iter key-list table false))

(define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

(define (insert! key-list value table)
    (if (list? key-list)
        (let ((current-key (car key-list))
              (remain-key (cdr key-list)))
            (let ((record (assoc current-key (cdr table))))
                (cond
                    ((and record (null? remain-key))
                     (set-cdr! record value) table)
                    ((and record remain-key)
                     (insert! remain-key value record) table)
                    ((and (not record) (not (null? remain-key)))
                     (join-in-table (insert! remain-key value (make-table current-key)) table) table)
                    ((and (not record) (null? remain-key))
                     (let ((new-record (cons current-key value)))
                        (join-in-table new-record table) table)))))
        (insert! (list key-list) value table)))
(define (join-in-table new-record table)
    (set-cdr! table
              (cons new-record (cdr table))))
(define (make-table . table-name)
    (if (null? table-name)
        (list '*table*)
        table-name))
; 3.26

; 3.27



; 3.3.4
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
        'ok)

(define (full-adder a b c-in sum c-out)
    (let ((s (make-wire))
          (c1 (make-wire))
          (c2 (make-wire)))
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
        'ok))

(define (inverter input output)
    (define (invert-input)
        (let ((new-value (logical-not (get-signal input))))
            (after-delay inverter-delay
                         (lambda ()
                            (set-signal! output new-value)))))
    (add-action! input invert-input)
    'ok)

(define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
    (if (and (= s1 1) (= s2 1))
        1
        0))

(define (logical-or s1 s2)
    (if (or (= s1 1) (= s2 1))
        1
        0))

(define (and-gate a1 a2 output)
    (define (and-action-procedure)
        (let ((new-value
                (logical-and (get-signal a1) (get-signal a2))))
            (after-delay and-gate-delay
                         (lambda ()
                            (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok)

; 3.28
(define (or-gate a1 a2 output)
    (define (or-action-procedure)
        (let ((new-value
                (logical-or (get-signal a1) (get-signal a2))))
            (after-delay or-gate-delay
                         (lambda ()
                            (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok)

; 3.29
(define (or-gate a1 a2 output)
    (let ((invert-1 (make-wire))
          (invert-2 (make-wire))
          (and-invert-1-invert-2 (make-wire)))
        (inverter input-1 invert-1)
        (inverter input-2 invert-2)
        (and-gate invert-1 invert-2 and-invert-1-invert-2)
        (inverter and-invert-1-invert-2 output)))

; 3.30

(define (ripple-carry-adder ak bk sk c)
    (define (iter a b s vc)
        (if (and (null? a) (null? b) (null? s))
            'ok
            (let ((ck (make-wire)))
                (set-signal! ck vc)
                (full-adder (car a) (car b) ck (car sk) c)
                (iter (cdr ak) (cdr bk) (cdr sk) (get-signal c)))))
    (iter ak bk sk (get-signal c)))
        

; wire
(define (make-wire)
    (let ((signal-value 0) (action-procedures '()))
        (define (set-my-signal! new-value)
            (if (not (= signal-value new-value))
                (begin (set! signal-value new-value)
                       (call-each action-procedures))
                'done))
        
        (define (accept-action-procedure! proc)
            (set! action-procedures (cons proc action-procedures))
            (proc))
            
        (define (dispatch m)
            (cond ((eq? m 'get-signal) signal-value)
                  ((eq? m 'set-signal!) set-my-signal!)
                  ((eq? m 'add-action!) accept-action-procedure!)
                  (else (error "Unknow operation -- WIRE" m))))
        dispatch))

(define (call-each procedures)
    (if (null? procedures)
        'done
        (begin
            ((car procedures))
            (call-each (cdr procedures)))))

(define (get-signal wire)
    (wire 'get-signal))

(define (set-signal! wire new-value)
    ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
    ((wire 'add-action!) action-procedure))

; delay-agenda
(define (make-time-segment time queue)
    (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
    (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
    (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
        (or (null? segments)
            (< time (segment-time (car segments)))))
    (define (make-new-time-segment time action)
        (let ((q (make-queue)))
            (insert-queue! q action)
            (make-time-segment time q)))
    (define (add-to-segments! segments)
        (if (= (segment-time (car segments)) time)
            (insert-queue! (segment-queue (car segments))
                           action)
            (let ((rest (cdr segments)))
                (if (belongs-before? rest)
                    (set-cdr!
                     segments
                     (cons (make-new-time-segment time action)
                           (cdr segments)))
                    (add-to-segments! rest)))))
    (let ((segments (segments agenda)))
        (if (belongs-before? segments)
            (set-segments!
             agenda
             (cons (make-new-time-segment time action)
                   segments))
            (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
        (delete-queue! q)
        (if (empty-queue? q)
            (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "Agenda is empty -- FIRST-AGENDA-ITEM")
        (let ((first-seg (first-segment agenda)))
            (set-current-time! agenda (segment-time first-seg))
            (front-queue (segment-queue first-seg)))))

(define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time the-agenda))
                    action
                    the-agenda))

(define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (let ((first-item (first-agenda-item the-agenda)))
            (first-item)
            (remove-first-agenda-item! the-agenda)
            (propagate))))

(define (probe name wire)
    (add-action! wire
                 (lambda ()
                    (newline)
                    (display name)
                    (display " ")
                    (display (current-time the-agenda))
                    (display " New-value = ")
                    (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
(set-signal! input-2 1)
(propagate)

; 3.31

; 3.32