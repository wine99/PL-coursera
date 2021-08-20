
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1:

(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low
            (sequence (+ low stride) high stride))))

;; Problem 2:

(define (string-append-map xs suffix)
  (map (lambda (s)
         (string-append s suffix))
       xs))

;; Problem 3:

(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [else
         (let ([pos (remainder n (length xs))])
           (list-ref xs pos))]))

;; Problem 4:

(define (stream-for-n-steps s n)
  (if (zero? n)
      '()
      (cons (car (s))
            (stream-for-n-steps (cdr (s)) (- n 1)))))

;; Problem 5:

(define (funny-stream-helper func x)
  (lambda ()
    (cons (func x)
          (funny-stream-helper func (+ x 1)))))

(define funny-number-stream
  (funny-stream-helper (lambda (x)
                         (if (zero? (remainder x 5))
                             (- x)
                             x))
                       1))

;; Problem 6:

(define dan-then-dog
  (funny-stream-helper (lambda (x)
                         (if (zero? (remainder x 2))
                             "dog.jpg"
                             "dan.jpg"))
                       1))

;; Problem 7:

(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s)))
          (stream-add-zero (cdr (s))))))

;; Problem 8:

(define (cycle-lists xs ys)
  (let ([x-length (length xs)]
        [y-length (length ys)])
    (funny-stream-helper (lambda (x)
                           (cons (list-ref xs (remainder x x-length))
                                 (list-ref ys (remainder x y-length))))
                         0)))

;; Problem 9:

(define (vector-assoc v vec)
  (letrec ([vec-length (vector-length vec)]
           [loop (lambda (n)
                   (cond [(= n vec-length) #f]
                         [(and (pair? (vector-ref vec n))
                               (equal? v (car (vector-ref vec n))))  (vector-ref vec n)]
                         [else (loop (+ n 1))]))])
    (loop 0)))

;; Problem 10:

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [index 0]
           [lookup (lambda (elem pos)
                     (cond [(= pos n) #f]
                           [(false? (vector-ref memo pos))  #f]
                           [(equal? elem  (car (vector-ref memo pos)))  (vector-ref memo pos)]
                           [else  (lookup elem (+ 1 pos))]))])
    (lambda (v)
      (let ([val  (lookup v 0)])
        (cond [(false? val) (let ([assoc-lookup (assoc v xs)])
                              (if assoc-lookup
                                  (begin
                                    (vector-set! memo index assoc-lookup)
                                    (set! index (remainder (+ index 1) n))
                                    assoc-lookup)
                                  #f))]
              [else val])))))


 ;; Problem 11:

(define-syntax while-less
  (syntax-rules (do)
    [(while e1 do e2)
     (letrec ([val e1]
              [loop (lambda ()
                      (if (>= e2 val)
                          #t
                          (loop)))])
       (loop))]))



