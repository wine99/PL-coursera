
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (el) (string-append el suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty-list")]
        [#t
         (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (stream acc)
                (if (> acc n)
                    null 
                    (let ([pr (stream)])
                      (cons (car pr) (f (cdr pr) (+ 1 acc))))))])
    (f s 1)))

(define funny-number-stream
  (letrec ([f (lambda (n)
                (cons (if (= (remainder n 5) 0) (- n) n)
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([values (list "dan.jpg" "dog.jpg")]
           [f (lambda (n)
                (cons (list-nth-mod values n)
                      (lambda () (f (+ 1 n)))))])
    (lambda () (f 0))))

(define (stream-add-zero s)
  (letrec ([f (lambda (stream)
                (let ([pr (stream)])
                  (cons (cons 0 (car pr)) (lambda () (f (cdr pr))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (if (not (vector? vec))
      (error "second argument must be a vector")
      (letrec ([l (vector-length vec)]
               [f (lambda (n)
                    (cond [(>= n l) #f]
                          [(not (pair? (vector-ref vec n))) (f (+ 1 n))]
                          [#t
                           (let ([val (vector-ref vec n)])
                             (if (equal? (car val) v)
                                 val
                                 (f (+ 1 n))))]))])
        (f 0))))

(define (cached-assoc xs n)
  (let* ([cache (make-vector n #f)]
         [next-ref 0]
         [f (lambda (val)
              (or (vector-assoc val cache)
                  (let ([v-ans (assoc val xs)])
                    (and v-ans
                         (begin
                           (vector-set! cache next-ref (cons val v-ans))
                           (set! next-ref (if (>= (+ next-ref 1) n)
                                              0
                                              (+ next-ref 1)))
                           v-ans)))))])
    (lambda (v) (f v))))