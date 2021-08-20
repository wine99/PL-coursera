
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      empty))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (empty? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

(define (stream-for-n-steps s n)
  (if (<= n 0)
      empty
      (let ([next (s)])
        (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([current (if (= 0 (remainder x 5))
                                   (- x)
                                   x)])
                  (cons current (lambda () (f (+ 1 x))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (cons (if x "dan.jpg" "dog.jpg") (lambda() (f (not x)))))])
    (lambda () (f #t))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (let ([current (x)])
                  (cons (cons 0 (car current)) (lambda () (f (cdr current))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([v-length (vector-length vec)]
           [loop (lambda (n)
                   (if (>= n v-length)
                       #f
                       (let ([current (vector-ref vec n)])
                         (if (and (cons? current) (equal? v (car current)))
                             current
                             (loop (+ n 1))))))])
    (loop 0)))

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [offset 0])
    (lambda (v)
      (let ([cached-result (vector-assoc v cache)])
        (if cached-result
            cached-result
            (let ([t (assoc v xs)])
              (if t
                  (begin
                    (vector-set! cache offset t)
                    (set! offset (remainder (+ 1 offset) n))
                    t)
                  #f)))))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2) (letrec ([flag e1]
                                    [loop (lambda ()
                                            (let ([v e2])
                                              (if (< v flag) (loop) #t)))])
                             (loop))]))
