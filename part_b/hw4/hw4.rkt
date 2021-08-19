#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (list-ref xs (remainder n (length xs)))]))

;; 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next (s)])
            (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

;; 5
(define funny-number-stream
  (letrec ([start-from
            (lambda (n)
              (cons (if (= (remainder n 5) 0)
                        (- n)
                        n)
                    (lambda () (start-from (+ n 1)))))])
    (lambda () (start-from 1))))

;; 6
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

;; 7
(define (stream-add-zero s)
  (lambda ()
    (let ([next (s)])
      (cons (cons 0 (car next))
            (stream-add-zero (cdr next))))))

;; 8
(define (cycle-lists xs ys)
  (define (stream-from n)
    (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
          (lambda () (stream-from (+ n 1)))))
  (lambda () (stream-from 0)))

;; 9
(define (vector-assoc v vec)
  (let loop ([i 0])
    (if (= i (vector-length vec))
        #f
        (let ([item (vector-ref vec i)])
          (cond [(not (pair? item)) (loop (+ i 1))]
                [(equal? (car item) v) item]
                [else (loop (+ i 1))])))))

;; 10
(define (cached-assoc xs n)
  (define round-robin 0)
  (define cache (make-vector n #f))
  (lambda (v)
    (or (vector-assoc v cache)
        (let ([item (assoc v xs)])
          (vector-set! cache round-robin item)
          (set! round-robin (remainder (+ round-robin 1) n))
          item))))

;; 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([boundary e1])
       (let loop ()
         (if (< e2 boundary)
             (loop)
             #t)))]))
