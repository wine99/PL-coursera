;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist xs)
  (if (empty? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      empty
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))
     
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error (format "MUPL fst applied to non-pair: ~v [~v]" e v))))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error (format "MUPL snd applied to non-pair: ~v [~v]" e v))))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)]) (if (aunit? v) (int 1) (int 0)))]
        [(mlet? e)
         (letrec ([var (mlet-var e)]
                  [var-value (eval-under-env (mlet-e e) env)]
                  [new-env (cons (cons var var-value) env)])
           (eval-under-env (mlet-body e) new-env))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([a-closure (eval-under-env (call-funexp e) env)])
           (if (closure? a-closure)
               (letrec ([fun-define (closure-fun a-closure)]
                        [fun-param (eval-under-env (call-actual e) env)]
                        [fun-variable-name (fun-formal fun-define)]
                        [env-with-variable-and-func (list (cons fun-variable-name fun-param) (cons (fun-nameopt fun-define) a-closure))]
                        [run-env (append env-with-variable-and-func (closure-env a-closure) env)])
                 (eval-under-env (fun-body fun-define) run-env))
               (error "MUPL call applied to non-function")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (cond [(not (and (int? v1) (int? v2))) (error "MUPL ifgreater applied to non-number e1/e2")]
                 [(> (int-num v1) (int-num v2)) (eval-under-env (ifgreater-e3 e) env)]
                 [#t (eval-under-env (ifgreater-e4 e) env)]))]
           
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([yield_pair (car lstlst)])
        (mlet (car yield_pair) (cdr yield_pair) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons"_y" e2))
         (ifgreater (var "_x") (var "_y") e4 ; when e1 <= e2 and e2 <= e1, they must be equal
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "_f"
       (fun "_r" "_list"
            (ifaunit (var "_list")
                     (aunit)
                     (apair (call (var "_f") (fst (var "_list"))) (call (var "_r") (snd (var "_list"))))))))
      
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "n"
             (call (var "map") (fun #f "x" (add (var "n") (var "x")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)      ;feels a little verbose....
  (letrec ([loop (lambda (e)       ;calc set of free vars from exp
                   (cond [(var? e) (set (var-string e))]
                         [(add? e) (set-union (loop (add-e1 e)) (loop (add-e2 e)))]
                         [(apair? e) (set-union (loop (apair-e1 e)) (loop (apair-e2 e)))]
                         [(fst? e) (loop (fst-e e))]
                         [(snd? e) (loop (snd-e e))]
                         [(isaunit? e) (loop (isaunit-e e))]
                         [(mlet? e) (set-union (loop (mlet-e e)) (loop (mlet-body e)))]
                         [(call? e) (set-union (loop (call-funexp e)) (loop (call-actual e)))]
                         [(ifgreater? e) (set-union (loop (ifgreater-e1 e))
                                                    (loop (ifgreater-e2 e))
                                                    (loop (ifgreater-e3 e))
                                                    (loop (ifgreater-e4 e)))]
                         [(fun? e) (let ([fvs (loop (fun-body e))])
                                     (set-remove fvs (fun-formal e)))]
                         [#f (set)]))])
    (cond [(int? e) e]
          [(closure? e) e]
          [(aunit? e) e]
          [(fun-challenge? e) e]
          [(var? e) e]
          [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars (add-e2 e)))]
          [(apair? e) (apair (compute-free-vars (apair-e1 e)) (compute-free-vars (apair-e2 e)))]
          [(fst? e) (fst (compute-free-vars (fst-e e)))]
          [(snd? e) (snd (compute-free-vars (snd-e e)))]
          [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
          [(mlet? e) (mlet (mlet-var e) (compute-free-vars (mlet-e e)) (compute-free-vars (mlet-body e)))]
          [(fun? e) 
           (fun-challenge (fun-nameopt e) (fun-formal e) (compute-free-vars (fun-body e)) (loop e))]
          [(call? e) (call (compute-free-vars (call-funexp e)) (compute-free-vars (call-actual e)))]
          [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e))
                                     (compute-free-vars (ifgreater-e2 e))
                                     (compute-free-vars (ifgreater-e3 e))
                                     (compute-free-vars (ifgreater-e4 e)))]
          ;; CHANGE add more cases here
          [#t (error (format "bad MUPL expression: ~v" e))])))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) 
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(apair? e) (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error (format "MUPL fst applied to non-pair: ~v [~v]" e v))))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error (format "MUPL snd applied to non-pair: ~v [~v]" e v))))]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)]) (if (aunit? v) (int 1) (int 0)))]
        [(mlet? e)
         (letrec ([var (mlet-var e)]
                  [var-value (eval-under-env-c (mlet-e e) env)]
                  [new-env (cons (cons var var-value) env)])
           (eval-under-env-c (mlet-body e) new-env))]
        [(fun-challenge? e)
         (closure (set-map (fun-challenge-freevars e) (lambda (s) (cons s (envlookup env s)))) e)]
        [(call? e)
         (let ([a-closure (eval-under-env-c (call-funexp e) env)])
           (if (closure? a-closure)
               (letrec ([fun-define (closure-fun a-closure)]
                        [fun-param (eval-under-env-c (call-actual e) env)]
                        [fun-variable-name (fun-challenge-formal fun-define)]
                        [env-with-variable-and-func (list (cons fun-variable-name fun-param) (cons (fun-challenge-nameopt fun-define) a-closure))]
                        [run-env (append env-with-variable-and-func (closure-env a-closure) env)])
                 (eval-under-env-c (fun-challenge-body fun-define) run-env))
               (error "MUPL call applied to non-function")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (cond [(not (and (int? v1) (int? v2))) (error "MUPL ifgreater applied to non-number e1/e2")]
                 [(> (int-num v1) (int-num v2)) (eval-under-env-c (ifgreater-e3 e) env)]
                 [#t (eval-under-env-c (ifgreater-e4 e) env)]))]
           
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))
  
;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

