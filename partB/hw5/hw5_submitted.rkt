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
; Part a
(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))
; Part b
(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
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
        ;; CHANGE add more cases here

        ; Value evaluates to itself
        [(int? e) e]
        [(aunit? e) e]
        ; Closure is a valid MUPL entity and needs to be evaluated
        [(closure? e) e]
        
        ; Evaluate ifgreater expressions with type checking
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           ; Only constants can be compared
           (if (and (int? v1)
                     (int? v2))
               ; Extract underlying values, compare and evaluate branch expressions accordingly
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               ; Either the result of evaluating e1 or e2 is not an int, thus cannot be compared
               (error "MUPL ifgreater applied to expressions that evaluates to non-integer")))]

        ; Evaluate mlet expression
        [(mlet? e)
         (let ([varname (mlet-var e)]
               [varval (eval-under-env (mlet-e e) env)])
           ; Evaluate expression body in extended environment (varname = varval binding appended to current environment)
           (eval-under-env (mlet-body e) (cons (cons varname varval) env)))]

        ; Evaluate apair expression
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]

        ; Evaluate fst expression
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to expression that evaluates to non-pair")))]

        ; Evaluate snd expression
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to expression that evaluates to non-pair")))]

        ; Evaluate isaunit expression
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]

        ; Evaluate fun expression
        [(fun? e) (closure env e)]

        ; Evaluate call expression
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([func (closure-fun v1)]
                      [env (closure-env v1)]
                      [fmap (cons (fun-nameopt func) v1)]
                      [fno-op (cons (fun-formal func) v2)])
                 (eval-under-env (fun-body func)
                                 (if (eq? (fun-nameopt func) #f)
                                     ; No need to map function name, just map function argument to subexpression of the call
                                     (cons fno-op env)
                                     ; Map function name to closure, argument to subexpression
                                     ; This is necessary for currying to function correctly
                                     (cons fmap (cons fno-op env)))))                          
               (error  "MPUL call applied to function expression that does not evaluate to a closure")))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
; Part a
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

;Part b
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      ; Recursively evaluate mlet* in expanded environment produced by the current environment, plus bindings defined in (car lstlst)
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

; Part c
(define (ifeq e1 e2 e3 e4)
  ; mlet* is needed such that e1 and e2 are only evaluted once, otherwise they are evaluated twice each
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         ; Say e1 evaluates to v1, e2 evaluates to v2, then v1=v2 iff !(v1 > v2) && !(v2 > v1)
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4
; Part a
(define mupl-map
  (fun "map" "f"
       (fun "helper" "mupls"
            (ifaunit (var "mupls")
                     (aunit)
                     ; Recursively apply f on all list members
                     ; Recall that mupls is a list (pair when evaluated), e.g. (cons m (cons mupls') ...)
                     (apair (call (var "f") (fst (var "mupls")))
                            (call (var "helper") (snd (var "mupls"))))))))
           
; Part b
(define mupl-mapAddN 
  (mlet "map" mupl-map
        ; Double currying going on here, mupl-mapAddN needs the integer i
        ; in order to complete the (already curried) map function
        (fun "helper" "i"
             (call (var "map") (fun "f" "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
