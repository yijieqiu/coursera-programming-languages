
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix)) xs))

; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(= (length xs) 0) (error "list-nth-mod: empty list")]
          [#t (car (list-tail xs (remainder n (length xs))))]))

; Problem 4
(define (stream-for-n-steps s n)
  (cond [(= n 0) null]
        [#t (let ([p (s)])
              (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))]))

; Problem 5
(define funny-number-stream
  (letrec ([negate-fives (lambda (x)
                           (if (= (remainder x 5) 0)
                               (cons (- x) (lambda () (negate-fives (+ x 1)))) 
                               (cons x (lambda () (negate-fives (+ x 1))))))])
    (lambda () (negate-fives 1))))

; Problem 6
(define dan-then-dog 
  (letrec ([alternate-str (lambda (flag)
                (if flag
                    (cons "dan.jpg" (lambda () (alternate-str #t)))
                    (cons "dog.jpg" (lambda () (alternate-str #f)))))])
    (lambda () (alternate-str #t))))

; Problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (stream)
                (let ([p (stream)])
                  (lambda () (cons (cons 0 (car p)) (stream-add-zero (cdr p))))))])
    (f s)))

; Problem 8
(define (cycle-lists xs ys)
  (letrec ([get-and-increment (lambda (n)
                                 (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (get-and-increment (+ n 1)))))])
    (lambda () (get-and-increment 0))))

; Problem 9
(define (vector-assoc v vec)
  [letrec ([f (lambda (n)
                (if (= n (vector-length vec))
                    #f
                    (let ([x (vector-ref vec n)])
                      (if (and (pair? x) (equal? (car x) v))
                          x
                          (f (+ n 1))))))])
    (f 0)])

; Problem 10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [i 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! memo i new-ans)
                              (set! i (remainder (+ i 1) n))
                              new-ans)
                            #f)))))])
    f))
                         
                            
                
   
                
      
           
           
  

                      
                              
                  
  
     