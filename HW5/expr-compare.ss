#lang racket
(provide (all-defined-out))

#|
  QUESTION 1: expr-compare
|#

(define (expr-compare x y)
  (cond [(equal? x y) x]
        ; cases with booleans
        [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
        ; if one of them is not list - which means that not function
        [(or (not (list? x)) (not (list? y))) (list 'if '% x y)]
        ; if both are list, but are lists of different length
        [(and (list? x) (list? y) (not (equal? (length x) (length y)))) (list 'if '% x y)]
        ; if both are lists of same lengths
        [(and (list? x) (list? y) (equal? (length x) (length y))) (expr-comp-list-start x y)]
        ))

; starting point for comparing lists of the SAME length 
(define (expr-comp-list-start x y)
  (cond
    ; want to first check if both of them are 'if's 
    [(and (equal? (car x) 'if) (equal? (car x) (car y))) (expr-comp-list x y)]
    ; check if only one of them are 'if'
    [(or (equal? (car x) 'if) (equal? (car y) 'if)) (list 'if '% x y)]
    ; check if either of them are quotes...
    [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) (list 'if '% x y)]
    ; check for beginning with 'lambda'
    [(and (equal? (car x) 'lambda) (equal? (car x) (car y)))
     (cond ; check whether length of arguments are the same
       [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% x y)]
       [else (process-lambda (cdr x) (cdr y) 'lambda '() '())])]
    ; check for beginning with lambda symbol
    [(and (equal? (car x) 'λ) (equal? (car x) (car y)))
     (cond ; check whether length of arguments are the same
       [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% x y)]
       [else (process-lambda (cdr x) (cdr y) 'λ '() '())])]
    ; check for beginning with different lambda symbols
    [(and (lambda? (car x)) (lambda? (car y)))
     (cond ; check whether length of arguments are the same
       [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% x y)]
       [else (process-lambda (cdr x) (cdr y) 'λ '() '())])]
    ; only one begin with lambda
    [(or (lambda? (car x)) (lambda? (car y))) (list 'if '% x y)]
    ; all other cases...
    [else (expr-comp-list x y)]
    ))

; calls when two list of same length is encountered, for lists that are NOT nested in lambda functions
(define (expr-comp-list x y)
  (cond [(and (empty? x) (empty? y)) '()]
        ; if element in lists are equal
        [(equal? (car x) (car y)) (cons (car x) (expr-comp-list (cdr x) (cdr y)))]
        ; if curr element are bools
        [(and (boolean? (car x)) (boolean? (car y))) 
         (cons (if (car x) '% '(not %)) (expr-comp-list (cdr x) (cdr y)))]
        ; case when element in same position of two lists are not equal
        [else (cond
                ; case when both elements are lists of equal length, want to call the other comp-list-start
                [(and (list? (car x)) (list? (car y)) (equal? (length (car x)) (length (car y))))
                 (cons (expr-comp-list-start (car x) (car y)) (expr-comp-list (cdr x) (cdr y)))]
                ; case when they are both lists, but of different length
                [(and (list? (car x)) (list? (car y)))
                 (cons (list 'if '% (car x) (car y)) (expr-comp-list (cdr x) (cdr y)))]
                ; all other cases..
                [else (cons (list 'if '% (car x) (car y)) (expr-comp-list (cdr x) (cdr y)))])]
        ))

; first function call for two lambda expression with same # of parameters
(define (process-lambda x y lambda dictx-list dicty-list)
  (list lambda (process-lambda-args (car x) (car y)) ; append correct lambda and output correctly combined arguments
        (process-lambda-fun (car (cdr x)) ; expr part of lambda expressions
                            (car (cdr y))
                            (cons (build-dictx (car x) (car y)) dictx-list) ; build list of dictionaries
                            (cons (build-dicty (car x) (car y)) dicty-list))))

; processes argument part of lambda functions
(define (process-lambda-args x y)
  (cond [(and (empty? x) (empty? y)) '()]
        ; if two args are named equal
        [(equal? (car x) (car y)) (cons (car x) (process-lambda-args (cdr x) (cdr y)))]
        ; if two args are named differently, we want to combine name
        [else (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))
               (process-lambda-args (cdr x) (cdr y)))]
        ))

; processes 'expr' part of lambda expression
(define (process-lambda-fun x y dictx-list dicty-list)
  ; we first get the most recent name for each element if there exists a new name
  (let ([x-curr (if (equal? (get-latest-name x dictx-list) "Not Found1") x (get-latest-name x dictx-list))]
        [y-curr (if (equal? (get-latest-name y dicty-list) "Not Found1") y (get-latest-name y dicty-list))])
  (cond
    ; if both are lists of same lengths, needs to check this first 
    [(and (list? x) (list? y) (equal? (length x) (length y))) (process-lambda-fun-list-start x y dictx-list dicty-list)]
    ; if curr elements r equal
    [(equal? x-curr y-curr) x-curr]
    ; if are boolean
    [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
    ; if one of them is not list
    [(or (not (list? x)) (not (list? y)))
     ; need to call replace-all to update all variable names before returning output if element is a list
     (list 'if '% (if (list? x) (replace-all x dictx-list #t) x-curr) (if (list? y) (replace-all y dicty-list #t) y-curr))]
    ; if both are list, but are lists of different length
    [(and (list? x) (list? y) (not (equal? (length x) (length y)))) (list 'if '% (replace-all x dictx-list #t) (replace-all y dicty-list #t))]
    )))

; starting point for 'expr' part of lambda expression that are *lists* of the same length
(define (process-lambda-fun-list-start x y dictx dicty)
  (cond
    ; want to first check if both of them are 'if's, append if first and deal with rest of list
    [(and (equal? (car x) 'if) (equal? (car x) (car y))) (cons 'if (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))]
    ; check if only one of them are 'if'
    [(or (equal? (car x) 'if) (equal? (car y) 'if)) (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t))]
    ; check if either of them are quotes...
    [(or (equal? (car x) 'quote) (equal? (car y) 'quote))
     (if (equal? x y) x (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t)))]
    ; check for beginning with 'lambda'
    [(and (equal? (car x) 'lambda) (equal? (car x) (car y)))
     (cond
       [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t))]
       [else (process-lambda (cdr x) (cdr y) 'lambda dictx dicty)])]
    ; check for beginning with lambda symbol
    [(and (equal? (car x) 'λ) (equal? (car x) (car y)))
     (cond
       [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t))]
       [else (process-lambda (cdr x) (cdr y) 'λ dictx dicty)])]
    ; check for beginning with different lambda symbols
    [(and (lambda? (car x)) (lambda? (car y)))
     (cond
       [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t))]
       [else (process-lambda (cdr x) (cdr y) 'λ dictx dicty)])]
    ; only one begin with lambda
    [(or (lambda? (car x)) (lambda? (car y))) (list 'if '% (replace-all x dictx #t) (replace-all y dicty #t))]
    ; all other cases...
    [else (expr-comp-list-lambda x y dictx dicty)]
    ))

; processes elements in lists within lambda functions one by one
(define (expr-comp-list-lambda x y dictx dicty)
  (if (and (empty? x) (empty? y)) '()
      (let ([x-curr (if (equal? (get-latest-name (car x) dictx) "Not Found1") (car x) (get-latest-name (car x) dictx))]
            [y-curr (if (equal? (get-latest-name (car y) dicty) "Not Found1") (car y) (get-latest-name (car y) dicty))])
        (cond
          ; want to first check if they're both lists
          [(and (list? x-curr) (list? y-curr))
           (cond
             ; if lists are of same length
             [(equal? (length (car x)) (length (car y))) (cons (process-lambda-fun-list-start (car x) (car y) dictx dicty)
                                                               (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))]
             ; if lists are different length
             [else (cons (list 'if '% (replace-all (car x) dictx #t) (replace-all (car y) dicty #t)) (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))]
             )]
          ; if curr element are equal
          [(equal? x-curr y-curr) (cons x-curr (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))]
          ; if curr element are bools
          [(and (boolean? (car x)) (boolean? (car y))) 
           (cons (if (car x) '% '(not %)) (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))]
          ; case when only one element is a list
          [(or (list? x-curr) (list? y-curr))
           (list 'if '% (if (list? x) (replace-all x dictx #t) x-curr) (if (list? y) (replace-all y dicty #t) y-curr))]
          ; all other cases...
          [else (cons (list 'if '% x-curr y-curr) (expr-comp-list-lambda (cdr x) (cdr y) dictx dicty))])
        )))

; helper function: checks whether input is either form of lambdas
(define (lambda? x) (member x '(lambda λ)))

; helper function: given a list x and list of dictionaries, updates all variable names
; note: head indicates we're at beginning of list in order to not override if/lambda at beginning of list
(define (replace-all x dictx-list head)
  (cond
    [(empty? x) '()]
    ; if x is a quotes list, we don't want to rename variables
    [(equal? (car x) 'quote) x]
    ; check if begin with lambda or lambda symbol
    [(and head (or (equal? (car x) 'lambda) (equal? (car x) 'λ)))
     (cons (car x) (cons (car (cdr x)) (replace-all (cdr (cdr x)) (cons (build-dictx (car (cdr x)) (car (cdr x))) dictx-list) #f)))]
    ; check if begin with 'if' 
    [(and head (equal? (car x) 'if)) (cons (car x) (replace-all (cdr x) dictx-list #f))]
    ; check if (car x) is a list
    [(list? (car x)) (cons (replace-all (car x) dictx-list #t) (replace-all (cdr x) dictx-list #f))]
    ; case for boolean, don't want to replace
    [(boolean? (car x)) (cons (car x) (replace-all (cdr x) dictx-list #f))]
    ; all other cases, we look for latest name in dictionary if there exists one
    [else (cons
           (if (equal? (get-latest-name (car x) dictx-list) "Not Found1") (car x) (get-latest-name (car x) dictx-list))
           (replace-all (cdr x) dictx-list #f))]
    ))

; helper function: given list of dictionaries in order of new->old, returns the most recent defined name for a variable
(define (get-latest-name x dict-list)
  (cond
    [(empty? dict-list) "Not Found1"]
    [(not (equal? (hash-ref (car dict-list) x "Not Found1") "Not Found1")) (hash-ref (car dict-list) x "Not Found1")]
    [else (get-latest-name x (cdr dict-list))]
    ))

; helper function: builds dictionary for x
(define (build-dictx x y)
  (cond [(and (empty? x) (empty? y)) (hash)]
        [(equal? (car x) (car y)) (hash-set (build-dictx (cdr x) (cdr y)) (car x) (car x))]
        [else (hash-set (build-dictx (cdr x) (cdr y))
                        (car x) (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y)))))]
        ))

; helper function: builds dictionary for y
(define (build-dicty x y)
  (cond [(and (empty? x) (empty? y)) (hash)]
        [(equal? (car x) (car y)) (hash-set (build-dicty (cdr x) (cdr y)) (car y) (car y))]
        [else (hash-set (build-dicty (cdr x) (cdr y))
                        (car y) (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y)))))]
        ))

#|
  QUESTION 2: test-expr-compare
|#

(define (test-expr-compare x y)
  (and
   (equal? (eval x) (eval (list 'let '([% #t]) (expr-compare x y))))
   (equal? (eval y) (eval (list 'let '([% #f]) (expr-compare x y))))
   )
  )

#|
  QUESTION 3: test-expr-x and test-expr-y
|#

(define test-expr-x
  '(lambda (a b) (lambda (b) (if a (quote (a b)) (lambda (d e f) (+ d a f))))))

(define test-expr-y
  '(lambda (c d) (lambda (if) (if if '(a b) (lambda (d h) (+ d if c))))))

#|
  BELOW ARE TESTCASES FROM TA AND MY OWN TESTCASES
|#

#|

; test cases from specs
(equal? (expr-compare 12 12) '12)
(equal? (expr-compare 12 20) '(if % 12 20))
(equal? (expr-compare #t #t) #t)
(equal? (expr-compare #f #f) #f)
(equal? (expr-compare #t #f) '%)
(equal? (expr-compare #f #t) '(not %))
(equal? (expr-compare 'a '(cons a b)) '(if % a (cons a b)))
(equal? (expr-compare '(cons a b) '(cons a b)) '(cons a b))
(equal? (expr-compare '(cons a lambda) '(cons a λ)) '(cons a (if % lambda λ)))
(equal? (expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c))) '(cons (cons a (if % b c)) (cons (if % b a) c)))
(equal? (expr-compare '(cons a b) '(list a b)) '((if % cons list) a b))
(equal? (expr-compare '(list) '(list a)) '(if % (list) (list a)))
(equal? (expr-compare ''(a b) ''(a c)) '(if % '(a b) '(a c)))
(equal? (expr-compare '(quote (a b)) '(quote (a c))) '(if % '(a b) '(a c)))
(equal? (expr-compare '(quoth (a b)) '(quoth (a c))) '(quoth (a (if % b c))))
(equal? (expr-compare '(if x y z) '(if x z z)) '(if x (if % y z) z))
(equal? (expr-compare '(if x y z) '(g x y z)) '(if % (if x y z) (g x y z)))
(equal? (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) '((lambda (a) ((if % f g) a)) (if % 1 2)))
(equal? (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2)) '((λ (a) ((if % f g) a)) (if % 1 2)))
(equal? (expr-compare '((lambda (a) a) c) '((lambda (b) b) d)) '((lambda (a!b) a!b) (if % c d)))
(equal? (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d)) '(if % '((λ (a) a) c) '((lambda (b) b) d)))
(equal? (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2))) '(+
     (not %)
     ((λ (a b!c) (f a b!c)) 1 2)))
(equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2)) '((λ (a b) (f (if % a b) (if % b a))) 1 2))
(equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2)) '((λ (a b!c) (f (if % a b!c) (if % b!c a))) 1 2))

(equal? (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3)) '((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3))
(equal? (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b)))) '((λ (a)
      ((if % eq? eqv?)
       a
       ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
        a (λ (a!b) (if % a!b a)))))
     (lambda (b!a a!b) (b!a a!b))))

; TA test cases posted on Piazza
(equal? (expr-compare '(cons a lambda) '(cons a λ)) '(cons a (if % lambda λ)))
(equal? (expr-compare '(lambda (a) a) '(lambda (b) b)) '(lambda (a!b) a!b))
(equal? (expr-compare '(lambda (a) b) '(cons (c) b)) '(if % (lambda (a) b) (cons (c) b)))
(equal? (expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3)) '((λ (if!fi) (+ if!fi 1)) 3))
(equal? (expr-compare '(lambda (lambda) lambda) '(λ (λ) λ)) '(λ (lambda!λ) lambda!λ))
(equal? (expr-compare ''lambda '(quote λ)) '(if % 'lambda 'λ))
(equal? (expr-compare '(lambda (a b) a) '(λ (b) b)) '(if % (lambda (a b) a) (λ (b) b)))
(equal? (expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b))) '(if % (λ (a b) (lambda (b) b)) (lambda (b) (λ (b) b))))
(equal? (expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y))) '(λ (let) (let (((if % x y) 1)) (if % x y))))
(equal? (expr-compare '(λ (x) ((λ (x) x) x))
              '(λ (y) ((λ (x) y) x))) '(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x))))
(equal? (expr-compare '(((λ (g)
                   ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
                    (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
                 (λ (r)                               ; Here (r) will be the function itself
                   (λ (n) (if (= n 0)
                              1
                              (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
                10)
              '(((λ (x)
                   ((λ (n) (x (λ () (n n))))
                    (λ (r) (x (λ () (r r))))))
                 (λ (g)
                   (λ (x) (if (= x 0)
                              1
                              (* x ((g) (- x 1)))))))
                9)) '(((λ (g!x)
                    ((λ (x!n) (g!x (λ () (x!n x!n))))
                     (λ (x!r) (g!x (λ () (x!r x!r))))))
                  (λ (r!g)
                    (λ (n!x) (if (= n!x 0)
                                 1
                                 (* n!x ((r!g) (- n!x 1)))))))
                 (if % 10 9)))
                 
; my own test case, I decided to not override if/lambdas at beginning of list, both ways work
(equal? (expr-compare '(lambda (a) (a)) '(lambda (b) (a))) '(lambda (a!b) ((if % a!b a)))) ;1
(equal? (expr-compare '(lambda (a) a) '(lambda (b) a)) '(lambda (a!b) (if % a!b a))) ;2
(equal? (expr-compare '(lambda (a b) a) '(lambda (c d) (+ c d))) '(lambda (a!c b!d) (if % a!c (+ a!c b!d)))) ;3
(equal? (expr-compare '(lambda (a b c) (if a b c)) '(lambda (d e f) (lambda (c) (c d e f))))
        '(lambda (a!d b!e c!f) (if % (if a!d b!e c!f) (lambda (c) (c a!d b!e c!f))))) ;4
(equal? (expr-compare '(λ (a b c) (if a b c)) '(lambda (d e f) (lambda (c) (c d e f))))
        '(λ (a!d b!e c!f) (if % (if a!d b!e c!f) (lambda (c) (c a!d b!e c!f))))) ;5
(equal? (expr-compare '(λ (a b c) (if a b c)) '(lambda (d e f) (lambda (c d) (c d e f))))
        '(λ (a!d b!e c!f) (if % (if a!d b!e c!f) (lambda (c d) (c d b!e c!f))))) ;6
(equal? (expr-compare '(λ (a if c) (if a if c)) '(lambda (d e f) (lambda (c d) (c d e f))))
        '(λ (a!d if!e c!f) (if % (if a!d if!e c!f) (lambda (c d) (c d if!e c!f))))) ;7
(equal? (expr-compare '(lambda (a) (lambda (a) (+ a 2))) '(lambda (b) (lambda (if) (if if b c))))
        '(lambda (a!b) (lambda (a!if) (if % (+ a!if 2) (if a!if a!b c))))) ;8
(equal? (expr-compare '(lambda (a) (lambda (a) (+ a 2))) '(lambda (b) (lambda (if b) (if if b c))))
        '(lambda (a!b) (if % (lambda (a) (+ a 2)) (lambda (if b) (if if b c))))) ;9
(equal? (expr-compare '(lambda (a) (lambda (a) (+ a 2))) '(lambda (if) (lambda (b c) (if if b c))))
        '(lambda (a!if) (if % (lambda (a) (+ a 2)) (lambda (b c) (if a!if b c))))) ;10
(equal? (expr-compare '(lambda (a) (lambda (b c) (+ a b))) '(lambda (b) (lambda (e c d) (+ b e c))))
        '(lambda (a!b) (if % (lambda (b c) (+ a!b b)) (lambda (e c d) (+ a!b e c))))) ;11
(equal? (expr-compare '(lambda (a) (quote (a b))) '(lambda (b) (quote (a b))))
        '(lambda (a!b) '(a b))) ;12
(equal? (expr-compare '(lambda (a) (a '(a d) c d)) '(lambda (b) (b '(a b) c d)))
        '(lambda (a!b) (a!b (if % '(a d) '(a b)) c d))) ;13
(equal? (expr-compare '(lambda (a) (a (quote (a d)) c d)) '(lambda (b) (b '(a d) c d)))
        '(lambda (a!b) (a!b '(a d) c d))) ;14
(equal? (expr-compare '(lambda (a) (quote (a d))) '(lambda (b) (b '(a d) c d)))
        '(lambda (a!b) (if % '(a d) (a!b '(a d) c d)))) ;15
(equal? (expr-compare '(lambda (quote) (quote (quote 1))) '(lambda (a) (a (a 1))))
        '(lambda (quote!a) (if % ''1 (quote!a (quote!a 1))))) ;16

|#