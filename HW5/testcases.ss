; my own test case
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