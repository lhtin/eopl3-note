(module exercise-2 (lib "eopl.ss" "eopl")

  ;; copied from https://github.com/mwand/eopl3/blob/master/chapter2/utils.scm
  (define-syntax equal??
    (syntax-rules ()
      ((_ x y)
       (let ((x^ x) (y^ y))
         (if (not (equal? x y))
           (eopl:error 'equal??
             "~s is not equal to ~s" 'x 'y)
             #t)))))

  ;; Exercise 2.1 [*]
  (let ((base 16))

    ;; Bignum representation
    (define zero
      (lambda ()
        '()))
    (define is-zero?
      (lambda (n)
        (null? n)))
    (define successor
      (lambda (n)
        (cond
          ((is-zero? n) '(1))
          ((eqv? (+ (car n) 1) base) (cons 0 (successor (cdr n))))
          (else (cons (+ (car n) 1) (cdr n))))))
    (define predecessor
      (lambda (n)
        (cond
          ((is-zero? n) (eopl:error 'predecessor "zero has no predecessor"))
          ((eqv? (car n) 0) (cons (- base 1) (predecessor (cdr n))))
          ((eqv? (car n) 1)
           (if (is-zero? (cdr n))
               '()
               (cons 0 (cdr n))))
          (else (cons (- (car n) 1) (cdr n))))))

    ;; unchanged below here!

    (define plus
      (lambda (x y)
        (if (is-zero? x)
          y
          (successor (plus (predecessor x) y)))))

    (define (scheme-int->my-int n)
        (if (zero? n) (zero)
          (successor (scheme-int->my-int (- n 1)))))

    (define (my-int->scheme-int x)
        (if (is-zero? x) 0
          (+ 1 (my-int->scheme-int (predecessor x)))))

    (equal?? 
      (my-int->scheme-int
        (plus 
          (scheme-int->my-int 3)
          (scheme-int->my-int 7)))
      10)

    #| 执行比较慢，所以默认先注释
    (set! base 2) ;; base必须大于等于2，为10时即十进制
    (define arg1 (* 9 8 7 6 5 4 3 2 1))
    (time (scheme-int->my-int arg1))
    (define arg2 (* 10 9 8 7 6 5 4 3 2 1))
    (time (scheme-int->my-int arg2))
    (eopl:printf "~s~%" (scheme-int->my-int arg2))
    
    (set! base 90)
    (time (scheme-int->my-int arg2))
    (eopl:printf "~s~%" (scheme-int->my-int arg2))
    |#
    ;; 参数的变化（阶乘n!的变化），会极大的影响运行时间，(n+1)!的运行时间是n!的n+1倍
    ;; base的变化影响不大
  )
  (eopl:printf "Exercise 2.1 passed~%")

  ;; Exercise 2.2 [**]
  ;; 1. Unary representation: 所有接口都可以很简单的实现，但是占用空间大并且不方便人识别
  ;; 2. Scheme number representation: 利用Scheme的数字表示，实现简单
  ;; 3. Bignum representation: 实现复杂，不过比起二进制，更大的base可以使用更少的数字表示相同的数
  (eopl:printf "Exercise 2.2 passed~%")

  ;; Exercise 2.3 [**]
  ;; 1.
  ;; 先证明0有多种表示。根据数学规则，两个相同的数字相减为0，又因为数字有无限多个，所以0有无限多种表示；再证明非0数字有多种表示。根据数学规则，任何数字减去0仍等于原来的数字，又因为0有多种表示，所以非0数字也有多种表示
  (let ()
    ;; 2.
     (define (my-int->scheme-int x)
        (if (eqv? (car x) 'diff)
            (- (my-int->scheme-int (cadr x))
               (my-int->scheme-int (caddr x)))
            1))
    (define zero
      (lambda ()
        '(diff (one) (one))))
    (define is-zero?
      (lambda (n)
        (eqv? (my-int->scheme-int n))))
    (define successor
      (lambda (n)
        (list 'diff n '(diff (diff (one) (one)) (one)))))
    (define predecessor
      (lambda (n)
        (list 'diff n '(one))))
    ;; test
    (equal??
     (my-int->scheme-int (successor '(one)))
     2)
    (equal??
     (my-int->scheme-int (predecessor (predecessor '(one))))
     -1)

    ;; 3.
    (define diff-tree-plus
      (lambda (n1 n2)
        (if (eqv? (car n2) 'one)
            ;; n1 + 1 == n1 - (-1)
            (list 'diff n1 '(diff (diff (one) (one)) (one)))
            ;; n2 == (c - d)
            ;; n1 + (c - d) == n1 - (d - c)
            (list 'diff n1 (list 'diff (caddr n2) (cadr n2))))))
    ;; test
    (equal??
     (my-int->scheme-int (diff-tree-plus '(one) '(one)))
     2)
    (equal??
     (my-int->scheme-int
      (diff-tree-plus
       (predecessor (predecessor '(one)))
       (predecessor (predecessor '(one)))))
     -2)
    )
  (eopl:printf "Exercise 2.3 passed~%")

  ;; Exercise 2.4 [**]
  ;; (empty-stack) = empty
  ;; (push item [stack]) = [new-stack], where (top [new-stack]) == item and (pop [new-stack]) == [stack]
  ;; (pop [new-stack]) = [stack], reverse operation of push, removed the latest pushed item
  ;; (top [stack]) = item, the latest pushed item
  ;; (empty-stack? [stack])
  ;; constructors: empty-stack, push, pop
  ;; observers: top, empty-stack?
  (eopl:printf "Exercise 2.4 passed~%")

  ;; the lambda copied from eopl3 book
  (define report-no-binding-found
    (lambda (search-var)
      (eopl:error 'apply-env "No binding for ~s" search-var)))
  ;; Exercise 2.5 [*]
  (let ()
    ;; empty-env: () -> '()
    (define empty-env
      (lambda ()
        '()))
    ;; extend-env: Var * SchemeVal * List -> List
    (define extend-env
      (lambda (var val env)
        (cons (cons var val) env)))
    ;; apply-env: List * Var -> SchemeVal
    (define apply-env
      (lambda (env search-var)
        (cond
          ((null? env) (report-no-binding-found search-var))
          ((eqv? (caar env) search-var) (cdar env))
          (else (apply-env (cdr env) search-var)))))
          
    (equal?? (empty-env) '())
    (equal?? (extend-env 'a 123 (empty-env)) '((a . 123)))
    (equal?? (apply-env (extend-env 'a 123 (empty-env)) 'a) 123)
    (equal?? (apply-env (extend-env 'a 123 (empty-env)) 'a) 123)
    )
  (eopl:printf "Exercise 2.5 passed~%")

  ;; Exercise 2.6 [*]
  ;; 1.
  ;; env: ((var2 var1) (val2 val1))
  ;; pair-list
  (let ()
    (define empty-env
      (lambda ()
        '(() ())))
    (define extend-env
      (lambda (var val env)
        (list
         (cons var (car env))
         (cons val (cadr env)))))
    (define apply-env
      (lambda (env search-var)
        (cond
          ((null? (car env)) (report-no-binding-found search-var))
          ((eqv? (caar env) search-var) (caadr env))
          (else (apply-env (list (cdar env) (cdadr env)) search-var)))))
    (equal?? (empty-env) '(() ()))
    (equal?? (extend-env 'a 123 (empty-env)) '((a) (123)))
    (equal?? (apply-env (extend-env 'a 123 (empty-env)) 'a) 123)
    (equal?? (apply-env (extend-env 'a 123 (empty-env)) 'a) 123)
    )
  (eopl:printf "Exercise 2.6 passed~%")

  
  ;; Exercise 2.7 [*]
  (let ()
    (define empty-env
      (lambda () (list 'empty-env)))
    (define extend-env
      (lambda (var val env)
        (list 'extend-env var val env)))
    (define apply-env
      (lambda (env search-var)
        (apply-env0 env search-var env)))
    (define apply-env0
      (lambda (env search-var originEnv)
        (cond
          ((eqv? (car env) 'empty-env) (report-no-binding-found search-var originEnv))
          ((eqv? (car env) 'extend-env)
           (let ((saved-var (cadr env))
                 (saved-val (caddr env))
                 (saved-env (cadddr env)))
             (if (eqv? search-var saved-var)
                 saved-val
                 (apply-env0 saved-env search-var originEnv))))
          (else
           (report-invalid-env originEnv)))))
    (define report-no-binding-found
      (lambda (search-var env)
        (eopl:error 'apply-env "No binding for ~s in ~s" search-var env)))
    (define report-invalid-env
      (lambda (env)
        (eopl:error 'apply-env "Bad environment: ~s" env)))
    ;; (apply-env (extend-env 'a 123 (empty-env)) 'b)
    (apply-env (extend-env 'a 123 (empty-env)) 'a)
    )
  (eopl:printf "Exercise 2.7 passed~%")

  
  (let ()
    (define empty-env
      (lambda ()
        '()))
    (define extend-env
      (lambda (var val env)
        (cons (cons var val) env)))
    (define apply-env
      (lambda (env search-var)
        (cond
          ((empty-env? env) (report-no-binding-found search-var))
          ((eqv? (caar env) search-var) (cadr env))
          (else (apply-env (cdr env) search-var)))))
    ;; Exercise 2.8 [*]
    (define empty-env?
      (lambda (env)
        (null? env)))
    
    (equal?? (empty-env? (empty-env)) #t)
    (equal?? (empty-env? (extend-env 'a 123 (empty-env))) #f)
    (eopl:printf "Exercise 2.8 passed~%")

    ;; Exercise 2.9 [*]
    (define has-binding?
      (lambda (env search-var)
        (cond
          ((empty-env? env) #f)
          ((eqv? (caar env) search-var) #t)
          (else (has-binding? (cdr env) search-var)))))
    
    (equal?? (has-binding? (empty-env) 'a) #f)
    (equal?? (has-binding? (extend-env 'a 123 (empty-env)) 'a) #t)
    (eopl:printf "Exercise 2.9 passed~%")

    ;; Exercise 2.10 [*]
    (define extend-env*
      (lambda (var-list val-list env)
        (if (null? var-list)
            env
            (extend-env*
             (cdr var-list)
             (cdr val-list)
             (extend-env (car var-list) (car val-list) env)))))
    (equal?? (extend-env* '(a b c) '(1 2 3) (empty-env)) '((c . 3) (b . 2) (a . 1)))
    (eopl:printf "Exercise 2.10 passed~%")
    )

  ;; Exercise 2.11 [**]
  (let ()
    (define empty-env
      (lambda ()
        '()))
    (define extend-env
      (lambda (var val env)
        (cons (cons (list var) (list val)) env)))
    (define apply-env
      (lambda (env search-var)
        (cond
          ((null? env) (report-no-binding-found search-var))
          (else (apply-ribs (caar env) (cdar env) (cdr env) search-var)))))
    (define apply-ribs
      (lambda (l-rib r-rib rest-env var)
        (cond
          ((null? l-rib) (apply-env rest-env var))
          ((eqv? (car l-rib) var) (car r-rib))
          (else (apply-ribs (cdr l-rib) (cdr r-rib) rest-env var)))))
    (define extend-env*
      (lambda (var-list val-list env)
        (if (null? var-list)
            env
            (cons (cons var-list val-list) env))))
    
    (equal?? (empty-env) '())
    (equal?? (extend-env 'a 123 (empty-env)) '(((a) . (123))))
    (equal?? (apply-env (extend-env 'a 123 (empty-env)) 'a) 123)
    (equal?? (apply-env (extend-env 'a 123 (empty-env)) 'a) 123)
    (equal?? (extend-env* '(a b c) '(11 12 13)
                          (extend-env* '(x z) '(66 77)
                                       (extend-env* '(x y) '(88 99)
                                                    (empty-env))))
             '(((a b c) . (11 12 13)) . (((x z) . (66 77)) . (((x y) . (88 99))))))
    )
  (eopl:printf "Exercise 2.11 passed~%")

  ;; Exercise 2.12 [*]
  (let ()
    (define empty-stack
      (lambda ()
        (lambda (action)
          (cond
            ((eqv? action 'pop) (eopl:error 'empty-stack "empty stack can't pop"))
            ((eqv? action 'top) (eopl:error 'empty-stack "empty stack no top"))
            ((eqv? action 'is-emtpy?) #t)))))
    (define push
      (lambda (item stack)
        (lambda (action)
          (cond
            ((eqv? action 'top) item)
            ((eqv? action 'pop) stack)
            ((eqv? action 'is-emtpy?) #f)))))
    (define pop
      (lambda (stack)
        (stack 'pop)))
    (define top
      (lambda (stack)
        (stack 'top)))
    (define empty-stack?
      (lambda (stack)
        (stack 'is-emtpy?)))

    ;; test
    (equal?? (empty-stack? (empty-stack)) #t)
    (equal?? (empty-stack? (push 1 (empty-stack))) #f)
    (equal?? (top (push 2 (empty-stack))) 2)
    (equal?? (top (pop (push 4 (push 3 (empty-stack))))) 3)
    
    )
  (eopl:printf "Exercise 2.12 passed~%")

  ;; Exercise 2.13 [**]
  (let ()
    (define empty-env
      (lambda ()
        (cons
         (lambda (search-var)
           (report-no-binding-found search-var))
         (lambda () #t))))
    (define extend-env
      (lambda (saved-var saved-val saved-env)
        (cons
         (lambda (search-var)
           (if (eqv? search-var saved-var) saved-val
               (apply-env saved-env search-var)))
         (lambda () #f))))
    (define apply-env
      (lambda (env search-var)
        ((car env) search-var)))
    (define empty-env?
      (lambda (env)
        ((cdr env))))

    (equal?? (empty-env? (empty-env)) #t)
    (equal?? (empty-env? (extend-env 'a 123 (empty-env))) #f)
    (equal?? (apply-env (extend-env 'b 456 (empty-env)) 'b) 456)
    )
  (eopl:printf "Exercise 2.13 passed~%")

  ;; Exercise 2.14 [**]
  (let ()
    (define empty-env
      (lambda ()
        (list
         (lambda (search-var)
           (report-no-binding-found search-var))
         (lambda () #t)
         (lambda () #f))))
    (define extend-env
      (lambda (saved-var saved-val saved-env)
        (list
         (lambda (search-var)
           (if (eqv? search-var saved-var) saved-val
               (apply-env saved-env search-var)))
         (lambda () #f)
         (lambda (var)
           (or (eqv? var saved-var)
               ((caddr saved-env) var))))))
    (define apply-env
      (lambda (env search-var)
        ((car env) search-var)))
    (define empty-env?
      (lambda (env)
        ((cadr env))))
    (define has-binding?
      (lambda (env var)
        ((caddr env) var)))

    (equal?? (empty-env? (empty-env)) #t)
    (equal?? (empty-env? (extend-env 'a 123 (empty-env))) #f)
    (equal?? (apply-env (extend-env 'b 456 (empty-env)) 'b) 456)
    (equal?? (has-binding? (extend-env 'c 789 (empty-env)) 'c) #t)
    )
  (eopl:printf "Exercise 2.14 passed~%")

  ;; Exercise 2.15 [*]
  (let ()
    ;; constructors
    (define var-exp
      (lambda (var) var))
    (define lambda-exp
      (lambda (var exp) (list 'lambda (list var) exp)))
    (define app-exp
      (lambda (rator rand)
        (list rator rand)))
    ;; predicates
    (define var-exp?
      (lambda (exp)
        (symbol? (var-exp->var exp))))
    (define lambda-exp?
      (lambda (exp)
        (eqv? (car exp) 'lambda)))
    (define app-exp?
      (lambda (exp)
        (and (exp? (app-exp->rator exp))
             (exp? (app-exp->rand exp)))))
    (define exp?
      (lambda (exp)
        (or (var-exp? exp)
            (lambda-exp? exp)
            (app-exp? exp))))
    ;; extractors
    (define var-exp->var
      (lambda (exp) exp))
    (define lambda-exp->bound-var
      (lambda (exp)
        (car (cadr exp))))
    (define lambda-exp->body
      (lambda (exp)
        (car (cddr exp))))
    (define app-exp->rator
      (lambda (exp)
        (car exp)))
    (define app-exp->rand
      (lambda (exp)
        (cadr exp)))
    (define occurs-free?
      (lambda (search-var exp)
        (cond
          ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
          ((lambda-exp? exp) (and (not (eqv? search-var (lambda-exp->bound-var exp)))
                                  (occurs-free? search-var (lambda-exp->body exp))))
          (else (or (occurs-free? search-var (app-exp->rator exp))
                    (occurs-free? search-var (app-exp->rand exp)))))))
    (equal?? (occurs-free? 'a (var-exp 'a)) #t)
    (equal?? (occurs-free? 'b (var-exp 'a)) #f)
    (equal?? (occurs-free? 'x (lambda-exp (var-exp 'x) (var-exp 'y))) #f)
    (equal?? (occurs-free? 'y (lambda-exp (var-exp 'x) (var-exp 'y))) #t)
    (equal?? (occurs-free? 'x (app-exp (lambda-exp (var-exp 'x) (var-exp 'y)) (var-exp 'x))) #t)
    )
   (eopl:printf "Exercise 2.15 passed~%")

  ;; Exercise 2.16 [*]
  (let ()
    ;; constructors
    (define var-exp
      (lambda (var) var))
    (define lambda-exp
      (lambda (var exp) (list 'lambda var exp)))
    (define app-exp
      (lambda (rator rand)
        (list rator rand)))
    ;; predicates
    (define var-exp?
      (lambda (exp)
        (symbol? (var-exp->var exp))))
    (define lambda-exp?
      (lambda (exp)
        (eqv? (car exp) 'lambda)))
    (define app-exp?
      (lambda (exp)
        (and (exp? (app-exp->rator exp))
             (exp? (app-exp->rand exp)))))
    (define exp?
      (lambda (exp)
        (or (var-exp? exp)
            (lambda-exp? exp)
            (app-exp? exp))))
    ;; extractors
    (define var-exp->var
      (lambda (exp) exp))
    (define lambda-exp->bound-var
      (lambda (exp)
        (cadr exp)))
    (define lambda-exp->body
      (lambda (exp)
        (car (cddr exp))))
    (define app-exp->rator
      (lambda (exp)
        (car exp)))
    (define app-exp->rand
      (lambda (exp)
        (cadr exp)))
    (define occurs-free?
      (lambda (search-var exp)
        (cond
          ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
          ((lambda-exp? exp) (and (not (eqv? search-var (lambda-exp->bound-var exp)))
                                  (occurs-free? search-var (lambda-exp->body exp))))
          (else (or (occurs-free? search-var (app-exp->rator exp))
                    (occurs-free? search-var (app-exp->rand exp)))))))
    (equal?? (occurs-free? 'a (var-exp 'a)) #t)
    (equal?? (occurs-free? 'b (var-exp 'a)) #f)
    (equal?? (occurs-free? 'x (lambda-exp (var-exp 'x) (var-exp 'y))) #f)
    (equal?? (occurs-free? 'y (lambda-exp (var-exp 'x) (var-exp 'y))) #t)
    (equal?? (occurs-free? 'x (app-exp (lambda-exp (var-exp 'x) (var-exp 'y)) (var-exp 'x))) #t)
    )
   (eopl:printf "Exercise 2.16 passed~%")

  ;; Exercise 2.17 [*]
  ;; Lc-exp ::= Identifier
  ;;        ::= (Identifier => Lc-exp)
  ;;        ::= (Lc-exp << Lc-exp)
  (let ()
    ;; constructors
    (define var-exp
      (lambda (var) var))
    (define lambda-exp
      (lambda (var exp)
        (list var '=> exp)))
    (define app-exp
      (lambda (rator rand)
        (list rator '<< rand)))
    ;; predicates
    (define var-exp?
      (lambda (exp)
        (symbol? (var-exp->var exp))))
    (define lambda-exp?
      (lambda (exp)
        (and (not (var-exp? exp))
             (eqv? (cadr exp) '=>))))
    (define app-exp?
      (lambda (exp)
        (and (not (var-exp? exp))
             (eqv? (cadr exp) '>>))))
    ;; extractors
    (define var-exp->var
      (lambda (exp) exp))
    (define lambda-exp->bound-var
      (lambda (exp)
        (car exp)))
    (define lambda-exp->body
      (lambda (exp)
        (car (cddr exp))))
    (define app-exp->rator
      (lambda (exp)
        (car exp)))
    (define app-exp->rand
      (lambda (exp)
        (car (cddr exp))))
    (define occurs-free?
      (lambda (search-var exp)
        (cond
          ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
          ((lambda-exp? exp) (and (not (eqv? search-var (lambda-exp->bound-var exp)))
                                  (occurs-free? search-var (lambda-exp->body exp))))
          (else (or (occurs-free? search-var (app-exp->rator exp))
                    (occurs-free? search-var (app-exp->rand exp)))))))
    (equal?? (occurs-free? 'a (var-exp 'a)) #t)
    (equal?? (occurs-free? 'b (var-exp 'a)) #f)
    (equal?? (occurs-free? 'x (lambda-exp (var-exp 'x) (var-exp 'y))) #f)
    (equal?? (occurs-free? 'y (lambda-exp (var-exp 'x) (var-exp 'y))) #t)
    (equal?? (occurs-free? 'x (app-exp (lambda-exp (var-exp 'x) (var-exp 'y)) (var-exp 'x))) #t)
    )

  ;; Lc-exp ::= (ident Identifier)
  ;;        ::= (lambda Identifier Lc-exp)
  ;;        ::= (app Lc-exp Lc-exp)
  (let ()
    ;; constructors
    (define var-exp
      (lambda (var)
        (list 'ident var)))
    (define lambda-exp
      (lambda (var exp)
        (list 'lambda var exp)))
    (define app-exp
      (lambda (rator rand)
        (list 'app rator rand)))
    ;; predicates
    (define var-exp?
      (lambda (exp)
        (eqv? 'ident (car exp))))
    (define lambda-exp?
      (lambda (exp)
        (eqv? 'lambda (car exp))))
    (define app-exp?
      (lambda (exp)
        (eqv? 'app (car exp))))
    ;; extractors
    (define var-exp->var
      (lambda (exp)
        (cadr exp)))
    (define lambda-exp->bound-var
      (lambda (exp)
        (cadr exp)))
    (define lambda-exp->body
      (lambda (exp)
        (car (cddr exp))))
    (define app-exp->rator
      (lambda (exp)
        (cadr exp)))
    (define app-exp->rand
      (lambda (exp)
        (car (cddr exp))))
    (define occurs-free?
      (lambda (search-var exp)
        (cond
          ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
          ((lambda-exp? exp) (and (not (eqv? search-var (lambda-exp->bound-var exp)))
                                  (occurs-free? search-var (lambda-exp->body exp))))
          (else (or (occurs-free? search-var (app-exp->rator exp))
                    (occurs-free? search-var (app-exp->rand exp)))))))
    (equal?? (occurs-free? 'a (var-exp 'a)) #t)
    (equal?? (occurs-free? 'b (var-exp 'a)) #f)
    (equal?? (occurs-free? 'x (lambda-exp (var-exp 'x) (var-exp 'y))) #f)
    (equal?? (occurs-free? 'y (lambda-exp (var-exp 'x) (var-exp 'y))) #t)
    (equal?? (occurs-free? 'x (app-exp (lambda-exp (var-exp 'x) (var-exp 'y)) (var-exp 'x))) #t)
    )
  (eopl:printf "Exercise 2.17 passed~%")

  ;; Exercise 2.18 [*]
  (let ()
    (define number->sequence
      (lambda (n)
        (list 7 '() '())))
    (define current-element
      (lambda (seq)
        (car seq)))
    (define move-to-left
      (lambda (seq)
        (if (null? (cadr seq))
            (eopl:error 'move-to-left "no left position to move")
            (list (car (cadr seq))
                  (cdr (cadr seq))
                  (cons (car seq) (caddr seq))))))
    (define move-to-right
      (lambda (seq)
        (if (null? (caddr seq))
            (eopl:error 'move-to-right "no right position to move")
            (list (caaddr seq)
                  (cons (car seq) (cadr seq))
                  (cdaddr seq)))))
    (define insert-to-left
      (lambda (n seq)
        (list (car seq)
              (cons n (cadr seq))
              (caddr seq))))
    (define insert-to-right
      (lambda (n seq)
        (list (car seq)
              (cadr seq)
              (cons n (caddr seq)))))

    (equal?? (number->sequence 7) '(7 () ()))
    (equal?? (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)
    (equal?? (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
    (equal?? (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9)))
    (equal?? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9)))
    (equal?? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9)))
    )
  (eopl:printf "Exercise 2.18 passed~%")

  ;; Exercise 2.19 [*]
  (let ()
    (define number->bintree
      (lambda (n)
        (list n '() '())))
    (define at-leaf?
      (lambda (bt)
        (null? bt)))
    (define current-element
      (lambda (bt)
        (if (at-leaf? bt)
            (eopl:error 'current-element "empty binary tree doesn't have current element")
            (car bt))))
    (define move-to-left-son
      (lambda (bt)
        (if (at-leaf? bt)
            (eopl:error 'move-to-left-son "empty binary tree doesn't have son")
            (cadr bt))))
    (define move-to-right-son
      (lambda (bt)
        (if (at-leaf? bt)
            (eopl:error 'move-to-right-son "empty binary tree doesn't have son")
            (caddr bt))))
    (define insert-to-left
      (lambda (n bt)
        (if (at-leaf? bt)
            (eopl:error 'insert-to-left "leaf node can't be insert")
            (list (current-element bt)
                  (list n (move-to-left-son bt) '())
                  (move-to-right-son bt)))))
    (define insert-to-right
      (lambda (n bt)
        (if (at-leaf? bt)
            (eopl:error 'insert-to-right "leaf node can't be insert")
            (list (current-element bt)
                  (move-to-left-son bt)
                  (list n (move-to-right-son bt) '())))))
    (equal?? (number->bintree 13) '(13 () ()))
    (define t1 (insert-to-right 14
                                (insert-to-left 12
                                                (number->bintree 13))))
    (equal?? t1 '(13 (12 () ()) (14 () ())))
    (equal?? (move-to-left-son t1) '(12 () ()))
    (equal?? (move-to-right-son t1) '(14 () ()))
    (equal?? (current-element t1) 13)
    (equal?? (at-leaf? '()) #t)
    (equal?? (at-leaf? t1) #f)
    (equal?? (insert-to-left 15 t1) '(13 (15 (12 () ()) ()) (14 () ())))
    )
  (eopl:printf "Exercise 2.19 passed~%")

  ;; Exercise 2.20 [***]
  ;; Bintree ::= (Node Parent)
  ;; Node ::= () | (Int Node Node)
  ;; Parent ::= () | (left Int Node Parent) | (right Int Node Parent)
  (let ()
    (define get-parent
      (lambda (bt)
        (cadr bt)))
    (define get-left-son
      (lambda (bt)
        (cadr (car bt))))
    (define get-right-son
      (lambda (bt)
        (caddr (car bt))))
    (define insert-to-parent
      (lambda (parent son)
        (cond
          ((eqv? 'left (car parent))
           (list (cadr parent)
                 son
                 (caddr parent)
                 (cadddr parent)))
          ((eqv? 'right (car parent))
           (list (cadr parent)
                 (caddr parent)
                 son
                 (cadddr parent))))))
    (define number->bintree
      (lambda (n)
        (list (list n '() '()) '())))
    (define at-leaf?
      (lambda (bt)
        (null? (car bt))))
    (define at-root?
      (lambda (bt)
        (null? (get-parent bt))))
    (define current-element
      (lambda (bt)
        (if (at-leaf? bt)
            (eopl:error 'current-element "empty binary tree doesn't have current element")
            (caar bt))))
    (define move-to-left-son
      (lambda (bt)
        (if (at-leaf? bt)
            (eopl:error 'move-to-left-son "empty binary tree doesn't have son")
            (list (list
                   (car (get-left-son bt))
                   (cadr (get-left-son bt))
                   (caddr (get-left-son bt)))
                  (list 'left
                        (current-element bt)
                        (get-right-son bt)
                        (get-parent bt))))))
    (define move-to-right-son
      (lambda (bt)
        (if (at-leaf? bt)
            (eopl:error 'move-to-right-son "empty binary tree doesn't have son")
            (list (list
                   (car (get-right-son bt))
                   (cadr (get-right-son bt))
                   (caddr (get-right-son bt)))
                  (list 'right
                        (current-element bt)
                        (get-left-son bt)
                        (get-parent bt))))))
    (define move-up
      (lambda (bt)
        (if (at-root? bt)
            (eopl:error 'move-up "root node can't move up")
            (let ((parent (get-parent bt)))
              (insert-to-parent (get-parent bt) (car bt))))))
    (define insert-to-left
      (lambda (n bt)
        (if (at-leaf? bt)
            (eopl:error 'insert-to-left "leaf node can't be insert")
            (list (list (current-element bt)
                        (list n (get-left-son bt) '())
                        (get-right-son bt))
                  (get-parent bt)))))
    (define insert-to-right
      (lambda (n bt)
        (if (at-leaf? bt)
            (eopl:error 'insert-to-left "leaf node can't be insert")
            (list (list (current-element bt)
                        (get-left-son bt)
                        (list n (get-right-son bt) '()))
                  (get-parent bt)))))
    
    (equal?? (number->bintree 13) '((13 () ()) ()))
    (define t1 (insert-to-right 14
                                (insert-to-left 12
                                                (number->bintree 13))))
    (equal?? t1 '((13 (12 () ()) (14 () ())) ()))
    (equal?? (move-to-left-son t1) '((12 () ()) (left 13 (14 () ()) ())))
    (equal?? (move-to-right-son t1) '((14 () ()) (right 13 (12 () ()) ())))
    (equal?? (current-element t1) 13)
    (equal?? (at-leaf? '(() ())) #t)
    (equal?? (at-leaf? t1) #f)
    (equal?? (insert-to-left 15 t1) '((13 (15 (12 () ()) ()) (14 () ())) ()))
    )
  (eopl:printf "Exercise 2.20 passed~%")

  ;; 2.4 A Tool for Defining Recursive Data Types
  ;; Exercise 2.21 [*]
  (define scheme-value?
      (lambda (val) #t))
  (let ()
    (define-datatype environment environment?
      (empty-env)
      (no-empty-env
       (var symbol?)
       (val scheme-value?)
       (env environment?)))
    (define has-binding?
      (lambda (env search-var)
        (cases environment env
          (empty-env () #f)
          (no-empty-env (var val env)
                        (or (eqv? var search-var)
                            (has-binding? env))))))
    (equal?? (has-binding? (empty-env) 'a) #f)
    (equal?? (has-binding? (no-empty-env 'a 123 (empty-env)) 'a) #t)
    (eopl:printf "Exercise 2.21 passed~%")
    )

  ;; Exercise 2.22 [*]
  (let ()
    ;; (empty-stack) = empty
    ;; (push item [stack]) = [new-stack], where (top [new-stack]) == item and (pop [new-stack]) == [stack]
    ;; (pop [new-stack]) = [stack], reverse operation of push, removed the latest pushed item
    ;; (top [stack]) = item, the latest pushed item
    ;; (empty-stack? [stack])
    ;; constructors: empty-stack, push, pop
    ;; observers: top, empty-stack?
    (define-datatype stack stack?
      (empty-stack)
      (no-empty-stack
       (item scheme-value?)
       (rest stack?)))
    (define push
      (lambda (item stack)
        (no-empty-stack item stack)))
    (define pop
      (lambda (s)
        (cases stack s
          (empty-stack () (eopl:error 'pop "empty stack can't pop"))
          (no-empty-stack (item rest) rest))))
    (define top
      (lambda (s)
        (cases stack s
          (empty-stack () (eopl:error 'pop "empty stack doesn't have top"))
          (no-empty-stack (item rest) item))))
    (define empty-stack?
      (lambda (s)
        (cases stack s
          (empty-stack () #t)
          (no-empty-stack (item rest) #f))))
    (equal?? (pop (push 123 (empty-stack))) (empty-stack))
    (equal?? (top (push 123 (empty-stack))) 123)
    (equal?? (empty-stack? (empty-stack)) #t)
    (equal?? (empty-stack? (push 456 (empty-stack))) #f)
    (eopl:printf "Exercise 2.22 passed~%")
    )

  ;; Exercise 2.23 [*]
  (let ()
    (define identifier?
      (lambda (ident)
        (and (symbol? ident)
             (not (eqv? ident 'lambda)))))
    (define-datatype lc-exp lc-exp?
      (var-exp
       (var identifier?))
      (lambda-exp
       (bound-var identifier?)
       (body lc-exp?))
      (app-exp
       (rator lc-exp?)
       (rand lc-exp?)))
    ;; (var-exp 'lambda)
    (var-exp 'abc)
    (eopl:printf "Exercise 2.23 passed~%")
    )

  (let ()
    ;; Exercise 2.24 [*]
    ;; copied from text book eopl3.
    (define-datatype bintree bintree?
      (leaf-node
       (num integer?))
      (interior-node
       (key symbol?)
       (left bintree?)
       (right bintree?)))
    
    (define bintree-to-list
      (lambda (bt)
        (cases bintree bt
          (leaf-node (num) (list 'leaf-node num))
          (interior-node (key left right)
                         (list 'interior-node
                               key
                               (bintree-to-list left)
                               (bintree-to-list right))))))
    (equal?? (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
             '(interior-node
               a
               (leaf-node 3)
               (leaf-node 4)))
    (eopl:printf "Exercise 2.24 passed~%")

    ;; Exercise 2.25 [**]
    (define get-max
      (lambda (max1 max2)
        (cond
          ((and (eqv? (car max1) 'leaf)
                (eqv? (car max2) 'leaf))
           '())
          ((eqv? (car max1) 'leaf) max2)
          ((eqv? (car max2) 'leaf) max1))))
    (define max-interior0
      (lambda (bt)
        (cases bintree bt
          (leaf-node (num) (cons 'leaf num))
          (interior-node (key left right)
                         (let* ((max-left (max-interior0 left))
                                (max-right (max-interior0 right))
                                (max (get-max max-left max-right)))
                           (cond
                             ((and (not (null? max))
                                   (> (cdr max) (+ (cdr max-left) (cdr max-right))))
                              max)
                             (else (cons key (+ (cdr max-left) (cdr max-right))))))))))
    (define max-interior
      (lambda (bt)
        (car (max-interior0 bt))))
    (define tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3)))
    (define tree-2 (interior-node 'bar (leaf-node -1) tree-1))
    (define tree-3 (interior-node 'baz tree-2 (leaf-node 1)))
    (equal?? (max-interior tree-2) 'foo)
    (equal?? (max-interior tree-3) 'baz)
    (eopl:printf "Exercise 2.25 passed~%")
    )

  ;; Exercise 2.26 [**]
  (let ()
    ;; copied from eopl3 book
    (define list-of
      (lambda (pred)
        (lambda (val)
          (or (null? val)
              (and (pair? val)
                   (pred (car val))
                   ((list-of pred) (cdr val)))))))
    (define-datatype red-blue-tree red-blue-tree?
      (leaf-node
       (num number?))
      (red-node
       (left red-blue-tree?)
       (right red-blue-tree?))
      (blue-node
       ;; 这里用到了list数据结构，牵涉到了实现细节
       (trees (list-of red-blue-tree?))))
    (define build-new-tree0
      (lambda (tree n)
        (cases red-blue-tree tree
          (leaf-node (num) (leaf-node n))
          (red-node (left right)
                    (red-node (build-new-tree0 left (+ n 1))
                              (build-new-tree0 right (+ n 1))))
          (blue-node (trees)
                     (blue-node (map (lambda (tree) (build-new-tree0 tree n)) trees))))))
    (define build-new-tree
      (lambda (tree)
        (build-new-tree0 tree 0)))
    (define tree (red-node (blue-node (list (leaf-node 26) (leaf-node 12)))
                           (red-node (leaf-node 11)
                                     (blue-node (list (leaf-node 117) (leaf-node 14))))))
    ;; (display (build-new-tree tree))
    (eopl:printf "Exercise 2.26 passed~%")
    )

  ;; 2.5 Abstract Syntax and Its Representation
  ;; Exercise 2.27 [*]
  (define ast1
    '(app-exp
      (rator
       (lambda-exp
          (bound-var a)
          (body
           (app-exp
            (rator
             (var-exp
              (var a)))
            (rand
             (var-exp
              (var b)))))))
      (rand
       (var-exp
        (var c)))))
  (define ast2
    '(lambda-exp
      (bound-var x)
      (body
       (lambda-exp
        (bound-var y)
        (body
         (app-exp
          (rator
           (lambda-exp
            (bound-var x)
            (body
             (app-exp
              (rator
               (var-exp
                (var x)))
              (rand
               (var-exp
                (var y)))))))
          (rand
           (var-exp
            (var x)))))))))
  (eopl:printf "Exercise 2.27 passed~%")

  ;; Exercise 2.28 [*]
  (let ()
    ;; copied from Exercise 2.23
    (define identifier?
      (lambda (ident)
        (and (symbol? ident)
             (not (eqv? ident 'lambda)))))
    (define-datatype lc-exp lc-exp?
      (var-exp
       (var identifier?))
      (lambda-exp
       (bound-var identifier?)
       (body lc-exp?))
      (app-exp
       (rator lc-exp?)
       (rand lc-exp?)))
    ;; Lc-exp ::= Identifier
    ;;        ::= proc Identifier => Lc-exp
    ;;        ::= Lc-exp(Lc-exp)
    (define unparse-lc-exp
      (lambda (exp)
        (cases lc-exp exp
          (var-exp (var) (symbol->string var))
          (lambda-exp (bound-var body)
                      (string-append "proc"
                                     " "
                                     (symbol->string bound-var)
                                     " => "
                                     (unparse-lc-exp body)))
          (app-exp (rator rand)
                    (string-append
                     (cases lc-exp rator
                       (var-exp (var) (unparse-lc-exp rator))
                       (lambda-exp (bound-var body)
                                   (string-append "("
                                                  (unparse-lc-exp rator)
                                                  ")"))
                       (app-exp (rator1 rand1)
                                (unparse-lc-exp rator)))
                     "("
                     (unparse-lc-exp rand)
                     ")")))))
    (equal?? (unparse-lc-exp
              (app-exp
               (lambda-exp 'a (app-exp (var-exp 'a) (var-exp 'b)))
               (var-exp 'c)))
             "(proc a => a(b))(c)")
    (eopl:printf "Exercise 2.28 passed~%")
    )

  ;; Exercise 2.29 [*]
  (let ()
    (define list-of
      (lambda (pred)
        (lambda (val)
          (or (null? val)
              (and (pair? val)
                   (pred (car val))
                   ((list-of pred) (cdr val)))))))
    (define identifier?
      (lambda (ident)
        (and (symbol? ident)
             (not (eqv? ident 'lambda)))))
    ;; Lc-exp ::= Identifier
    ;;        ::= (lambda ({Identifier}*) Lc-exp)
    ;;        ::= (Lc-exp {Lc-exp}*)
    (define-datatype lc-exp lc-exp?
      (var-exp
       (var identifier?))
      (lambda-exp
       (bound-vars (list-of identifier?))
       (body lc-exp?))
      (app-exp
       (rator lc-exp?)
       (rands (list-of lc-exp?))))
    (define parse-lc-exp
      (lambda (datum)
        (cond
          ((symbol? datum) (var-exp datum))
          ((pair? datum)
           (if (eqv? (car datum) 'lambda)
               (lambda-exp
                (cadr datum)
                (parse-lc-exp (caddr datum)))
               (app-exp (parse-lc-exp (car datum))
                        (map parse-lc-exp (cdr datum)))))
          (else (eopl:error 'parse-lc-exp "can't parse lc-exp source code")))))
    (define ast (parse-lc-exp '((lambda (a b) (a b)) a b)))
    (equal?? ast (app-exp (lambda-exp (list 'a 'b)
                                      (app-exp (var-exp 'a)
                                               (list (var-exp 'b))))
                          (list (var-exp 'a) (var-exp 'b))))
    (eopl:printf "Exercise 2.29 passed~%")
    )

  ;; Exercise 2.30 [**]
  (let ()
    (define identifier?
      (lambda (ident)
        (and (symbol? ident)
             (not (eqv? ident 'lambda)))))
    (define check-lc-exp
      (lambda (datum)
        (cond
          ((symbol? datum) (check-var-exp datum))
          ((pair? datum)
           (if (eqv? (car datum) 'lambda)
               (check-lambda-exp datum)
               (check-app-exp datum)))
          (else (eopl:error 'check-lc-exp "check-lc-exp can't know datum's syntax: ~s" datum)))))
    (define check-lambda-exp
      (lambda (datum)
        (cond
          ((null? (cdr datum))
           (eopl:error 'check-lambda-exp "lambda-exp should have bound-var and body: ~s" datum))
          ((or (null? (cadr datum)) (not (identifier? (caadr datum))))
           (eopl:error 'check-lambda-exp "lambda-exp should have one bound-var symbol with parentheses wrap: ~s" datum))
          ((or (null? (cddr datum)) (null? (caddr datum)))
           (eopl:error 'check-lambda-exp "lambda-exp should have a non-empty body: ~s" datum))
          (else (check-lc-exp (caddr datum))))))
    (define check-app-exp
      (lambda (datum)
        (cond
          ((null? datum)
           (eopl:error 'check-app-exp "app-exp can't be empty: ~s" datum))
          ((or (null? (car datum)) (null? (cdr datum)) (not (null? (cddr datum))))
           (eopl:error 'check-app-exp "app-exp must have two item: ~s" datum))
          (else (and (check-lc-exp (car datum))
                     (check-lc-exp (cadr datum)))))))
    (define check-var-exp
      (lambda (datum)
        (if (and (identifier? datum) (not (eqv? datum 'lambda)))
            #t
            (eopl:error 'check-var-exp "var-exp must be a invalid symbol except 'lambda: ~s" datum))))
    (define parse-expression
      (lambda (datum)
        (check-lc-exp datum)))
    ;; (parse-expression '())
    ;; (parse-expression '(lambda))
    ;; (parse-expression '(lambda (12)))
    ;; (parse-expression '(lambda (a)))
    ;; (parse-expression '(a))
    ;; (parse-expression '(a b c))
    ;; (parse-expression 'lambda)
    (eopl:printf "Exercise 2.30 passed~%")
    )

  ;; Exercise 2.31 [**]
  (let ()
    (define-datatype prefix-exp prefix-exp?
      (const-exp
       (num integer?))
      (diff-exp
       (operand1 prefix-exp?)
       (operand2 prefix-exp?)))
    (define parse-prefix-exp
      (lambda (ls)
        (cond
          ((null? ls) (eopl:error 'parse-prefix-list "list can't empty"))
          ((integer? (car ls))
           (cons (const-exp (car ls))
                 (cdr ls)))
          ((eqv? '- (car ls))
           (let* ((res1 (parse-prefix-exp (cdr ls)))
                  (res2 (parse-prefix-exp (cdr res1))))
             (cons (diff-exp
                    (car res1)
                    (car res2))
                   (cdr res2)))))))
    (define parse-prefix-list
      (lambda (ls)
        (car (parse-prefix-exp ls))))
    
    (equal?? (parse-prefix-list '(- - 3 2 - 4 - 12 7))
             (diff-exp
              (diff-exp
               (const-exp 3)
               (const-exp 2))
              (diff-exp
               (const-exp 4)
               (diff-exp
                (const-exp 12)
                (const-exp 7)))))
    (eopl:printf "Exercise 2.31 passed~%")
    )
)