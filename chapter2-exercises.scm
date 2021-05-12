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
  (eopl:printf "Exercise 2.4 passed~%")
  
  
)