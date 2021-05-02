#!r6rs

(import (rnrs))
(define print
  (lambda (msg)
    (display msg)
    (newline)))
(define print-title
  (lambda (msg)
    (newline)
    (print msg)))

;; 约定和假设:
;; `s`      -> a symbol
;; `n`      -> a nonnegative integer
;; `lst`    -> a list
;; `loi`    -> a list of integers
;; `los`    -> a list of symbols
;; `slist`  -> an s-list (S表达式组成的list)
;; `x`      -> any Scheme value

;; Exercise 1.9 [*]
;; remove: Symbol * Listof(Symbol) -> Listof(Symbol)
;; usage: (remove s los) returns a list with the same elements arranged in the same order as `los`, except that all occurrences of the symbol `s` is removed.
(define remove
  (lambda (s los)
    (cond
      ((null? los) '())
      ((eqv? (car los) s) (remove s (cdr los)))
      (else (cons (car los)
                  (remove s (cdr los)))))))
(print-title "Exercise 1.9:")
(print (remove 'a '(a b c d)))
(print (remove 'a '(b c d)))

;; Exercise 1.15 [*]
;; duple: Int * SchemeValue -> Listof(SchemeValue)
;; usage: (duple n x) returns a list containing n copies of x
(define duple
  (lambda (n x)
    (if (eqv? n 0)
        '()
        (cons x (duple (- n 1) x)))))

(print-title "Exercise 1.15:")
(print (duple 2 3))
(print (duple 4 '(ha ha)))
(print (duple 0 '(blah)))

;; Exercise 1.16 [*]
;; invert: Listof(2-list) -> Listof(2-list)
;; usage: (invert lst), where `lst` is a list of 2 lists(list of length two), returns a list with each 2-list reversed
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (cadar lst) (caar lst))
              (invert (cdr lst))))))

(print-title "Exercise 1.16:")
(print (invert '((a 1) (a 2) (1 b) (2 b))))

;; Exercise 1.17 [*]
;; down: List -> List
;; usage: (down lst) wraps parentheses around each top-level element of `lst`.
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst))
               (down (cdr lst))))))

(print-title "Exercise 1.17:")
(print (down '(1 2 3)))
(print (down '((a) (fine) (idea))))
(print (down '(a (more (complicated)) object)))

;; Exercise 1.18 [*]
;; swapper: Symbol * Symbol * Listof(S-exp) -> Listof(S-exp)
;; usage: (swapper s1 s2 slist) returns a list the same as `slist`, but with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons (swapper-s-exp s1 s2 (car slist))
              (swapper s1 s2 (cdr slist))))))
;; swapper-s-exp: Symbol * Symbol * S-exp -> S-exp
;; usage: (swapper-s-exp s1 s2 s-exp) returns a scheme exp the same as `s-exp`, but with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.
(define swapper-s-exp
  (lambda (s1 s2 s-exp)
    (cond
      ((symbol? s-exp)
       (cond
         ((eqv? s-exp s1) s2)
         ((eqv? s-exp s2) s1)
         (else s-exp)))
      (else (swapper s1 s2 s-exp)))))
(print-title "Exercise 1.18:")
(print (swapper 'a 'd '(a b c d)))
(print (swapper 'a 'd '(a d () c d)))
(print (swapper 'x 'y '((x) y (z (x)))))

;; Exercise 1.19 [**]
;; list-set: Listof(SchemeValue) * Int * SchemeValue -> Listof(SchemeValue)
;; usage: (list-set lst n x) returns a list like `lst`, except that the n-th element, using zero-based indexing, is `x`.
(define list-set
  (lambda (lst n x)
    (cond
      ((null? lst) '())
      ((eqv? n 0) (cons x (cdr lst)))
      (else (cons (car lst)
                  (list-set (cdr lst) (- n 1) x))))))
(print-title "Exercise 1.19:")
(print (list-set '(a b c d) 2 '(1 2)))
(print (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3))
(print (list-set '(1) 2 '2))

;; Exercise 1.20 [*]
;; count-occurrences: Symbol * Listof(S-exp) -> Int
;; usage: (count-occurrences s list) returns the number of occurrences of `s` in `slist`.
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-occurrences-s-exp s (car slist))
           (count-occurrences s (cdr slist))))))
;; count-occurrences-s-exp: Symbol * S-exp -> Int
;; usage: (count-occurrences-s-exp s s-exp) returns the number of occurrences of `s` in `s-exp`.
(define count-occurrences-s-exp
  (lambda (s s-exp)
    (if (symbol? s-exp)
        (if (eqv? s-exp s) 1 0)
        (count-occurrences s s-exp))))
(print-title "Exercise 1.20:")
(print (count-occurrences 'x '((f x) y (((x z) x)))))
(print (count-occurrences 'x '((f x) y (((x z) () x)))))
(print (count-occurrences 'w '((f x) y (((x z) x)))))

;; Exercise 1.21 [**]
;; product: Listof(Symbol) * Listof(Symbol) -> Listof(Symbol)
;; usage: (product sos1 sos2), where sos1 and sos2 are each a list of symbols without repetitions, returns a list of 2-lists that represents the Cartesian product of sos1 and sos2. The 2-lists may appear in any order.
(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (product-s (car sos1) sos2 (product (cdr sos1) sos2)))))
;; product-s: Symbol * Listof(Symbol) * List -> List
;; usage: (product-s s sos lst) returns a list of 2-lists which contains `s` and item of `sos`, and cons the `lst`.
(define product-s
  (lambda (s sos lst)
    (if (null? sos)
        lst
        (cons (list s (car sos))
              (product-s s (cdr sos) lst)))))
(print-title "Exercise 1.21:")
(print (product '(a b c) '(x y)))
(print (product '() '(x y)))

;; Exercise 1.22 [**]
;; filter-in: Pred * List -> List
;; usage: (filter-in pred lst) returns the list of those elements in lst that satisfy the predicate pred.
(define filter-in
  (lambda (pred lst)
    (cond
      ((null? lst) '())
      ((pred (car lst))
       (cons (car lst)
             (filter-in pred (cdr lst))))
      (else (filter-in pred (cdr lst))))))
(print-title "Exercise 1.22:")
(print (filter-in number? '(a 2 (1 3) b 7)))
(print (filter-in symbol? '(a (b c) 17 foo)))

;; Exercise 1.23 [**]
;; list-index: Pred * List -> Int|#f
;; usages: (list-index pred lst) returns the 0-based position of the first element of lst that satisfies the predicate pred. If no element of lst satisfies the predicate, then list-index returns #f.
(define list-index
  (lambda (pred lst)
    (list-index-n pred lst 0)))
(define list-index-n
  (lambda (pred lst n)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) n)
      (else (list-index-n pred (cdr lst) (+ n 1))))))

(print-title "Exercise 1.23:")
(print (list-index number? '(a 2 (1 3) b 7)))
(print (list-index symbol? '(a (b c) 17 foo)))
(print (list-index symbol? '(1 2 (a b) 3)))

;; Exercise 1.24 [**]
;; every?: Pred * List -> Bool
;; usage: (every? pred lst) returns #f if any element of lst fails to satisfy pred, and returns #t otherwise.
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (and (pred (car lst))
             (every? pred (cdr lst))))))
(print-title "Exercise 1.24:")
(print (every? number? '(a b c 3 e)))
(print (every? number? '(1 2 3 5 4)))

;; Exercise 1.25 [**]
;; exists?: Pred * List -> Bool
;; (exists?predlst)returns#tifanyelementoflstsatisfies pred, and returns #f otherwise.
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (or (pred (car lst))
            (exists? pred (cdr lst))))))
(print-title "Exercise 1.25:")
(print (exists? number? '(a b c 3 e)))
(print (exists? number? '(a b c d e)))

;; Exercise 1.26 [**]
;; up: List -> List
;; usages: (up lst) removes a pair of parentheses from each top-level element of lst. If a top-level element is not a list, it is included in the result, as is.
(define up
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((symbol? (car lst))
       (cons (car lst)
             (up (cdr lst))))
      (else (up-list (car lst) (up (cdr lst)))))))
(define up-list
  (lambda (lst1 lst)
    (if (null? lst1)
        lst
        (cons (car lst1)
              (up-list (cdr lst1) lst)))))
(print-title "Exercise 1.26:")
(print (up '((1 2) (3 4))))
(print (up '((x (y)) z)))

;; Exercise 1.27 [**]
;; flatten: Listof(S-exp) -> Listof(Symbol)
;; usage: (flatten slist) returns a list of the symbols contained in slist in the order in which they occur when slist is printed. Intuitively, flatten removes all the inner parentheses from its argument.
(define flatten
  (lambda (slist)
    (flatten-slist slist '())))
(define flatten-slist
  (lambda (slist rest)
    (if (null? slist)
        rest
        (flatten-sexp (car slist) (flatten-slist (cdr slist) rest)))))
(define flatten-sexp
  (lambda (sexp rest)
    (if (symbol? sexp)
        (cons sexp rest)
        (flatten-slist sexp rest))))

(print-title "Exercise 1.27:")
(print (flatten '(a b c)))
(print (flatten '((a) () (b ()) () (c))))
(print (flatten '((a b) c (((d)) e))))
(print (flatten '(a b (() (c)))))
      

;; Exercise 1.28 [**]
;; merge: Listof(Int) * Listof(Int) -> Listof(int)
;; usage: (merge loi1 loi2),whereloi1andloi2arelistsofintegers that are sorted in ascending order, returns a sorted list of all the integers in loi1 and loi2.
(define merge
  (lambda (loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((<= (car loi1) (car loi2))
       (cons (car loi1) (merge (cdr loi1) loi2)))
      (else (cons (car loi2) (merge loi1 (cdr loi2)))))))
(print-title "Exercise 1.28:")
(print (merge '(1 4) '(1 2 8)))
(print (merge '(35 62 81 90 91) '(3 83 85 90)))

;; Exercise 1.29 [**]
;; sort: Listof(Int) -> Listof(Int)
;; usage: (sort loi) returns a list of the elements of loi in ascending order.
(define sort
  (lambda (loi)
    (sort0 loi '())))
(define sort0
  (lambda (loi rest)
    (if (null? loi)
        rest
        (sort0 (cdr loi) (merge (list (car loi)) rest)))))

(print-title "Exercise 1.29:")
(print (sort '(8 2 5 2 3)))

;; Exercise 1.30 [**]
;; sort/predicate: Pred * Listof(Int) -> Listof(Int)
;; usage: (sort/predicatepredloi)returnsalistofelementssorted by the predicate.
(define sort/predicate
  (lambda (pred loi)
    (sort/predicate0 pred loi '())))
(define sort/predicate0
  (lambda (pred loi rest)
    (if (null? loi)
        rest
        (sort/predicate0
         pred
         (cdr loi)
         (merge/predicate pred (list (car loi)) rest)))))
(define merge/predicate
  (lambda (pred loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((pred (car loi1) (car loi2))
       (cons (car loi1) (merge/predicate pred (cdr loi1) loi2)))
      (else (cons (car loi2) (merge/predicate pred loi1 (cdr loi2)))))))

(print-title "Exercise 1.30:")
(print (sort/predicate < '(8 2 5 2 3)))
(print (sort/predicate > '(8 2 5 2 3)))

;; Exercise 1.31 [*] Write the following procedures for calculating on a bintree (definition 1.1.7): leaf and interior-node, which build bintrees, leaf?, which tests whether a bintree is a leaf, and lson, rson, and contents-of, which extract the components of a node. contents-of should work on both leaves and interior nodes.

;; leaf: Int -> Int
(define leaf
  (lambda (i)
    i))
;; interior-node: Symbol * Bintree * Bintree -> Bintree
(define interior-node
  (lambda (s lson rson)
    (list s lson rson)))
;; leaf?: Bintree -> Bool
(define leaf?
  (lambda (bt)
    (number? bt)))
;; lson: Bintree -> Bintree
(define lson
  (lambda (bt)
    (if (leaf? bt)
        (raise "leaf not have left son")
        (car (cdr bt)))))
;; rson: Bintree -> Bintree
(define rson
  (lambda (bt)
    (if (leaf? bt)
        (raise "leaf not have right son")
        (car (cdr (cdr bt))))))
;; contents-of: Bintree -> Int
(define contents-of
  (lambda (bt)
    (if (leaf? bt)
        bt
        (car bt))))

(print-title "Exercise 1.31:")
(define leaf1 (leaf 1))
(define leaf2 (leaf 2))
(print leaf1)
(print leaf2)
(define foo (interior-node 'foo leaf1 leaf2))
(print foo)
(define bar (interior-node 'bar leaf1 foo))
(print bar)
(define baz (interior-node 'baz bar (interior-node 'biz (leaf 4) (leaf 5))))
(print baz)

;; Exercise 1.32
;; double-tree: Bintree -> Bintree
;; usage: (double-tree bt) returns a bintree like the original, but with all the integers in the leaves doubled.
(define double-tree
  (lambda (bt)
    (if (leaf? bt)
        (leaf (* 2 bt))
        (interior-node (contents-of bt)
                       (double-tree (lson bt))
                       (double-tree (rson bt))))))

(print-title "Exercise 1.32:")
(print (double-tree baz))

;; Exercise 1.33 [**]
;; mark-leaves-with-red-depth: Bintree -> Bintree
;; usages: (mark-leaves-with-red-depth bt) returns a bintree of the same shape as the original, except that in the new tree, each leaf contains the integer of nodes between it and the root that contain the symbol red.
(define mark-leaves-with-red-depth
  (lambda (bt)
    (mark-leaves-with-red-depth0 bt 0)))
(define mark-leaves-with-red-depth0
  (lambda (bt n)
    (cond
      ((leaf? bt) n)
      ((eqv? (contents-of bt) 'red)
       (interior-node 'red
                      (mark-leaves-with-red-depth0 (lson bt) (+ n 1))
                      (mark-leaves-with-red-depth0 (rson bt) (+ n 1))))
      (else (interior-node (contents-of bt)
                           (mark-leaves-with-red-depth0 (lson bt) n)
                           (mark-leaves-with-red-depth0 (rson bt) n))))))

(print-title "Exercise 1.33:")
(define red
  (interior-node 'red
                 (interior-node 'bar
                                (leaf 26)
                                (leaf 12))
                 (interior-node 'red
                                (leaf 11)
                                (interior-node 'quux
                                               (leaf 117)
                                               (leaf 14)))))
(print (mark-leaves-with-red-depth red))

;; Exercise 1.34 [***]
;; path: BinSearchTree -> Listof(Symbol)
;; Binary-search-tree ::= () | (Int Binary-search-tree Binary-search-tree)
;; usage: (path n bst) returns a list of lefts and rights showing how to find the node containing n. If n is found at the root, it returns the empty list.
(define path
  (lambda (n bst)
    (path0 n bst '())))
(define path0
  (lambda (n bst prefix-path)
    (cond
      ((null? bst) '())
      ((< n (car bst)) (path0 n (car (cdr bst)) (cons prefix-path (list 'left))))
      ((eqv? n (car bst)) (flatten prefix-path))
      ((> n (car bst)) (path0 n (car (cdr (cdr bst))) (cons prefix-path (list 'right)))))))

(print-title "Exercise 1.34:")
(print (path 17 '(14 (7 () (12 () ()))
                     (26 (20 (17 () ())
                             ())
                         (31 () ())))))
(print (path 18 '(14 (7 () (12 () ()))
                     (26 (20 (17 () ())
                             ())
                         (31 () ())))))

;; Exercise 1.35 [***]
;; number-leaves: Bintree -> Bintree
;; usage: (number-leaves bt) returns a bintree like the original, except the contents of the leaves are numbered starting from 0.
(define n -1)
(define number-leaves
  (lambda (bt)
    (if (leaf? bt)
        (begin (set! n (+ n 1)) (leaf n))
        (interior-node (contents-of bt)
                       (number-leaves (lson bt))
                       (number-leaves (rson bt))))))
(print-title "Exercise 1.35:")
(print (number-leaves
        (interior-node 'foo
                       (interior-node 'bar
                                      (leaf 26)
                                      (leaf 12))
                       (interior-node 'baz
                                      (leaf 11)
                                      (interior-node 'quux
                                                     (leaf 117)
                                                     (leaf 14))))))

;; Exercise 1.36 [***]
;; g: List(Int, SchemeVal) * Listof(List(Int, SchemeVal)) -> Listof(List(Int, SchemeVal))
;; (g '(0 a) (g '(0 b) (g '(0 c) '()))) => '((0 a) (1 b) (2 c))
(define g
  (lambda (item rest)
          (cons item (plus-one rest))))
;; plus-one: Listof(List(Int, SchemeVal)) -> Listof(List(Int, SchemeVal))
;; usage: (plus-one lst) returns a list like lst, but all item's first element be plus one.
(define plus-one
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (+ (caar lst) 1)
                    (cdar lst))
              (plus-one (cdr lst))))))
(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(print-title "Exercise 1.36:")
(print (number-elements '(a b c)))

    
