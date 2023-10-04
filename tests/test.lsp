(include "./std.lsp")
;; basics
(asseq (add 1 2) 3)
    ;; fns are first class, so we've aliased "add" to "+"
    (asseq (+ 1 2) 3)
(asseq (sub 1 2) -1)
(asseq (hd (cons 1 (cons 2 '()))) 1)
(asseq (tl (cons 1 (cons 2 '()))) (cons 2 '()))

;; imports / includes
;;(include "./tests/std.lsp") -- it is autoloaded
;; `inc` is defined in `std.lsp`, as is `asseq` (assert equals)
(asseq (inc 5) 6)

;; compute fib
(defn fib (lambda (n)
    (if (eq 0 n) 0
    (if (eq 1 n) 1
    (add (fib (sub n 1)) (fib (sub n 2)))))))
(asseq (fib 5) 5)
(asseq (fib 10) 55)

;; demonstrate invalid value of fib with newly defined fn
(defn assneq (lambda (expected actual)
    (ass (not (eq expected actual)))))
(assneq (fib 5) 55)

;; define map()
(defn map2 (lambda (fn ls acc)
    (if (empty? ls) acc
    (map2 fn (tl ls) (cons acc (fn (hd ls)))))))
;; increment a list with `inc` loaded from `std`
(defn ls (list 1 2 3))
(defn incd-ls (map2 inc ls '()))
(asseq incd-ls (cons (cons (cons '() 2) 3) 4))

;; Note: a symbol can be defined to be either a lambda, a constant,
;; or a series of `defns` followed optionally by an expression.
;; In this case, the value of the symbol is what the final expression evaluates to

;; lexical scoping
(defn a 0)
(defn outer
    (defn a 1)
    (add a 1))
(asseq a 0)
(asseq outer 2)

;; closures
(defn make-adder (lambda (n)
    (lambda (x) (add n x))))

(defn add-two (make-adder 2))
(asseq (add-two 2) 4)
(assneq (add-two 2) 3)

;; We can create a an outer `defn`,
;; and then using inner `defn`s which have local scope,
;; with the final expression
;; being what is returned when the outer symbol is evaluated
(defn life
    (defn doc "Docstring")
    (defn LIFE 42)
    (defn the-meaning-of-life (lambda () LIFE))
    (defn the-meaning-of-life-inc (inc (the-meaning-of-life)))
    the-meaning-of-life-inc)
(asseq life 43)

;; We can do something more sophisticated by leveraging
;; the final expression, to be a function that lets us
;; access the inner environment.
;; This creates a notion of a namespace, in that
;; we can access scoped member variables and values
(defn life
    (defn doc "Docstring")
    (defn LIFE 42)
    (defn the-meaning-of-life (lambda () LIFE))
    (lambda (method) (eval method)))

(asseq (life 'doc) "Docstring")
;; With syntactic sugar, we can do
(asseq life->doc "Docstring")
;; Note: We can have the sugar call a proper method, at some point,
;; rather than this arbitrary process currently of $a->$b => ($a '$b)


;; We make a `math` package which includes
;; a procedure `min` which overrides the one in the stdlib
;; in the outser scope
(defn math (make-pkg '(list
    (defn min (lambda (a b) -10))
    (defn pow (lambda (x n)
        ;; computes x^n
        (if (eq n 0) 1
        (mul x (pow x (sub n 1))))))
)))

(assneq (min 1 2) (math->min 1 2))
(asseq (min 1 2) 1)
(asseq (math->min 1 2) -10)
(asseq (math->pow 2 8) 256)

;; We can nest scope

(defn outer (make-pkg '(list
    (defn inner (make-pkg '(list
        (defn x 1)
    ))))))

(asseq outer->inner->x 1)

;; Quasiquotes
(asseq `1 (quasiquote 1))
(asseq `(+ 1 2) (quasiquote (+ 1 2)))
(asseq (eval `2) 2)
(asseq (eval `(+ 1 ,(+ 2 3))) 6)
(asseq (eval `(+ 1 `(+ ,(+ 1 1) ,(+ 2 3)))) 8)


;; Quasiquoting gives us closures
(defn close-over 2)
(defn x `(fib ,close-over))
(asseq (eval x) 1)
(print "All tests passed.")