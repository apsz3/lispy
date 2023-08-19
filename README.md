
Inspired by Peter Norvig's Lispy tutorial, with help from SICP, I implemented a Lisp-like language. Beyond the basics (like control flow), I added support for strings, lexical scoping, a module system, some syntactic sugar, and a nice REPL.

Warning: the implementations here aren't pretty, and have known bugs. I've put them on Git specifically for my application here. First, they represent fast-moving experimentation; to this end, they are written in Python, my primary language, which conveniently has robust builtin types, compared to C. Next, they could certainly be designed more efficiently with better practices. However, I am generally happy with the features accomplished thus-far.

```
(py310) ➜  lispy git:(master) ✗ python3 lis.py --help
Usage: lis.py [OPTIONS] [FILE]

Options:
  -i      Drop into REPL after executing file
  --help  Show this message and exit.
```

If the `file` argument is `test`, run test suite:

`python3 lis.py test`

The below code is copied from `tests/test.lsp`

```lisp
;; imports / includes
(include "./tests/std.lsp")
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

;; Getting higher-level, we can make
;; a procedure that will automatically
;; give us this ending lambda method.
;; Note the use of `begin` here to let us
;; `eval` into the environment
(defn make-pkg
    (lambda (body)
    (begin
        (eval body)
        (lambda (method) (eval method)))))

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
```