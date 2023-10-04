
A Lisp-like language supporting support for closures, lexical scoping, a module system and imports, quasiquoting, and an expressive nice REPL.

```lisp
;; (include ./std.lsp) -- This is imported by default by the interpreter

(defn fib (lambda (n)
    (if (= n 0) 0
    (if (= n 1) 1
    (+ (fib (- n 1)) (fib (- n 2)))))))
(asseq (fib 10) 55) ;; assert-equals; stdlib

;; quasiquoting
(defn n 10)
(defn fib-10 `(fib ,n))
(asseq (eval (fib-10)) 55)

;; modules
;; make-pkg is included in the stdlib (implemented in Lispy + parser convention)
(defn my-fib (make-pkg '(list
    (defn fib-1 (fib 1)))))
(asseq my-fib->fib-1 1)

;; higher-order functions
;; map included in stdlib (implemented in Lispy)
(defn ls (list 1 2 3))
(defn inc (lambda (i) (+ i 1)))
(print map inc ls)
```
Warning: the implementations here aren't pretty, and have known bugs. I've put them on Git specifically for my application here. First, they represent fast-moving experimentation; to this end, they are written in Python, my primary language, which conveniently has robust builtin types, compared to C. Next, they could certainly be designed more efficiently with better practices. However, I am generally happy with the features accomplished thus-far.

```
(py310) ➜  lispy git:(master) ✗ lispy --help
Usage: lispy [OPTIONS] [FILE]

Options:
  -i      Drop into REPL after executing file
  --help  Show this message and exit.
```


https://github.com/apsz3/lispy/blob/master/tests/test.lsp

# TODO:

```txt
###########
# Testing #
###########

# TODO: transition from Exprs to box-and-pointer model...
# We want to be able to do (hd '( 1 2 )) and not get an expr error.


# TODO: wtf?
# (lisp)> (cons 1 '())
# $ (1, None)
# (lisp)> (cons 1 ())
# $ (1, None)
# (lisp)> (tl (cons 1 ()))
# (lisp)> (tl (cons 1 '()))
# $ None
# (lisp)>

# (eq (tl (cons 1 ())) '()) -> ERROR NoneType has no attribute `.s` (for comparison)
# However, '() is a symbol, so we can do
# (eq (tl (cons 1 '())) '()) -> True
```