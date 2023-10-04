
A Lisp-like language supporting support for closures, lexical scoping, a module system and imports, quasiquoting, and an expressive REPL.

The parser partially supports a form of tail-call elimination, where some statements,
like conditionals, are evaluated without entering a new stack frame in the interpreter.
General tail-call elimination is the goal.

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
;; make-pkg is included in the stdlib (implemented in Lispy + parser convention
;; for sugaring)
(defn my-fib (make-pkg '(list
    (defn fib-1 (fib 1)))))
(asseq my-fib->fib-1 1)

;; higher-order functions
;; map included in stdlib
(defn ls (list 1 2 3))
(defn inc (lambda (i) (+ i 1)))
(asseq (map inc ls) (list 2 3 4))
```

See  `https://github.com/apsz3/lispy/blob/master/tests/test.lsp` for more
Lispy code (these tests are tested in `pytest`)

# Usage

```
> lispy --help
Usage: lispy [OPTIONS] [FILE]

Options:
  -i      Drop into REPL after executing file
  --help  Show this message and exit.
```

# Testing

Run `pytest` in the root directory.

# Notes

Note: the codebase is in the midst of refactoring / cleaning; currently, this repository represents fast-moving experimentation, and to this end, I have implemented Lispy in Python, which is more convenient to work with due to its robust builtin types, than more efficient languages like C.

# TODO

## Cons / '() fixups
```txt
# TODO: transition from Exprs to box-and-pointer model...
# We want to be able to do (hd '( 1 2 )) and not get an expr error.
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

## Quasiquoting symbols

Need to handle cases where a symbol is directly quasiquoted and unquoted

```
(lisp)> (defn x `(+ 1 2))
(lisp)> (print `,x)
,x
```