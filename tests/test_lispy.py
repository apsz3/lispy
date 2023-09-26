from unittest.mock import ANY

from lispy import *

tests = [
    ("1", 1),  # -> int
    ("()", None),
    ('"a"', "a"),  # -> str,
    ("#true", True),  # -> bool
    ("add", op.add),  # -> procedure
    ("(add 1 1)", 2),  # -> int
    ("(add 1 (add (add 1 1) 1))", 4),  # -> 4
    ("(add (add 1 1) (add 1 1))", 4),  # -> 4
    ("quine", ANY),  # -> procedure
    ("(quine)", "quine"),  # -> "quine"
    ("(if #true 1 2)", 1),
    ("(if #false 1 2)", 2),
    ("(if (not #true) 1 2)", 2),
    ('(if (gt (add 1 2) 0) (add (if #true (add 1 -1) 1) 3) "fals")', 3),  # => 3
    (
        '(if (gt (add 1 2) 4) (add (if #true (add 1 -1) 1) 3) "fals")',
        "fals",
    ),  # => "fals",
    ("(eval '(add 1 2))", 3),
]
list_tests = [
    ("(cons 1 2)", BoxPtr(1, 2)),
    ("(cons (cons 1 2) 3)", BoxPtr((BoxPtr(1, 2)), 3)),
    # The list construction should be equivalent to Cons.
    # Unfortunately, for now we don't have any way of representing variadic
    # arguments to procedures, so we have to build `list` into our interpreter.
    # TODO.
    ("(list 1 2 3)", BoxPtr(1, BoxPtr(2, BoxPtr(3, NULL)))),
    ("(cons 1 (cons 2 (cons 3 '())))", BoxPtr(1, BoxPtr(2, BoxPtr(3, NULL)))),
    # ("(print (eq '() (tl (tl (list 1 2))))", True)
]


# lambda <symbols to bind; not a native datatype or expression>
# <thing you will substitute bound symbols into and EVALUATE>
# "(lambda (thing) arg)", # -> procedure
# "(lambda (thing) (fn arg))", # -> procedure
# "( (lambda (thing) (add 1 thing)) 1 )" # -> 2
advanced_tests = [
    ("(lambda (x y) 1)", Procedure),
    ("((lambda (x y) 1))", 1),
    ("(lambda (x y) (add x y))", Procedure),
    ("(lambda () 1)", Procedure),
    ("((lambda () 1))", 1),
    ("(lambda () ())", Procedure),
    ("((lambda () ()))", None),
    ("()", None),
    ("'()", Symbol(None)),
    ("(quote 3)", Symbol(3)),
    ("(quote add)", Symbol("add")),
    (
        "(quote (cons (cons 1 2) 3))",
        Expr(
            Symbol("cons"),
            Expr(
                Symbol("cons"), Atom("1"), Atom("2")
            ),  # Notice how we don't send a tuple in. this is because expr takes *args. TODO -- fix / standardize this arg passing.
            Atom("3"),
        ),
    ),
    ("(quote (cons 1 2))", Expr(Symbol("cons"), Atom("1"), Atom("2"))),
    ("'(cons 1 2)", Expr(Symbol("cons"), Atom("1"), Atom("2"))),
    (
        "'(cons (cons 1 2) 3))",
        Expr(
            Symbol("cons"),
            Expr(
                Symbol("cons"), Atom("1"), Atom("2")
            ),  # Notice how we don't send a tuple in. this is because expr takes *args. TODO -- fix / standardize this arg passing.
            Atom("3"),
        ),
    ),
    ("( (lambda (x y) (add x y)) 1 2)", 3),
]

more_elaborate = [
    (
        """(((lambda (x) (if (eq x 1) (lambda (y) y) x)) 1) 5)
""",
        5,
    ),
    ("((lambda()123))", 123),
    ("(defn fizz (lambda (x y) (add x y)))", Procedure),
    ("(add 3 (fizz 1 1))", 5),
    (
        "(defn fib (lambda (n) (if (eq n 0) 0 (if (eq n 1) 1 (add (fib (sub n 1)) (fib (sub n 2)))))))",
        Procedure,
    ),
    ("(fib 10)", 55),
    ("(defn bz (lambda () 3))", Procedure),
    ("(bz)", 3),
    ("(defn bz 4)", 4),
    ("(add bz 4)", 8),
    (
        """
(defn fibMultiLine (lambda (n)
    (if (eq n 0) 0
    (if (eq n 1) 1
    (add
        (fib (sub n 1)) (fib (sub n 2)))))))""",
        Procedure,
    ),
    ("(fibMultiLine 10)", 55),
    (
        """
(defn first
    (lambda ()
        (lambda ()
            (lambda () "last")))))
""",
        Procedure,
    ),
    ("(((first)))", "last"),
]

nesting = [
    ("(defn outer (defn inner 3) (defn fff 4))", 4),
    ("(defn outer (defn inner 3) (defn fff 4) 5)", 5),
    ("(defn outer (defn inner 3) (defn fff 4) (add inner fff))", 7),
    ("(defn outer (defn inner 6) (defn fff 4) (add inner fff))", 10),
    (
        "(defn inc (lambda (x) ( (defn dec (lambda (y) (add x y))) (dec -1) )))",
        Procedure,
    ),
]

strs = [
    ('(defn hello-world "Hello, world!") hello-world', "Hello, world!"),
    ('(defn hello-world "Hello,\n\n world!\n") hello-world', "Hello,\n\n world!\n"),
]


def test_lispy():
    num_unit, num_bootstrapped = 0, 0
    for case, expected in (
        tests + list_tests + advanced_tests + more_elaborate + nesting + strs
    ):
        res = Exec(case)
        print(parse(case), "=> ", res)
        try:
            if expected is Procedure:
                assert isinstance(res, Procedure)
            else:
                assert res == expected
        except AssertionError as exc:
            raise Exception(
                f"Test error: actual result {res} failed against expected {expected} "
            )
    print("=== PARSING TESTS PASSED ===")
    p = str(Path(__file__).parent) + "/test.lsp"
    print(f"... Running program {p} ...")
    exprs = loadfile(p)
    # Clean environment to avoid horribly pernicious bugs between
    # unit tests in the different file, and these.
    _new_global = Env(None)
    _new_global.inner = BUILTINS
    for expr in exprs:
        res = Exec(expr, _new_global)
        print(expr, "=>", res)
    print("=== RUNTIME TESTS PASSED ===")
