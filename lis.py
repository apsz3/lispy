#!/usr/bin/env python3

import operator as op
import sys
import traceback
from dataclasses import dataclass
from enum import Enum, auto
from hashlib import new
from pathlib import Path
from typing import Any, List, Union
from unittest.mock import ANY

import click
from prompt_toolkit import PromptSession, print_formatted_text, prompt
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory
from prompt_toolkit.completion import WordCompleter
from prompt_toolkit.history import FileHistory


# -------- Primitive types
class Symbol:
    def __init__(self, s):
        self.s = s

    def __repr__(self):
        return f"${self.s}"

    def __str__(self):
        return self.s

    def __hash__(self):
        return hash(self.s)

    def __eq__(self, x):  # Have to implement for comparison, like key lookup.
        try:
            return self.s == x.s
        except:
            return False


class Atom:
    def __init__(self, s):
        if s is None:
            self.s = None
        elif isinstance(s, Token):
            self.s = s
        elif s.startswith('"'):
            self.s = s[1:-1]  # Quote chars not part of the string literal
        else:
            if s == "#true":
                self.s = True
            elif s == "#false":
                self.s = False
            else:
                self.s = int(s)

    # When using a non-standard __repr__,
    # you need to define __eq__ for proper equality checks;
    # because without an __eq__, it uses __repr__
    def __repr__(self):
        return repr(self.s)

    def __eq__(self, other):

        return self.s == other.s


NULL = Atom(None)


def ass(x):
    assert x


def JOLLA_EQ(a, b):
    try:
        return a == b
    except:
        return False  # Comapring different types gets hairy.
        # At the moment, the main issue is comparing a list element, to '().
        # TODO: resolve.


class Expr:
    def __init__(self, op, *operands):
        self.op = op
        self.operands = operands  # Can be []

    def __eq__(self, other):
        return self.op == other.op and self.operands == other.operands

    def __repr__(self):
        return "{" + str(self.op) + "{" + " ".join(map(str, self.operands)) + "}" + "}"


# An Expr is what gets inputted to the Eval.
# You have primitive Exprs -- numbers, symbols, strings.
# You also have compound Exprs -- procedure application.


class Env:
    def __init__(self, outer):
        self.inner: dict = {}
        self.outer: "Env" = outer

    def find(self, k):
        if k in self.inner:
            return self.inner[k]
        else:

            try:
                return self.outer.find(k)
            except Exception as exc:
                # Symbol missing is missing, eventually, from even
                # the global environment -- self.outer.None.find() will be called.
                raise KeyError(f"Cannot find symbol {k} in any environment.")


GLOBAL_ENV = Env(None)


@dataclass
class BoxPtr:
    box: Any
    ptr: Union[Any, "BoxPtr"]

    def __str__(self):
        return f"({self.box}, {self.ptr})"

    @classmethod
    def from_python_tuple(cls, *args):
        # We use this construction to convert
        # variadic args into a representable list.
        hd = cls(NULL, NULL)
        cur = hd
        for arg in args:

            if JOLLA_EQ(cur.box, NULL):
                cur.box = arg
            else:
                cur.ptr = cls(arg, NULL)
                cur = cur.ptr
        return hd


@dataclass
class Procedure:
    bound_params: Expr
    body: Expr
    env: Env

    def __call__(self, actual_params):
        # Struct Interp of Prog pg 241
        frame = Env(self.env)
        frame.inner = {k: v for k, v in zip(self.bound_params, actual_params)}
        return Eval(self.body, frame)


# ----------- Parsing

COMMENT_CHAR = ";;"


def balancedparens(s: str) -> List[str]:
    expts, tally = [], []
    stack = 0
    for c in s:
        if c == ")":
            tally.append(")")
            stack -= 1
            if stack == 0:
                expts.append("".join(tally))
                tally.clear()
        elif c == "(":
            stack += 1
            tally.append("(")
        else:
            tally.append(c)
    return expts


def strip_comments(s) -> str:
    # All comments begin somewhere with `--` and end in a \n.
    # This will exclude things entered in the REPL.
    # TODO. Fix that.
    splits = s.split("\n")
    filtered = []
    for splt in splits:
        if COMMENT_CHAR in splt:  # TODO: Won't handle if its quoted...
            splt = splt.strip()  # so we can indent the comments
            if not splt.startswith(COMMENT_CHAR):
                # this is a comment following code on the same line.
                filtered.append(splt.split(COMMENT_CHAR)[0])
        else:
            filtered.append(splt)
    return "\n".join(filtered)


class Token(Enum):
    SPACE = " "

    def __str__(self):
        return self.value


def parse(expr: str):
    # Expr is a single balanced / well-formed expression.
    # We do not handle a sequence of expressions here.
    if "(" not in expr and ")" not in expr:
        # TODO: this is a hacky parsing method.
        # For example, a string with spaces parses as a whole...
        # Literals should ideally be considered 'expressions' too.
        # Then, just set their op to their val, and no operands.
        if expr.startswith("'"):
            expr = expr[1:]
            try:
                val = Atom(expr)
            except:
                val = Symbol(expr)
            return Expr(Symbol("quote"), val)
        else:
            # A symbol or a literal
            try:
                val = Atom(expr)
            except:
                val = Symbol(expr)
            return val

    expr = expr.replace("(", " ( ").replace(")", " ) ")
    expr = expr.replace("\\n", "\n")  # raw char encoding -> proper encoding

    def lex(s: str):
        """
        "(lambda (thing) arg)"
        -> ['(', 'lambda', '(', 'thing', ')', '(', 'fn', 'arg', ')', ')']
        """
        tokens = []
        string_joiner = []
        in_quote = False  # Double quotes, for strings.
        for char in s.strip():
            if char == "(":
                tokens.append(char)
            elif char == ")":
                # Closing an expr; concatenate the loose tokens
                if string_joiner:
                    joined = "".join(string_joiner)
                    tokens.append(joined)
                    string_joiner.clear()
                tokens.append(char)
            elif char == '"':
                in_quote = not in_quote
                # We rely on still appending the character,
                # because Atom(<str>) will cut off the leading/
                # trailing `"` characters. This isn't ideal for now; TODO.
                string_joiner.append(char)
            elif char == " ":
                if in_quote:
                    string_joiner.append(Token.SPACE)
                else:
                    # (foo (...) (...)) -- the middle space
                    # if we are at the space char between
                    # the (...), string_joiner is empty:
                    # and we don't want to append the empty string
                    # to the tokens list. Hence, this check:
                    if string_joiner:
                        # Convert tokens back to their str-values.
                        joined = "".join(map(str, string_joiner))
                        tokens.append(joined)
                        string_joiner.clear()
            elif char == "\n":
                if in_quote:
                    string_joiner.append("\n")
                else:
                    continue  # We don't care about it for now.
            else:
                string_joiner.append(char)
        return tokens

    lexed = lex(expr)

    def _rewrite(stack):
        # apply syntactic sugar transforms
        popped = stack.pop()
        if popped == []:  # Consecutive ()
            res = NULL
            if len(stack) != 0 and isinstance(stack[-1], str) and stack[-1] == "'":
                res = Expr(Symbol("quote"), res)  # not an Expr(res) here.
                stack.pop()  # get rid of the `'`.
        else:
            expr = Expr(*popped)
            # Whenever the token before
            # an expression we've finished gathering
            # is `'`, it means that we've quoted the expr.
            # Also, do the string type check, so that we don't accidentally
            # get an Atom("'") == "'".
            if len(stack) != 0 and isinstance(stack[-1], str) and stack[-1] == "'":
                # NOTE/TODO: sending `*` as the OPERANDS to an Expr
                # will pack it as a TUPLE.
                res = Expr(Symbol("quote"), expr)
                stack.pop()  # get rid of the `'` now.
            else:
                res = expr
        return res

    def _parse(toks: List[str]):
        stack = []
        # ' before parens / outside expr is an individual token.
        # If its quoting an atom or a symbol, it prepends those characters.
        for tok in toks:
            # TODO: the issue with x->y not parsing
            # without being in () is because we only rewrite
            # expressions...
            if tok == "(":
                stack.append([])
            elif tok == ")":
                # The only allowable situation for `()` is
                # for a lambda fn with no inputs;
                # otherwise, it is the empty expression;
                # and when quoted, it's None
                res = _rewrite(stack)
                if len(stack) == 0:  # S-expr popped.
                    return res
                else:
                    stack[-1].append(res)  # Append a sub-expression to the parent

            elif tok.startswith("'") and tok != "'":
                # Handle an individually quoted element
                # within an expression.
                # It will be an OPERAND; this is not quoting an expression.
                # That is handled implicitly by appending it as a string,
                # and then processing it on popping an expr.
                tok = tok[1:]
                try:
                    val = Atom(tok)
                except:

                    val = Symbol(tok)
                stack[-1].append(Expr(Symbol("quote"), val))
            else:
                if tok == "'":
                    stack.append(tok)
                else:
                    try:
                        val = Atom(tok)
                    except:
                        if "->" in tok:
                            refs = tok.split("->")
                            # (pkg-name->method *args) EXPANDS TO ((pkg-name 'method) *args)
                            # which is currently our idiom for accessing smth.
                            hd = Expr(
                                Symbol(refs[0]), Expr(Symbol("quote"), Symbol(refs[1]))
                            )
                            val = hd
                        else:
                            val = Symbol(tok)
                    stack[-1].append(val)  # Normal case.
        return stack

    return _parse(lexed)


###########
# Runtime #
###########


def include(path: str, env):
    p = Path(path)
    if not p.is_absolute():
        p = Path.cwd() / path
    path = Path(p).resolve()
    exprs = loadfile(str(path))
    _execd = [Exec(expr, env) for expr in exprs]
    return _execd[-1]


BUILTINS = {
    Symbol("add"): op.add,
    Symbol("not"): op.not_,
    Symbol("eq"): JOLLA_EQ,
    Symbol("gt"): op.gt,
    Symbol("lt"): op.lt,
    Symbol("sub"): op.sub,
    Symbol("mul"): op.mul,
    Symbol("div"): op.truediv,
    Symbol("quine"): lambda: "quine",
    Symbol("ass"): ass,
    Symbol("print"): print,
    # no matter the inputs, return a flattened list.
    # Symbol("merge"): lambda a,b: \
    #     (a if type(a) is list else [a])
    #     + (b if type(b) is list else [b]),
    Symbol("cons"): lambda a, b: BoxPtr(a, b),
    Symbol("hd"): lambda a: a.box,
    Symbol("tl"): lambda a: a.ptr,
    Symbol("list"): lambda *args: BoxPtr.from_python_tuple(*args),
}

GLOBAL_ENV.inner = BUILTINS


def Eval(x, env):
    # Expects a parsed input.
    if isinstance(x, Atom):
        return x.s
    elif isinstance(x, Symbol):
        return env.find(x)
    assert isinstance(x, Expr)
    # Have to check for nested Expr first,
    # because the following conditionals
    # expect expr.op to be a Symbol.
    if isinstance(x.op, Expr):
        # This is the case of an APPLICATION
        # of a lambda.
        # First, evaluate the lambda to get the procedure object.
        # Also evaluate then the arguments to the lambda
        # Then, call the procedure on these arguments.
        # Note: assert x.op.op == Symbol("lambda") won't always work.
        # What if you have a lambda, returning a lambda?
        return Eval(x.op, env)([Eval(var, env) for var in x.operands])

    # Do not evaluate; return it literally
    if x.op == Symbol("quote"):
        # TODO: this is annoying, having to do this distinguishing.
        # We should make an internal representation which is just cons/car/cdr,
        # so we don't have to run into this sort of thing, where even singletons
        # in our implementation, are lists.
        if len(x.operands) == 1:
            return x.operands[0]
        return x.operands
    elif x.op == Symbol("begin"):
        return [Eval(expr, env) for expr in x.operands][-1]
    # Conditional
    elif x.op == Symbol("if"):
        cond, branch_true, branch_false = x.operands
        return Eval(branch_true, env) if Eval(cond, env) else Eval(branch_false, env)
    elif x.op == Symbol("defn"):
        # Instead of just evaluating the 'first' expression;
        # we evaluate all of them, and bind the name
        # to the result of the last evaluated expresion.
        # This lets us do nested definitions.
        nested_env = Env(env)
        procedure = [Eval(i, nested_env) for i in x.operands[1:]][-1]
        env.inner[x.operands[0]] = procedure
        return procedure
    elif x.op == Symbol("lambda"):
        # The evaluation of a lambda creates a procedure.
        # (lambda (<params>) <expr|atom|symbol>)
        params, body = x.operands
        if params is NULL:  # Empty parameter list.
            return Procedure([], body, env)
        else:
            # Hack the Expr() syntax here to collect our
            # paramters; (a b c) will have `a` as the expr op.
            params = [params.op, *params.operands]
            return Procedure(params, body, env)
    elif x.op == Symbol("include"):
        # Load the definitions and evaluate them in the CURRENT
        # environment.
        return include(Eval(x.operands[0], env), env)
    elif x.op == Symbol("eval"):
        return Eval(Eval(*x.operands, env), env)  # Eval(Eval(x.operands, env), env)
    else:
        # Primitive operations here -- add, etc. CPU crap.
        op = Eval(x.op, env)
        operands = [Eval(o, env) for o in x.operands]
        if isinstance(op, Procedure):  # Hack because it expects a list.
            # for binding.
            return op(operands)
        return op(
            *operands
        )  # But builtins don't operate on lists; we unpack them instead.


###########
# Testing #
###########

# TODO: transition from Exprs to box-and-pointer model...
# We want to be able to do (hd '( 1 2 )) and not get an expr error.

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


def test():
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
    print("=== UNIT TESTS PASSED ===")
    p = str(Path(__file__).parent) + "/tests/test.lsp"
    print(f"... Running program {p} ...")
    exprs = loadfile(p)
    # Clean environment to avoid horribly pernicious bugs between
    # unit tests in the different file, and these.
    _new_global = Env(None)
    _new_global.inner = BUILTINS
    for expr in exprs:
        res = Exec(expr, _new_global)
        print(expr, "=>", res)
    print("=== PROGRAM TESTS PASSED ===")


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


#################
# Interactivity #
#################


def Exec(s, env=GLOBAL_ENV):
    return Eval(parse(s), env)


def loadfile(path) -> List[Expr]:
    with open(path, "r") as fp:
        code = fp.read()
        code = strip_comments(code)
        exprs = balancedparens(code)
    return exprs


def REPL(env=GLOBAL_ENV):
    session = PromptSession(FileHistory("~/.lispy"))
    # Builtins we don't define in symbol table, that we want
    # to show up in repl completer
    extend_complete = ["defn", "quote", "eval", "list"]
    while True:
        # Update it with new symbols
        lisp_completer = WordCompleter(
            list(map(str, env.inner.keys())) + extend_complete
        )
        text = session.prompt(
            "(lisp)> ",
            completer=lisp_completer,
            auto_suggest=AutoSuggestFromHistory(),
            complete_while_typing=True,
        )
        text = strip_comments(text)
        exprs = balancedparens(text)  # Multiline-pasting into REPL needs to be handled.

        for expr in exprs:
            try:
                parsed = parse(expr)
            except SyntaxError as exc:
                print_formatted_text(f"<Syntax error> {exc}")
                continue

            try:
                res = Eval(parsed, env)
                if res is not None:
                    print_formatted_text(res)
            except KeyError as exc:
                traceback_str = "".join(traceback.format_tb(exc.__traceback__))
                print(traceback_str)
                print(f"<Evaluation error> Undefined ``{exc} in {parsed}''")
            except AssertionError as exc:
                traceback_str = "".join(traceback.format_tb(exc.__traceback__))
                print(traceback_str)
                print(f"<Assertion error> {exc} in {parsed}")
            except Exception as exc:
                traceback_str = "".join(traceback.format_tb(exc.__traceback__))
                print(traceback_str)
                print(f"<Evaluation error> {exc}:{exc} in {parsed}")


@click.command()
@click.argument("file", required=False)
@click.option(
    "-i", is_flag=True, required=False, help="Drop into REPL after executing file"
)
# TODO allow specifying an include path, a directory to prefix all
# relative includes too...
def main(file, i):
    std = loadfile(Path(__file__).parent / "tests/std.lsp")
    [Exec(expr) for expr in std]
    if file:
        if file == "test":
            test()
            return
        exprs = loadfile(file)
        [Exec(expr) for expr in exprs]
        if i:
            REPL()
    else:
        REPL()


if __name__ == "__main__":
    main()
