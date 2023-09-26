#!/usr/bin/env python3

import operator as op
import traceback
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any, List, Union


# -------- Primitive types
class Symbol:
    def __init__(self, s):
        self.s = s

    def __repr__(self):
        return self.s

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


class Expr:
    def __init__(self, op, *operands):
        self.op = op
        self.operands = operands  # Can be []

    def __eq__(self, other):
        return self.op == other.op and self.operands == other.operands

    def __repr__(self):
        return f"({str(self.op)} " + " ".join(map(str, self.operands)) + ")"


# An Expr is what gets inputted to the Eval.
# You have primitive Exprs -- numbers, symbols, strings.
# You also have compound Exprs -- procedure application.


class Env:
    def __init__(self, outer):
        self.inner: dict = {}
        self.outer: "Env" = outer

    def __repr__(self):
        return str(self.inner)

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
            if IS_EQ(cur.box, NULL):
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

    def __repr__(self):
        return str(self.bound_params) + " -> " + str(self.body)


def ass(x):
    assert x


def IS_EQ(a, b):
    try:
        return a == b
    except:
        return False  # Comparing different types gets hairy.
        # At the moment, the main issue is comparing a list element, to '().
        # TODO: resolve.


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


def atom_or_symbol(val):
    try:
        val = Atom(val)
    except:
        val = Symbol(val)
    return val


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
            val = atom_or_symbol(expr)
            return Expr(Symbol("quote"), val)
        else:
            # A symbol or a literal

            return atom_or_symbol(expr)

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

                stack[-1].append(Expr(Symbol("quote"), atom_or_symbol(tok)))
            else:
                if tok == "'":
                    stack.append(tok)
                else:
                    try:
                        val = Atom(tok)
                    except:
                        if "->" in tok:
                            val = collect_arrow_refs(tok)
                        else:
                            val = Symbol(tok)
                    stack[-1].append(val)  # Normal case.
        return stack

    return _parse(lexed)


def collect_arrow_refs(tok: str):
    # (pkg-name->method *args) EXPANDS TO ((pkg-name 'method) *args).
    # But it is possible we have (pkg-outer->pkg-inner->method *args);
    # We need to evaluate this properly.
    # The basic idea is to go
    # ((pkg-outer 'pkg-inner))
    # TODO: dont split and rejoin.
    refs = tok.split("->")
    hd = Symbol(refs[0])
    if len(refs) == 1:
        return hd

    res = Expr(hd, Expr(Symbol("quote"), collect_arrow_refs("->".join(refs[1:]))))
    return res


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
    Symbol("and"): op.and_,
    Symbol("or"): op.or_,
    Symbol("not"): op.not_,
    Symbol("eq"): IS_EQ,
    Symbol("neq"): lambda a, b: not IS_EQ(a, b),
    Symbol("gt"): op.gt,
    Symbol("lt"): op.lt,
    Symbol("gte"): op.ge,
    Symbol("lte"): op.le,
    Symbol("sub"): op.sub,
    Symbol("mul"): op.mul,
    Symbol("div"): op.truediv,
    Symbol("mod"): op.mod,
    Symbol("pow"): op.pow,
    Symbol("quine"): lambda: "quine",
    Symbol("ass"): ass,
    Symbol("print"): print,
    Symbol("cons"): lambda a, b: BoxPtr(a, b),
    Symbol("hd"): lambda a: a.box,
    Symbol("tl"): lambda a: a.ptr,
    Symbol("list"): lambda *args: BoxPtr.from_python_tuple(*args),
}

GLOBAL_ENV.inner = BUILTINS


def Eval(x, env):
    while True:
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
            if Eval(cond, env):
                x = branch_true
            else:
                x = branch_false
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
            x = Eval(*x.operands, env)  # Eval(Eval(x.operands, env), env)
        else:
            # Primitive operations here -- add, etc. CPU crap.
            op = Eval(x.op, env)
            operands = [Eval(o, env) for o in x.operands]
            if isinstance(op, Procedure):  # Hack because it expects a list.
                # for binding.
                return op(operands)
            else:
                return op(*operands)
            # But builtins don't operate on lists; we unpack them instead.


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
