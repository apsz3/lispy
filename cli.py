import traceback
from pathlib import Path

import click
from prompt_toolkit import PromptSession, print_formatted_text
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory
from prompt_toolkit.completion import WordCompleter
from prompt_toolkit.history import FileHistory

import lispy as lp


def REPL(env=lp.GLOBAL_ENV):
    session = PromptSession(FileHistory("~/.lispy"))
    # Builtins we don't define in symbol table, that we want
    # to show up in repl completer
    extend_complete = [sym.s for sym in lp.BUILTINS] + ["defn"]
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
        text = lp.strip_comments(text)
        exprs = lp.balancedparens(
            text
        )  # Multiline-pasting into REPL needs to be handled.
        for expr in exprs:
            try:
                parsed = lp.parse(expr)
            except SyntaxError as exc:
                print_formatted_text(f"<Syntax error> {exc}")
                continue

            try:
                res = lp.Eval(parsed, env, False)
                if res is not None:
                    print(str(res))
                    # print_formatted_text(res) # This breaks because,
                    # for some reason, we are CALLING res, which if it is a
                    # procedure, breaks. This is because we have __call__
                    # overrided. TODO.
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
    std = lp.loadfile(Path(__file__).parent / "std.lsp")
    [lp.Exec(expr) for expr in std]
    if file:
        exprs = lp.loadfile(file)
        [lp.Exec(expr) for expr in exprs]
        if i:
            REPL()
    else:
        REPL()


if __name__ == "__main__":
    main()
