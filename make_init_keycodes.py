#!/usr/bin/env python3
"""Reads SDL_keycode.h to generate a init_keycodes.lisp file.

Requires: Python 3.10+

Pass --search-dir DIR as many times as needed to add search
directories when the defaults don't work.

The init_keycodes.lisp file is included by init.lisp as a
default keymap.
"""
from __future__ import annotations

# Before imports for the sake of convenience
INCLUDE_FILE_SEARCH_LOCATIONS = {
    "linux" : (
        '/usr/include/SDL3',
        '/usr/local/include/SDL3'
    ),
    "darwin": (
        '${HOMEBREW}/usr/include/SDL3',
    )
}


# And now, back to the usual Python file structure.
import argparse
import os
import datetime
import hashlib
import io
import logging
import re
import sys
import subprocess
import textwrap

from contextlib import ExitStack
from collections.abc import Iterable
from dataclasses import dataclass, field
from itertools import chain
from pathlib import Path
from typing import TYPE_CHECKING


PathLike = str | Path

log: "logging.LogAdapter" | "logging.Logger"

# Patch docstring to it shows up in argparse help
def patched_docstring(docstring, to_list: dict[str, tuple[str]]) -> str:
    _stream = io.StringIO(docstring)
    indent = 0
    def s(*args, **kwargs):
        if indent:
            _stream.write("  " * indent)
        print(*args, file=_stream, **kwargs)

    s("")
    s("Default search locations for each platform are:")
    s("")
    indent += 1
    for platform, known_dirs in INCLUDE_FILE_SEARCH_LOCATIONS.items():
        s(f"{platform!r}:")
        indent += 1
        for known_dir in known_dirs:
            s(known_dir)
        indent -= 1
        s("")
    s("")
    return _stream.getvalue()


__doc__ = patched_docstring(__doc__, INCLUDE_FILE_SEARCH_LOCATIONS)


parser = argparse.ArgumentParser(
    prog=__file__,
    description=__doc__,
    formatter_class=argparse.RawDescriptionHelpFormatter
)


def parse_log_level_arg(raw: str) -> int:
    level = None
    try:
        level = int(raw)
    except ValueError as e:
        level = getattr(logging, raw.upper(), None)
    if level is None or not isinstance(level, int):
        raise ValueError("Could not parse a log level from {raw!r}")
    print(level, raw)
    return level


def parse_output_stream(raw: str) -> Path | "io.TextIOBase":
    """Returns stdout if given "-" or a Path object."""
    if raw == "-":
        return sys.stdout
    else:
        return Path(raw)


parser.add_argument(
    "--output-file",
    default="init_keycode.lisp", type=parse_output_stream,
    help="Pass - to write to stdout or a filename.")

parser.add_argument("--search-dir",
    action="append", default=[],
    help="Override any default platform search dirs by specifying one.")

parser.add_argument("--log-level",
    type=parse_log_level_arg,
    default=logging.INFO,
    help="A Python log level as DEBUG, INFO, WARNING, ERROR, CRITICAL, or an int.")


RE_SHELLSLOT = re.compile(
    r"""  # Matches ${ENV_VARS} to template.
    \$\{  # Opening
    (?P<varname>
        [^}]*
    )
    \}    # End of template
    """,
    re.X
)



def get_templated_path(path_as_str: str) -> Path:
    """Replace ${VAR_NAMES} in path_as_str, then return a Path object.

    Args:
        path_as_str: A string to template and convert to a Path.

    Returns:
        a Path object.
    """
    to_replace: dict[str, str] = {}

    for match in RE_SHELLSLOT.finditer(path_as_str):
        replacement_string = match.group(0)
        env_var_name = match.group('varname')
        var_value = os.getenv(env_var_name)

        if not env_var_name:
            raise NameError("Cannot resolve value for expression {replacement_string!r}")

        to_replace[replacement_string] = var_value

    replaced = path_as_str
    for to_replace, value in to_replace.items():
        replaced = replaced.replace(to_replace, value)

    return Path(replaced)


def resolve_header_file(
    filename: str,
    search_list: Iterable[str] | None = None
) -> Path | None:
    """Resolve a Path for the header file by searching possible locations.

    If no search_list is provided, this tries to read from platform defaults.
    """
    if not search_list:
        have_default = INCLUDE_FILE_SEARCH_LOCATIONS.get(sys.platform, None)
        if not have_default:
            raise NotImplementedError(
                f"Pass an --include-dir or implement in generate_keys.py"
            )
        search_list = have_default

    found = None

    for candidate_raw in search_list:
        candidate_parent = get_templated_path(candidate_raw)
        candidate = candidate_parent / filename
        candidate_str = str(candidate)

        if not candidate.exists():
            log.debug(
                f"SKIP: {str(candidate_parent)} lacks {filename}, skipping")
        elif candidate.is_dir():
            log.warning(
                f"DIR: {candidate_str!r} exists, but as a dir?")
        elif candidate.is_file():
            found = candidate
            log.debug(f"FILE: Found {candidate_str!r}")

    return found


NAME_REGEX = re.compile(
    r"""
    \#define                     # The C define opening
    \s+
    (?P<full_constant_name>
        SDL
        (?P<name>
            K_                   # Keyname prefix
            [A-Z0-9_]+           # Allcaps, numbers, and _
        )
    )
    \s+                          # Howevermany tabs/spaces
    (?P<value>
        (?P<expr>  # Option 1: some expression
            \(
            ([^)])+
            \)
        )
        |
        (?P<hex>  # A hex literal
            0x[0-9a-f]+
        )u?
        |
        (?P<decimal>  # A decimal literal
            [0-9]+
        )u?
    )
    """, re.X)


_SUPPORTED_OPERATORS = frozenset(['<<', '>>', '|', '+', '&', '^'])
_empty_set = frozenset(())



def _listwrap(iterable: Iterable, opening: str, closing: str, sep =', '):
    joined = ''.join([
        opening,
        sep.join(iterable),
       closing
    ])
    return joined


def asSetlikeStr(iterable: Iterable):
    return _listwrap(iterable, opening='{', closing='}', sep=', ')


@dataclass
class Expression:
    operator: str
    operands: list[str] = field(default_factory=list)

    @classmethod
    def from_string(cls, expr: str, permitted_operators: set[str] | None = _SUPPORTED_OPERATORS) -> "Expression":
        if permitted_operators is None:
            permitted_operators = _empty_set

        clean_parts = expr.lstrip('(').rstrip(')').split()
        if len(clean_parts) != 3:
            raise ValueError(f"Unknown expression format: {expr}")

        operator = clean_parts[1]
        a = clean_parts[0].rstrip('u')
        b = clean_parts[-1].rstrip('u')

        if operator not in permitted_operators:
            _joined = asSetlikeStr(
                [repr(op) for op in permitted_operators]
            )
            # The syntax below is nicer escaping { in Python's f-strings
            raise ValueError(f"Operator {operator!r} not one of {_joined}")

        expression = cls(operator=operator, operands=[a, b])

        return expression

    def __str__(self):
        return _listwrap(
            self.operands,
            opening=f"({self.operator} ",
            closing=")",
            sep=' '
        )


DefsDict = dict[str, str | Expression]


def extract_key_defs(source: str) -> DefsDict:

    found: DefsDict = {}

    for match in NAME_REGEX.finditer(source):
        definition = match.groupdict()
        name = definition['name']
        if have_hex := definition.get('hex', None):
            found[name] = have_hex
        elif have_decimal := definition.get('decimal', None):
            found[name] = have_decimal
        elif expression := definition.get('expr', None):
            _e = Expression.from_string(expression)
            found[name] = _e
        else:
            raise ValueError("Unhandled definition: {definition!r}")

    return found


def print_key_defs(defs: DefsDict, file=sys.stdout):
    for k, v in defs.items():
        line_items = [k, str(v)]

        formatted = _listwrap(
            line_items,
            opening="(define ",
            closing=")",
            sep=" "
        )
        print(formatted, file=file)


USE_HASH = 'sha256'


def hash_source_code(source: str, hashname=USE_HASH) -> str:
    _hash = hashlib.new(hashname)
    as_bytes = source.encode("utf8")
    _hash.update(as_bytes)
    return _hash.hexdigest()


def get_utc_timestamp() -> str:
    """Get the UTC timestamp in ISO format."""
    dt = None
    if sys.version_info >= (3, 12):
        # Deprecated in 3.12 in favor of else clause's form
        dt = datetime.datetime.now(datetime.UTC)
    else:
        dt = datetime.datetime.utcnow()
    return dt.isoformat()


def print_header_table(header_table: dict[str, str], file=sys.stdout):

    name_column_width = max((len(k) for k in header_table))

    def fprint(*args, **kwargs):
        print(*args, file=file, **kwargs)

    for name, value in header_table.items():
        fprint(f"; {name:<{name_column_width}} : {value}")


def main(filename="SDL_keycode.h"):
    args = parser.parse_args()
    logging.basicConfig(level=args.log_level)
    global log
    log = logging.getLogger()

    search_dirs = args.search_dir
    if search_dirs:
        log.info("Using search dir overrides from CLI")

    have_header_file = resolve_header_file(filename, search_dirs)
    if not have_header_file:
        log.error(f"Could not resolve {filename!r}")
        exit(1)
    header_path_str = str(have_header_file)

    # Load everything we know how to
    log.info(f"Reading source file {header_path_str!r}")
    source_code = have_header_file.read_text()
    source_code_hash = hash_source_code(source_code)
    log.debug(f"Got source ({USE_HASH}={source_code_hash})")

    log.debug(f"Extracting definitions from {header_path_str!r}")
    defs = extract_key_defs(source_code)

    # Write it out as LISP
    output = args.output_file
    using_stdout = output is sys.stdout
    reporting_name = "stdout" if using_stdout else f"{str(output)}"
    table = {
        "SDL header": filename ,
        "UTC timestamp": get_utc_timestamp(),
        USE_HASH: source_code_hash
    }
    log.info(f"Writing LISP to {reporting_name!r}")

    with ExitStack() as stack:
        if using_stdout:
            file = output
        else:
            file = stack.enter_context(open(output, "w"))

            print(f"; Auto-generated by {sys.argv[0]}", file=file)
            print_header_table(table, file=file)
            print_key_defs(defs, file=file)


if __name__ == "__main__":
    main()

