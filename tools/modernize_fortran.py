#!/usr/bin/env python3
"""
modernize_fortran.py
====================
Convert Fortran 77 fixed-form source files to Fortran 90+ free-form for the
Modsim project.

Transformations applied
-----------------------
  1.  Comment lines  (C / c / * in column 1)  →  ! comments
  2.  Continuation lines (non-blank / non-0 in column 6) joined with &
  3.  Tab-indented lines (tab in col 1 treated as advance to col 7)
  4.  Relational operators  .EQ. → ==,  .NE. → /=,  .LT. → <, etc.
  5.  Type declarations
        CHARACTER*N         →  character(len=N)
        CHARACTER*(*)       →  character(len=*)
        INTEGER*N           →  integer(N)
        REAL*N              →  real(N)
        LOGICAL*N           →  logical
        COMPLEX*N           →  complex(N)
  6.  Old-style numbered DO … CONTINUE loops → do … end do
  7.  IMPLICIT NONE inserted after last USE statement if not already present
  8.  Fortran keywords lowercased (string literals are skipped)
  9.  Source files renamed  .FOR / .for → .f90
  10. The Visual Fortran project file (ModS.vfproj) updated accordingly
"""

import re
import os
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

MODSIMDL = Path(r"C:\Users\User\Repos\ModSIm\Modsim\Modsimdl")
PSD_DIR  = Path(r"C:\Users\User\Repos\ModSIm\Modsim\PSD")
VFPROJ   = Path(r"C:\Users\User\Repos\ModSIm\ModS\ModS.vfproj")

TARGET_FILES = [
    (MODSIMDL, "AM_SCHN.FOR"),   (MODSIMDL, "AMDGRP.FOR"),
    (MODSIMDL, "BETAAMD.FOR"),   (MODSIMDL, "CALC.FOR"),
    (MODSIMDL, "CALC1.FOR"),     (MODSIMDL, "CLASMODS.FOR"),
    (MODSIMDL, "CLASREPS.FOR"),  (MODSIMDL, "COALMODS.FOR"),
    (MODSIMDL, "COALREPS.FOR"),  (MODSIMDL, "CRSHMODS.FOR"),
    (MODSIMDL, "CRSHREPS.FOR"),  (MODSIMDL, "DATAINPT.FOR"),
    (MODSIMDL, "DIMINP.FOR"),    (MODSIMDL, "DMINP.FOR"),
    (MODSIMDL, "FLTNMODS.FOR"),  (MODSIMDL, "Fltnreps.for"),
    (MODSIMDL, "GENLMODS.FOR"),  (MODSIMDL, "GENLREPS.FOR"),
    (MODSIMDL, "GENLSUBS.FOR"),  (MODSIMDL, "GRAVMODS.FOR"),
    (MODSIMDL, "GRAVREPS.FOR"),  (MODSIMDL, "KYNCHGRP.FOR"),
    (MODSIMDL, "LIBGRAPH.FOR"),  (MODSIMDL, "LJUBAMD.FOR"),
    (MODSIMDL, "MAGNMODS.FOR"),  (MODSIMDL, "MAGNREPS.FOR"),
    (MODSIMDL, "MILLMODS.FOR"),  (MODSIMDL, "millmods_old.for"),
    (MODSIMDL, "MILLREPS.FOR"),  (MODSIMDL, "millreps_old.for"),
    (MODSIMDL, "MODELGRP.FOR"),  (MODSIMDL, "Opgraph.for"),
    (MODSIMDL, "ORDER.FOR"),     (MODSIMDL, "OUTPMODS.FOR"),
    (MODSIMDL, "PPHI2.FOR"),     (MODSIMDL, "Pphi5.for"),
    (MODSIMDL, "PPHO2.FOR"),     (MODSIMDL, "REPORT.FOR"),
    (MODSIMDL, "REPWRT.FOR"),    (MODSIMDL, "SCRNMODS.FOR"),
    (MODSIMDL, "SCRNREPS.FOR"),  (MODSIMDL, "SIMOP.FOR"),
    (MODSIMDL, "SIMULATE.FOR"),  (MODSIMDL, "TEMPLATE.FOR"),
    (MODSIMDL, "TRYAMD.FOR"),    (MODSIMDL, "TRYSIM.FOR"),
    (MODSIMDL, "UNITREPS.FOR"),  (MODSIMDL, "UNITS.FOR"),
    (MODSIMDL, "WSEPMODS.FOR"),  (MODSIMDL, "WSEPREPS.FOR"),
    (PSD_DIR,  "3COMBAR.FOR"),   (PSD_DIR,  "Plotpsd.for"),
]


# ---------------------------------------------------------------------------
# Fixed-form reader  →  list of logical-line dicts
# ---------------------------------------------------------------------------

def read_fixed_form(text: str) -> list:
    """
    Parse fixed-form Fortran text into a list of dicts:
      { 'type'   : 'comment' | 'blank' | 'code',
        'label'  : int | None,
        'text'   : str,          # full logical statement (continuations joined)
        'indent' : str           # leading whitespace of first physical line
      }
    Comments between continuation lines are preserved as separate items.
    """
    raw_lines = text.splitlines()
    items: list = []
    i = 0

    while i < len(raw_lines):
        raw = raw_lines[i]
        line = raw.rstrip()

        # ---- blank -------------------------------------------------------
        if not line.strip():
            items.append({'type': 'blank', 'label': None, 'text': '', 'indent': ''})
            i += 1
            continue

        first = line[0]

        # ---- full-line comment  (C/c/* in col 1) -------------------------
        if first in ('C', 'c', '*'):
            items.append({'type': 'comment', 'label': None,
                           'text': line[1:].rstrip(), 'indent': ''})
            i += 1
            continue

        # ---- inline ! comment (already free-form style) ------------------
        if first == '!':
            items.append({'type': 'comment', 'label': None,
                           'text': line[1:].rstrip(), 'indent': ''})
            i += 1
            continue

        # ---- tab-indented line (Intel/Compaq extension) ------------------
        # A tab in column 1 advances to column 7; the rest is the statement.
        if first == '\t':
            label = None
            stmt = line[1:].rstrip()
            # Check for tab-digit which means tab + digit = continuation
            if stmt and stmt[0].isdigit() and stmt[0] != '0':
                # treat as continuation of previous
                if items and items[-1]['type'] == 'code':
                    items[-1]['text'] = items[-1]['text'].rstrip() + ' ' + stmt[1:].lstrip()
                i += 1
                continue
            indent = '      '
            # Look ahead for continuation lines
            j = i + 1
            while j < len(raw_lines):
                nxt = raw_lines[j].rstrip()
                if not nxt.strip():
                    break
                nxt_first = nxt[0] if nxt else ''
                if nxt_first in ('C', 'c', '*', '!'):
                    break
                col6_nxt = nxt[5] if len(nxt) >= 6 else ' '
                if col6_nxt not in (' ', '0', '') and col6_nxt.isprintable():
                    cont = nxt[6:72].rstrip() if len(nxt) > 6 else ''
                    stmt = stmt.rstrip() + ' ' + cont.lstrip()
                    j += 1
                else:
                    break
            items.append({'type': 'code', 'label': label,
                           'text': stmt, 'indent': indent})
            i = j
            continue

        # ---- normal fixed-form code line ---------------------------------
        # Columns 1-5 (indices 0-4): statement label
        label_str = line[:5].strip() if len(line) >= 5 else line.strip()
        label = int(label_str) if label_str.isdigit() else None

        # Column 6 (index 5): continuation marker
        col6 = line[5] if len(line) >= 6 else ' '
        is_continuation = col6 not in (' ', '0', '') and col6.isprintable()

        if is_continuation:
            # Append to the last code item
            cont = line[6:72].rstrip() if len(line) > 6 else ''
            # Find the last code item to append to
            for prev in reversed(items):
                if prev['type'] == 'code':
                    prev['text'] = prev['text'].rstrip() + ' ' + cont.lstrip()
                    break
            i += 1
            continue

        # Statement text from column 7 onward (indices 6-71)
        stmt = line[6:72].rstrip() if len(line) > 6 else ''
        # Preserve indentation within the statement for output
        raw_indent = stmt
        # Normalise: leading whitespace from within the statement
        indent = '      ' + (stmt[: len(stmt) - len(stmt.lstrip())])

        # Look ahead for continuation lines, respecting intervening comments
        j = i + 1
        while j < len(raw_lines):
            nxt = raw_lines[j].rstrip()
            if not nxt.strip():
                break
            nxt_first = nxt[0] if nxt else ''
            if nxt_first in ('C', 'c', '*', '!'):
                break   # comment stops look-ahead
            if nxt_first == '\t':
                # tab continuation if followed by digit != 0
                if len(nxt) >= 2 and nxt[1].isdigit() and nxt[1] != '0':
                    cont = nxt[2:].rstrip()
                    stmt = stmt.rstrip() + ' ' + cont.lstrip()
                    j += 1
                    continue
                break
            col6_nxt = nxt[5] if len(nxt) >= 6 else ' '
            if col6_nxt not in (' ', '0', '') and col6_nxt.isprintable():
                cont = nxt[6:72].rstrip() if len(nxt) > 6 else ''
                stmt = stmt.rstrip() + ' ' + cont.lstrip()
                j += 1
            else:
                break

        items.append({'type': 'code', 'label': label,
                       'text': stmt, 'indent': indent})
        i = j

    return items


# ---------------------------------------------------------------------------
# String-aware text transformer helpers
# ---------------------------------------------------------------------------

def split_by_strings(text: str):
    """
    Yield (is_string, chunk) pairs splitting Fortran text on quoted literals.
    Handles both ' and " delimiters; doubled delimiter = escape.
    Also handles inline ! comments (not inside strings) as a 'comment' region.
    """
    result = []
    current = ''
    in_str = False
    qchar = None
    i = 0
    n = len(text)
    while i < n:
        c = text[i]
        if in_str:
            current += c
            if c == qchar:
                if i + 1 < n and text[i + 1] == qchar:   # escaped quote
                    current += text[i + 1]
                    i += 2
                    continue
                result.append((True, current))
                current = ''
                in_str = False
                qchar = None
        else:
            if c in ('"', "'"):
                if current:
                    result.append((False, current))
                    current = ''
                in_str = True
                qchar = c
                current = c
            elif c == '!':
                # Inline comment — capture to end of line
                if current:
                    result.append((False, current))
                result.append(('comment', text[i:]))
                current = ''
                i = n
                continue
            else:
                current += c
        i += 1
    if current:
        result.append((in_str, current))
    return result


# ---------------------------------------------------------------------------
# Substitution patterns
# ---------------------------------------------------------------------------

RELATIONAL_OPS = [
    (re.compile(r'\.EQ\.', re.IGNORECASE), '=='),
    (re.compile(r'\.NE\.', re.IGNORECASE), '/='),
    (re.compile(r'\.LT\.', re.IGNORECASE), '<'),
    (re.compile(r'\.GT\.', re.IGNORECASE), '>'),
    (re.compile(r'\.LE\.', re.IGNORECASE), '<='),
    (re.compile(r'\.GE\.', re.IGNORECASE), '>='),
]

# CHARACTER*N / CHARACTER*(*)
RE_CHAR = re.compile(r'\bCHARACTER\s*\*\s*\(?([*]|\d+)\)?', re.IGNORECASE)
# INTEGER*N / REAL*N / COMPLEX*N
RE_INT  = re.compile(r'\bINTEGER\s*\*\s*(\d+)', re.IGNORECASE)
RE_REAL = re.compile(r'\bREAL\s*\*\s*(\d+)',    re.IGNORECASE)
RE_CMPL = re.compile(r'\bCOMPLEX\s*\*\s*(\d+)', re.IGNORECASE)
# LOGICAL*N
RE_LOGI = re.compile(r'\bLOGICAL\s*\*\s*\d+',   re.IGNORECASE)

# Keyword lowercasing pattern
RE_KW = re.compile(
    r'\b('
    r'PROGRAM|SUBROUTINE|FUNCTION|MODULE|SUBMODULE|BLOCK\s+DATA|'
    r'END\s+PROGRAM|END\s+SUBROUTINE|END\s+FUNCTION|END\s+MODULE|'
    r'END\s+IF|END\s+DO|END\s+SELECT|END\s+WHERE|END\s+TYPE|'
    r'ENDIF|ENDDO|ENDSELECT|ENDTYPE|END|RETURN|CONTAINS|'
    r'IMPLICIT\s+NONE|IMPLICIT|NONE|'
    r'IF|THEN|ELSE\s*IF|ELSEIF|ELSE|'
    r'SELECT\s+CASE|CASE\s+DEFAULT|CASE|'
    r'DO\s+WHILE|DO|WHILE|CYCLE|EXIT|CONTINUE|'
    r'GOTO|GO\s+TO|'
    r'CALL|USE|ONLY|INTERFACE|'
    r'INTEGER|REAL|DOUBLE\s+PRECISION|COMPLEX|LOGICAL|CHARACTER|'
    r'DIMENSION|ALLOCATABLE|ALLOCATE|DEALLOCATE|NULLIFY|POINTER|TARGET|'
    r'COMMON|EQUIVALENCE|PARAMETER|DATA|SAVE|INTENT|OPTIONAL|'
    r'OPEN|CLOSE|READ|WRITE|PRINT|FORMAT|REWIND|BACKSPACE|INQUIRE|FLUSH|'
    r'STOP|PAUSE|EXTERNAL|INTRINSIC|RECURSIVE|PURE|ELEMENTAL|'
    r'IN|OUT|INOUT'
    r')\b',
    re.IGNORECASE
)


def _modernise_code_chunk(chunk: str) -> str:
    """Apply all non-string-safe transformations to a non-string code chunk."""
    for pat, repl in RELATIONAL_OPS:
        chunk = pat.sub(repl, chunk)
    chunk = RE_CHAR.sub(
        lambda m: 'character(len=*)' if m.group(1) == '*'
                  else f'character(len={m.group(1)})', chunk)
    chunk = RE_INT.sub(lambda m: f'integer({m.group(1)})', chunk)
    chunk = RE_REAL.sub(lambda m: f'real({m.group(1)})', chunk)
    chunk = RE_CMPL.sub(lambda m: f'complex({m.group(1)})', chunk)
    chunk = RE_LOGI.sub('logical', chunk)
    chunk = RE_KW.sub(lambda m: m.group(0).lower(), chunk)
    return chunk


def transform_text(text: str, is_format: bool = False) -> str:
    """
    Apply transformations to a logical Fortran statement, skipping string literals.
    FORMAT statements are only keyword-lowercased (avoid altering their content).
    """
    if is_format:
        return RE_KW.sub(lambda m: m.group(0).lower(), text)

    parts = split_by_strings(text)
    out = ''
    for kind, chunk in parts:
        if kind is False:          # plain code
            out += _modernise_code_chunk(chunk)
        elif kind == 'comment':    # inline ! comment — lowercase only
            out += RE_KW.sub(lambda m: m.group(0).lower(), chunk)
        else:                      # string literal — pass through unchanged
            out += chunk
    return out


# ---------------------------------------------------------------------------
# DO-loop label → do…end do converter
# ---------------------------------------------------------------------------

def resolve_do_loops(items: list) -> list:
    """
    Convert  DO N var = ...  /  N CONTINUE  →  do var = ... / end do.
    Multiple DOs sharing the same terminal label are handled correctly.
    If the terminal label is on a non-CONTINUE statement, end do is inserted
    before it and the label is retained (it may also be a GOTO target).
    """
    out = []
    # Stack of target labels (ints); one entry per active DO
    stack: list = []

    for item in items:
        if item['type'] != 'code':
            out.append(item)
            continue

        text = item['text']
        lbl  = item['label']
        indent = item.get('indent', '      ')

        # ---- Match DO N var = start, end[, stride] or DO N WHILE(…) ----
        m_do = re.match(
            r'^(\s*)(do)\s+(\d+)\s+(.+)$',
            text, re.IGNORECASE
        )
        if m_do:
            lead       = m_do.group(1)   # preserve leading whitespace
            target_lbl = int(m_do.group(3))
            rest       = m_do.group(4).strip()
            stack.append(target_lbl)
            new_item = dict(item)
            new_item['text']  = lead + 'do ' + rest
            new_item['label'] = lbl   # preserve any label on the DO line itself
            out.append(new_item)
            continue

        # ---- Closing label? --------------------------------------------
        if lbl is not None and stack:
            # Close ALL DOs that share this label (handles shared-label loops)
            while stack and stack[-1] == lbl:
                stack.pop()
                out.append({'type': 'code', 'label': None,
                            'text': 'end do', 'indent': indent})

            # A CONTINUE statement exists only as a loop sentinel — discard it
            if re.match(r'^\s*CONTINUE\s*$', text, re.IGNORECASE):
                continue   # don't emit the CONTINUE itself

            # Non-CONTINUE terminal statement: keep it but strip the label
            # (the DO has already been closed above; the label might still be
            # a GOTO target so we leave the label intact for safety)
            # lbl is unchanged — will be written to output below

        new_item = dict(item)
        new_item['text']  = text
        new_item['label'] = lbl
        out.append(new_item)

    return out


# ---------------------------------------------------------------------------
# IMPLICIT NONE injector
# ---------------------------------------------------------------------------

RE_PROG_UNIT  = re.compile(
    r'^\s*(subroutine|function|program|block\s*data)\b', re.IGNORECASE
)
RE_IMPLICIT   = re.compile(r'^\s*implicit\b', re.IGNORECASE)
RE_USE        = re.compile(r'^\s*use\b',       re.IGNORECASE)
RE_INTERFACE  = re.compile(r'^\s*interface\b',  re.IGNORECASE)
RE_END_IFACE  = re.compile(r'^\s*end\s+interface\b', re.IGNORECASE)


def insert_implicit_none(items: list) -> list:
    """
    After each top-level program-unit header, insert  implicit none  if not
    already present, placing it after all USE statements.
    Skips function/subroutine declarations that live inside INTERFACE blocks.
    """
    result = list(items)
    i = 0
    interface_depth = 0   # track whether we are inside an INTERFACE block
    while i < len(result):
        item = result[i]
        if item['type'] == 'code':
            txt = item['text'].strip()
            if RE_INTERFACE.match(txt) and not RE_END_IFACE.match(txt):
                interface_depth += 1
            elif RE_END_IFACE.match(txt):
                interface_depth = max(0, interface_depth - 1)
            elif interface_depth == 0 and RE_PROG_UNIT.match(txt):
                j = i + 1
                has_implicit = False
                last_use_pos = i
                while j < len(result):
                    ji = result[j]
                    if ji['type'] in ('blank', 'comment'):
                        j += 1
                        continue
                    if ji['type'] == 'code':
                        jtxt = ji['text'].strip()
                        if RE_IMPLICIT.match(jtxt):
                            has_implicit = True
                            break
                        if RE_USE.match(jtxt):
                            last_use_pos = j
                            j += 1
                            continue
                    break
                if not has_implicit:
                    ins = {'type': 'code', 'label': None,
                           'text': 'implicit none', 'indent': '      '}
                    result.insert(last_use_pos + 1, ins)
                    i = last_use_pos + 2
                    continue
        i += 1
    return result


# ---------------------------------------------------------------------------
# Free-form writer
# ---------------------------------------------------------------------------

RE_FORMAT_STMT = re.compile(r'^\s*\d*\s*format\s*\(', re.IGNORECASE)


def write_free_form(items: list) -> str:
    lines = []
    for item in items:
        t     = item['type']
        lbl   = item.get('label')
        text  = item.get('text', '')

        if t == 'blank':
            lines.append('')
            continue

        if t == 'comment':
            stripped = text.strip()
            lines.append(('! ' + stripped) if stripped else '!')
            continue

        # Code — determine if it's a FORMAT statement (don't alter body)
        is_fmt = bool(RE_FORMAT_STMT.match(text.strip()))
        text = transform_text(text, is_format=is_fmt)

        if lbl is not None:
            lines.append(f'{lbl:<5} {text}')
        else:
            lines.append('      ' + text)

    return '\n'.join(lines) + '\n'


# ---------------------------------------------------------------------------
# Per-file pipeline
# ---------------------------------------------------------------------------

def convert_file(src: Path) -> str:
    raw = src.read_text(encoding='latin-1', errors='replace')
    items = read_fixed_form(raw)
    items = resolve_do_loops(items)
    items = insert_implicit_none(items)
    return write_free_form(items)


# ---------------------------------------------------------------------------
# vfproj patcher
# ---------------------------------------------------------------------------

def patch_vfproj(rename_map: dict):
    """Update all RelativePath entries in the .vfproj file."""
    text = VFPROJ.read_text(encoding='utf-8')
    for old_rel, new_rel in rename_map.items():
        # Try both \ and / separators, case-insensitive.
        # Use lambda replacement to avoid backslash interpretation in new_rel.
        for sep in ('\\', '/'):
            old_r = old_rel.replace('\\', sep).replace('/', sep)
            new_r = new_rel.replace('\\', sep).replace('/', sep)
            text  = re.sub(re.escape(old_r), lambda m, r=new_r: r,
                           text, flags=re.IGNORECASE)
    VFPROJ.write_text(text, encoding='utf-8')


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    converted = 0
    skipped   = []
    rename_map = {}

    for directory, fname in TARGET_FILES:
        src = directory / fname
        if not src.exists():
            skipped.append(str(src))
            continue

        dst = src.with_suffix('.f90')
        try:
            content = convert_file(src)
            dst.write_text(content, encoding='utf-8')
            src.unlink()
            # Build rename map relative to vfproj location
            old_rel = os.path.relpath(str(src), str(VFPROJ.parent))
            new_rel = os.path.relpath(str(dst), str(VFPROJ.parent))
            rename_map[old_rel] = new_rel
            print(f'  OK   {fname:30s}  →  {dst.name}')
            converted += 1
        except Exception as exc:
            print(f'  ERR  {fname}: {exc}')

    patch_vfproj(rename_map)
    print(f'\nDone: {converted} files converted, {len(skipped)} not found.')
    if skipped:
        print('Not found:')
        for s in skipped:
            print(f'  {s}')
    print(f'vfproj updated: {VFPROJ}')


if __name__ == '__main__':
    main()
