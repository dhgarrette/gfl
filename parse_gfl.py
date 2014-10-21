#!/usr/bin/env python
"""
Convert from annotations format to FUDG JSON, as specified in FUDG_JSON.md.

dhg: Copied from gfl_syntax/scripts/make_json.py, but modified to read from
stdin instead of file.

Input: First line is the input sentence (whitespace separated tokens).  
Subsequent lines are the GFL annotations.
Output: FUDG JSON string  
"""
from __future__ import print_function

import sys,re,os
try:
    import ujson as json
except ImportError:
    import json

import view
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'gflparser'))
import gfl_parser
import string

lines = [line.strip() for line in sys.stdin]
tokens = lines[0].split()
code = '\n'.join(lines[1:])
try:
    parse = gfl_parser.parse(tokens, code, check_semantics=False)
    #view.desktop_open(view.draw(parse, 'x'))
    parseJ = parse.to_json()
    print(json.dumps(parseJ), sep='\t')
except gfl_parser.GFLError as e:
    print(str(e), file=sys.stderr)
    sys.exit(100)
