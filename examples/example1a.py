#!/bin/env python
# ===============================================================================
# Copyright (c) 2007 Jason Evans <jasone@canonware.com>
# Copyright (c) 2017 Yannick Versley <yversley@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# ===============================================================================
#
# Usage: example1a.py [-v] "<input>"
#                          ^^^^^^^
#                          <input> is a string of whitespace-separated tokens.
#
# This example is a counterpart to example1.py, but using the new functionality
# for using a declarative base class instead of module inspection, and deriving
# a scanner from the tokens and keywords declared in the grammar.
#
# There is a long tradition of examples that parse simple mathematical expressions.
# Most such series of examples start with a hierarchy of non-terminals/productions
# in order to get operator precedence right, then move on to demonstrate the
# parser generator's built-in precedence disambiguation facilities.  This
# example skips the traditional first step and jumps right to using operator
# precedence specifications.
#
# The parser supports four mathematical operations on integers: +, -, *, and /.
# Multiplication and division take precedence over addition and subtraction, as
# is traditional.
#
# In order to minimize clutter, this file omits numerous details that are
# important for understanding, especially if you are not experienced with
# parser generators.  You can find such information in the Parsing module's
# docstring-based documentation, by entering the following via the Python
# prompt:
#
#   >>> import Parsing
#   >>> help(Parsing)
#
# ===============================================================================

from __future__ import print_function
from __future__ import division

import sys
import parsing
from parsing.ast_objs import mktoken
from parsing.grammar import Precedence
from parsing.cls_adapt import Grammar


# ===============================================================================
# Tokens/precedences.  See Parsing documentation to learn about the
# significance of left-associative precedence.

class ExampleGrammar(Grammar):
    pAddOp = Precedence.left()
    pMulOp = Precedence.left(before=pAddOp)
    add = mktoken('add', pAddOp, tokens='+ -')
    mul = mktoken('mul', pMulOp, tokens='* / %')

    TokenInt = mktoken('TokenInt', re='[0-9]+', convert=int)


Nonterm = ExampleGrammar.nonterm_base()

# ===============================================================================
# Nonterminals, with associated productions.  In traditional BNF, the following
# productions would look something like:
#
#   AddOp ::= plus
#           | minus.
#   MulOp ::= star
#           | slash.
#   Expr ::= int
#          | Expr AddOp Expr
#          | Expr MulOp Expr.
#   Result ::= Expr.

class Expr(Nonterm):

    def reduceInt(self, i):
        "%reduce TokenInt"
        self.val = i.val

    def reduceParen(self, _lp, expr, _rp):
        "%reduce '(' Expr ')'"
        self.val = expr.val

    def reduceAdd(self, exprA, AddOp, exprB):
        "%reduce Expr add Expr"
        if AddOp.val == "+":
            self.val = exprA.val + exprB.val
            print("{:d} <- {:d} + {:d}".format(self.val, exprA.val, exprB.val))
        elif AddOp.val == "-":
            self.val = exprA.val - exprB.val
            print("{:d} <- {:d} - {:d}".format(self.val, exprA.val, exprB.val))

    def reduceMul(self, exprA, mul_op, exprB):
        "%reduce Expr mul Expr [pMulOp]"
        if mul_op.val == "*":
            self.val = exprA.val * exprB.val
            print("{:d} <- {:d} * {:d}".format(self.val, exprA.val, exprB.val))
        elif mul_op.val == "/":
            self.val = exprA.val / exprB.val
            print("{:d} <- {:d} / {:d}".format(self.val, exprA.val, exprB.val))


# This is the start symbol; there can be only one such class in the grammar.
class Result(Nonterm):
    """%start"""

    def reduce(self, Expr):
        "%reduce Expr"
        print(u"Result: {:d}".format(Expr.val))


# ===============================================================================
# Parser.

# Parser subclasses the Lr parser driver.  Since the grammar is unambiguous, we
# have no need of the Glr driver's extra functionality, though there is nothing
# preventing us from using it.
#
# If you are curious how much more work the GLR driver has to do, simply change
# the superclass from Parsing.Lr to Parsing.Glr, then, run this program with
# verbosity enabled.
class Parser(parsing.Lr):
    def __init__(self, spec):
        parsing.Lr.__init__(self, spec)

    # Use the scanner provided by the Grammar class
    def scan(self, input):
        ExampleGrammar.feed(input, self)
        # Tell the parser that the end of input has been reached.
        self.eoi()


# ===============================================================================
# Main code.

# Introspect this module to generate a parser.  Enable all the bells and
# whistles.
spec = ExampleGrammar.spec(
        pickleFile="example1a.pickle", skinny=False,
        logFile="example1a.log",
        graphFile="example1a.dot",
        verbose=True)


parser = Parser(spec)

# Read input as a string from the command line; treat a leading -v specially,
# if present.
args = sys.argv[1:]
if len(sys.argv) > 1 and sys.argv[1] == "-v":
    # Enable verbose parsing output.
    parser.verbose = True
    args.pop(0)

# Parse command line input.
if len(args) > 0:
    parser.scan(args[0])

# ===============================================================================
