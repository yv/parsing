# ===============================================================================
# Copyright (c) 2007 Jason Evans <jasone@canonware.com>
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
# ===============================================================================
#
# Release history:
#
# 1.4 (15 December 2012): Python 3 support.
#
#                         Performance optimizations & bug fixes.
#
#                         Published on PyPI.
#
# 1.3 (8 August 2007): Retroactively number public releases.
#
#                      Back-port to Python 2.4.
#
#                      Remove some magic surrounding epsilon, in order to
#                      generalize/simplify.
#
# 1.2 (6 May 2007): Fix some off-by-one errors in production count reporting.
#
#                   Add some missing code that helps detect which definitions
#                   are used/unused when building the parser.
#
# 1.1 (22 March 2007): Optimize/generalize Lr._production() by using argument
#                      list expansion.
#
# 1.0 (19 March 2007): Initial public release.
#
# ===============================================================================
"""
There are two separate parser driver classes:

  Lr : Standard Characteristic Finite State Machine (CFSM) driver, based on
       unambiguous LR(1) parsing tables.  This driver is faster than the Glr
       driver, but it cannot deal with all parsing tables that the Glr
       driver can.

  Glr : Generalized LR driver, capable of tracking multiple parse trees
        simultaneously, if the %split precedence is used to mark ambiguous
        actions.  This driver is closely based on Elkhound's design, which
        is described in a technical report:

            Elkhound: A Fast, Practical GLR Parser Generator
            Scott McPeak
            Report No. UCB/CSD-2-1214 (December 2002)
            http://www.cs.berkeley.edu/~smcpeak/elkhound/

Parser generator directives are embedded in docstrings, and must begin with
a '%' character, followed immediately by one of several keywords:

    Precedence : %fail %nonassoc %left %right %split
         Token : %token
  Non-terminal : %start %nonterm
    Production : %reduce

All of these directives are associated with classes except for %reduce.
%reduce is associated with methods within non-terminal classes.  The Parsing
module provides base classes from which precedences, tokens, and
non-terminals must be derived.  This is not as restrictive as it sounds,
since there is nothing preventing, for example, a master Token class that
subclasses Parsing.Token, which all of the actual token types then subclass.
Also, nothing prevents using multiple inheritance.

Folowing are the base classes to be subclassed by parser specifications:

  * Precedence
  * Token
  * Nonterm

The Parsing module implements the following exception classes:

  * Exception
  * SpecError
  * SyntaxError
  * AttributeError
"""
from __future__ import print_function

__all__ = ["SpecError", "Parser", "Spec", "Lr", "Glr"]

import sys
import types

import six

from parsing.ast import Nonterm, Token, \
    NontermStart
from parsing.grammar import Precedence, Production, \
    TokenSpec, SymbolSpec, NontermSpec, Item, \
    Epsilon, epsilon, EndOfInput, eoi, SpecError

RETURN_NONE = object()


class UnexpectedToken(SyntaxError):
    def __init__(self, message, offset, token):
        SyntaxError.__init__(self, message)
        self.offset = offset
        self.token = token


class String(list):
    def __init__(self, args=None):
        if args is None:
            args = []
        list.__init__(self, args)

        self.hash = self._hash()

    def __cmp__(self, other):
        assert isinstance(other, String)
        minLen = min(len(self), len(other))
        for i in range(minLen):
            if self[i] < other[i]:
                return -1
            elif self[i] > other[i]:
                return 1

        # Prefixes are identical.  Handle trailing characters, if any.
        if len(self) < len(other):
            return -1
        elif len(self) == len(other):
            return 0
        else:
            assert len(self) > len(other)
            return 1

    def __eq__(self, other):
        if len(self) == len(other):
            for i in range(len(self)):
                if not self[i] == other[i]:
                    return False
            return True
        else:
            return False

    def _hash(self):
        ret = 5381
        for sym in self:
            ret = ((ret << 5) + ret) + hash(sym)
            ret &= 0xffffffffffffffff
        return ret

    def __hash__(self):
        return self.hash


class StringSpec(object):
    cache = {}

    def __init__(self, s):
        assert isinstance(s, String)
        for sym in s:
            assert isinstance(sym, SymbolSpec)

        self.s = s
        if s in StringSpec.cache:
            self.firstSet = StringSpec.cache[s]
        else:
            # Calculate the first set for the string encoded by the s vector.
            self.firstSet = []  # Set.
            merge_epsilon = True
            for sym in self.s:
                has_epsilon = False
                for elm in sym.firstSet:
                    if elm == epsilon:
                        has_epsilon = True
                    elif sym not in self.firstSet:
                        self.firstSet.append(elm)
                if not has_epsilon:
                    merge_epsilon = False
                    break
            # Merge epsilon if it was in the first set of every symbol.
            if merge_epsilon:
                self.firstSet.append(epsilon)

            # Cache the result.
            StringSpec.cache[s] = self.firstSet


class ItemSet(dict):
    def __init__(self, args=[]):
        dict.__init__(self, args)
        self._added = {}

    def __repr__(self):
        kernel = [item for item in six.iterkeys(self)]
        kernel.sort()
        added = [item for item in six.iterkeys(self._added)]
        added.sort()
        return "ItemSet(kernel: %s, added: %r)" % \
               (", ".join(["%r" % item for item in kernel]), \
                ", ".join(["%r" % item for item in added]))

    def __hash__(self):
        # This works because integers never overflow, and addition is
        # transitive.
        ret = 0
        for item in six.iterkeys(self):
            ret += item.hash
        return ret

    def __eq__(self, other):
        if len(self) != len(other):
            return False
        for sItem in six.iterkeys(self):
            if sItem not in other:
                return False
        return True

    def __iter__(self):
        for item in six.iterkeys(self):
            assert item.production.lhs.name == "<S>" or item.dotPos != 0
            yield item
        for item in six.iterkeys(self._added):
            assert item.dotPos == 0
            assert item.production.lhs.name != "<S>"
            yield item

    # Merge a kernel item.
    def append(self, item):
        assert item.production.lhs.name == "<S>" or item.dotPos != 0

        if item in self:
            print("item:",item,self[item] is item, self[item] == item)
            self[item].lookahead.update(item.lookahead)
        else:
            tItem = Item(item.production, item.dotPos, sorted(item.lookahead.keys()))
            self[tItem] = tItem

    # Merge an added item.
    def addedAppend(self, item):
        assert item.dotPos == 0
        assert item.production.lhs.name != "<S>"

        if item in self._added:
            lookahead = self._added[item].lookahead
            oldLen = len(lookahead)
            lookahead.update(item.lookahead)
            return (oldLen != len(lookahead))
        else:
            self._added[item] = item
            return True

    # Given a list of items, compute their closure and merge the results into
    # the set of added items.
    def _closeItems(self, items):
        # Iterate over the items until no more can be added to the closure.
        i = 0
        while i < len(items):
            item = items[i]
            rhs = item.production.rhs
            dotPos = item.dotPos
            if (dotPos < len(rhs) and
                    isinstance(rhs[dotPos], NontermSpec)):
                for lookahead in list(item.lookahead.keys()):
                    string = StringSpec(
                        String(rhs[dotPos + 1:] + [lookahead]))
                    lhs = rhs[dotPos]
                    for prod in lhs.productions:
                        tItem = Item(prod, 0, string.firstSet)
                        if self.addedAppend(tItem):
                            items.append(tItem)
            i += 1

    # Calculate and merge the kernel's transitive closure.
    def closure(self):
        items = []
        for item in six.iterkeys(self):
            rhs = item.production.rhs
            dotPos = item.dotPos
            if dotPos < len(rhs) and isinstance(
                    rhs[dotPos], NontermSpec):
                for lookahead in six.iterkeys(item.lookahead):
                    string = StringSpec(
                        String(rhs[dotPos + 1:] + [lookahead]))
                    lhs = rhs[dotPos]
                    for prod in lhs.productions:
                        tItem = Item(prod, 0, string.firstSet)
                        if self.addedAppend(tItem):
                            items.append(tItem)
        self._closeItems(items)

    # Calculate the kernel of the goto set, given a particular symbol.
    def goto(self, sym):
        ret = ItemSet()
        for item in self:
            rhs = item.production.rhs
            dotPos = item.dotPos
            if dotPos < len(rhs) and rhs[dotPos] == sym:
                tItem = Item(item.production, dotPos + 1, list(item.lookahead.keys()))
                ret.append(tItem)
        return ret

    def merge(self, other):
        """
        Merge the kernel of other into this ItemSet, then update the closure.
        It is not sufficient to copy other's added items, since other has not
        computed its closure.
        
        :param other: another ItemSet
        :return: True
        """
        items = []
        for item in six.iterkeys(other):
            if item in self:
                lookahead = self[item].lookahead
                tLookahead = []
                for sym in six.iterkeys(item.lookahead):
                    if sym not in lookahead:
                        lookahead[sym] = sym
                        tLookahead.append(sym)
                if len(tLookahead) > 0:
                    tItem = Item(item.production, item.dotPos, tLookahead)
                    items.append(tItem)
            else:
                tItem = Item(
                    item.production, item.dotPos, list(item.lookahead.keys()))
                self[tItem] = tItem
                items.append(tItem)

        if len(items) > 0:
            self._closeItems(items)
            return True
        else:
            return False

    def weakCompat(self, other):
        """
        Determine if self and other are weakly compatible, as defined by the
        Pager(1977) algorithm.
    
        :param other: an itemset that is a candidate for merging
        :type other: ItemSet
        :return: 
        """
        # Check for identical kernel LR(0) items, and pair items, for later use.
        if len(self) != len(other):
            return False
        pairs = []
        for sItem in six.iterkeys(self):
            if sItem not in other:
                return False
            oItem = other[sItem]
            pairs.append((sItem, oItem))

        # Check for lookahead compatibility.
        # Two items i and j are lookahead incompatible iff there is
        # no lookahead item shared between i and j in either self or other
        # but merging the items would yield a common lookahead item.
        for i in range(len(pairs) - 1):
            iPair = pairs[i]
            isItem = iPair[0]
            ioItem = iPair[1]
            for j in range(i + 1, len(pairs)):
                jPair = pairs[j]
                jsItem = jPair[0]
                joItem = jPair[1]

                if (isItem.lookaheadDisjoint(joItem)
                        and ioItem.lookaheadDisjoint(jsItem)):
                    pass
                elif not isItem.lookaheadDisjoint(jsItem):
                    pass
                elif not ioItem.lookaheadDisjoint(joItem):
                    pass
                else:
                    return False
        return True


class Action(object):
    """
    Abstract base class, subclassed by {Shift,Reduce}Action.
    """

    def __init__(self):
        pass


class ShiftAction(Action):
    """
Shift action, with assocated nextState.
"""

    def __init__(self, nextState):
        Action.__init__(self)
        self.nextState = nextState

    def __repr__(self):
        return "[shift %r]" % self.nextState

    def __eq__(self, other):
        if not isinstance(other, ShiftAction):
            return False
        if self.nextState != other.nextState:
            return False
        return True


class ReduceAction(Action):
    """
Reduce action, with associated production.
"""

    def __init__(self, production):
        Action.__init__(self)
        self.production = production

    def __repr__(self):
        return "[reduce %r]" % self.production

    def __eq__(self, other):
        if not isinstance(other, ReduceAction):
            return False
        if self.production != other.production:
            return False
        return True


class Spec(object):
    """
The Spec class contains the read-only data structures that the Parser
class needs in order to parse input.  Parser generation results in a
Spec instance, which can then be shared by multiple Parser instances.
"""

    def __init__(self, grammar_adapter, pickleFile=None, pickleMode="rw",
                 skinny=True, logFile=None, graphFile=None, verbose=False):
        """
modules : Either a single module, or a list of modules, wherein to
          look for parser generator directives in docstrings.

pickleFile : The path of a file to use for Spec pickling/unpickling.

pickleMode :  "r" : Unpickle from pickleFile.
              "w" : Pickle to pickleFile.
              "rw" : Unpickle/pickle from/to pickleFile.

skinny : If true, discard all data that are only strictly necessary
         while constructing the parsing tables.  This reduces
         available debugging context, but substantially reduces
         pickle size.

logFile : The path of a file to store a human-readable copy of the
          parsing tables in.

graphFile : The path of a file to store a graphviz representation
            (dot format) of the precedence relationship graph.

verbose : If true, print progress information while generating the
          parsing tables.
"""
        assert pickleFile is None or type(pickleFile) == str
        assert pickleMode in ["rw", "r", "w"]
        assert type(skinny) == bool
        assert logFile is None or type(logFile) == str
        assert graphFile is None or type(graphFile) == str
        assert type(verbose) == bool

        if isinstance(grammar_adapter, types.ModuleType):
            # Compatibility with parsing 1.5: use a ModuleAdapter for modules
            from parsing.module_spec import ModuleSpecSource
            grammar_adapter = ModuleSpecSource(grammar_adapter)

        self._skinny = skinny
        self._verbose = verbose

        # Default (no) precedence.
        self._none = Precedence("none", "fail", {})
        self._split = Precedence("split", "split", {})

        # Symbols are maintained as two separate sets so that non-terminals and
        # terminals (tokens) can be operated on separately where needed.
        self._precedences = {self._none.name: self._none,
                             self._split.name: self._split}
        self._nonterms = {}
        self._aux_nonterms = {}
        self._tokens = {eoi.name: eoi, epsilon.name: epsilon}
        self._sym2spec = {EndOfInput: eoi, Epsilon: epsilon}
        self._productions = []

        self._userStartSym = None
        self._startSym = None
        self._startProd = None

        # Everything below this point is computed from the above (once
        # introspection is complete).

        self._itemSets = []  # Each element corresponds to an element in _action.
        self._itemSetsHash = None
        # LR parsing tables.  The tables conceptually contain one state per row,
        # where each row contains one element per symbol.  The table is
        # conceptually in row-major form, but each row is actually a dictionary.
        # If no entry for a symbol exists for a particular state, then input of
        # that symbol is an error for that state.
        self._action = []
        self._goto = []
        self._startState = None
        self._nActions = 0
        self._nConflicts = 0
        self._nImpure = 0  # Number of LR impurities (does not affect GLR).

        # Introspect modules and generate parse tables.
        self._prepare(grammar_adapter, pickleFile, pickleMode, logFile, graphFile)

    def __getPureLR(self):
        return (self._nConflicts + self._nImpure == 0)

    def __setPureLR(self):
        raise AttributeError

    pureLR = property(__getPureLR, __setPureLR)

    def __getConflicts(self):
        return self._nConflicts

    def __setConflicts(self):
        raise AttributeError

    conflicts = property(__getConflicts, __setConflicts)

    def __repr__(self):
        if self._skinny:
            # Print a very reduced summary, since most info has been discarded.
            return "Parsing.Spec: %d states, %d actions (%d split)" % \
                   (len(self._action), self._nActions, self._nImpure)

        lines = []

        # =======================================================================
        lines.append("Precedences:")
        deco = [(prec.name, prec) for prec in six.itervalues(self._precedences)]
        deco.sort()
        for elm in deco:
            prec = elm[1]
            lines.append("  %r" % prec)

        lines.append("Tokens:")
        syms = [sym for sym in six.itervalues(self._tokens)]
        syms.sort()
        for token in syms:
            lines.append("  %r %r" % (token, token.prec))
            lines.append("    First set: %r" % token.firstSet)
            lines.append("    Follow set: %r" % token.followSet)

        lines.append("Non-terminals:")
        syms = [sym for sym in six.itervalues(self._nonterms)]
        syms.sort()
        for sym in syms:
            lines.append("  %r %r" % (sym, sym.prec))
            lines.append("    First set: %r" % sym.firstSet)
            lines.append("    Follow set: %r" % sym.followSet)
            lines.append("    Productions:")
            prods = sym.productions[:]
            prods.sort()
            for prod in prods:
                lines.append("      %r" % prod)

        lines.append("Item sets:")
        for i in range(len(self._itemSets)):
            lines.append("  %d: %r" % (i, self._itemSets[i]))
        # =======================================================================

        ntokens = len(self._tokens) - 1
        nnonterms = len(self._nonterms) - 1
        nproductions = len(self._productions) - 1
        nstates = len(self._action)

        def plural(n):
            if n == 1:
                return ''
            else:
                return 's'

        lines.append((
                         "Parsing.Spec: %d token%s, %d non-terminal%s, " +
                         "%d production%s, %d state%s, %d action%s (%d split):") % (
                         ntokens, plural(ntokens),
                         nnonterms, plural(nnonterms),
                         nproductions, plural(nproductions),
                         nstates, plural(nstates),
                         self._nActions, plural(self._nActions == 1),
                         self._nImpure))
        if self.pureLR:
            lines.append("Algorithm compatibility: GLR, LR")
        elif self._nConflicts == 0:
            lines.append("Algorithm compatibility: GLR")
        else:
            lines.append("Algorithm compatibility: None, due to ambiguity")
        lines.append("Parsing tables:")
        for i in range(len(self._action)):
            lines.append("  %s" % ("=" * 78))
            lines.append("  State %d:%s" % \
                         (i, ("", " (start state)")[self._startState == i]))
            items = [item for item in self._itemSets[i]]
            items.sort()
            for item in items:
                lines.append(" %s%s" % (" " * (len("%d" % i) + 9),
                                        item.lr0__repr__()))
            lines.append("    Goto:")
            syms = [sym for sym in self._goto[i]]
            syms.sort()
            for sym in syms:
                lines.append("    %15r : %r" % (sym, self._goto[i][sym]))
            lines.append("    Action:")
            syms = [sym for sym in self._action[i]]
            syms.sort()
            for sym in syms:
                for action in self._action[i][sym]:
                    conflict = "   "
                    for other in self._action[i][sym]:
                        if action != other:
                            resolution = self._resolve(sym, other, action)
                            if resolution == "err":
                                conflict = "XXX"
                                break

                    if type(action) == ShiftAction:
                        if sym.name == 'star' and 'X' in conflict:
                            import ipdb
                            ipdb.set_trace()
                        lines.append("%s %15r : %-6s %d [%s] _%s" % \
                                     (conflict, sym, "shift", action.nextState, \
                                      sym.prec.name, sym.tokenType))
                    else:
                        assert type(action) == ReduceAction
                        lines.append("%s %15r : %-6s %r" % \
                                     (conflict, sym, "reduce", action.production))

        ret = "\n".join(lines)
        return ret

    def _prepare(self, adapter, pickleFile, pickleMode, logFile, graphFile):
        """
Compile the specification into data structures that can be used by
the Parser class for parsing.
"""
        # Get the grammar specification.
        self._introspect(adapter)

        # Augment grammar with a special start symbol and production:
        #
        #   <S> ::= S <$>.
        assert self._startSym is None
        assert isinstance(self._userStartSym, NontermSpec)
        self._startSym = NontermSpec("<S>", NontermStart,
                                     "%s.NontermStart" % __name__, self._none)
        self._startProd = Production(NontermStart.reduce,
                                     "%s.NontermStart.reduce" % __name__,
                                     self._none, self._startSym, \
                                     [self._userStartSym, eoi])
        self._startSym.productions.append(self._startProd)
        self._nonterms["<S>"] = self._startSym
        self._productions.append(self._startProd)

        # Resolve references in the grammar specification.
        self._references(logFile, graphFile)

        # Check for a compatible pickle.
        compat = self._unpickle(pickleFile, pickleMode)

        if compat == "incompatible":
            # Create the collection of sets of LR(1) items.
            self._firstSets()
            self._followSets()
            self._items()

        if compat == "compatible":
            # Just because the pickle was compatible does not mean that it is
            # valid for parsing.
            if self._nConflicts != 0:
                raise SpecError("Compatible pickle is invalid due to conflicts (%d)" % \
                                self._nConflicts)
        if compat in ["itemsets", "incompatible"]:
            # Generate LR(1) parsing tables.
            self._lr()

            # Disambiguate actions.
            self._disambiguate()

            # Check for unused or ambiguous definitions, as well as reporting
            # ambiguities.
            try:
                self._validate(logFile)
            finally:
                # Pickle the spec, if method parameters so dictate, even if
                # there were validation errors, so that the pickle might be
                # used in part during later runs.
                self._pickle(pickleFile, pickleMode)
        elif compat == "repickle":
            # Pickle the spec, if method parameters so dictate.
            self._pickle(pickleFile, pickleMode)

        if self._skinny:
            # Discard data that are not needed during parsing.  Note that
            # _pickle() also discarded data that don't even need to be pickled.
            del self._precedences
            del self._nonterms
            del self._tokens
            del self._productions

    # Introspect modules and find special parser declarations.  In order to be
    # a special class, the class must both 1) be subclassed from Token or
    # Nonterm, and 2) contain the appropriate %foo docstring.
    def _introspect(self, adapter):
        # if self._verbose:
        #    print(("Parsing.Spec: Introspecting module%s to acquire formal" + \
        #    " grammar specification...") % ("s", "")[len(modules) == 1])

        self._precedences["none"] = self._none
        self._precedences["split"] = self._split

        for prec in adapter.get_precedences():
            name = prec.name
            if name in self._precedences:
                raise SpecError("Duplicate precedence name: %s" % name)
            if name in self._tokens:
                raise SpecError("Identical token/precedence names: %s" % name)
            if name in self._nonterms:
                raise SpecError("Identical nonterm/precedence names: %s" % \
                                name)
            self._precedences[name] = prec

        for token in adapter.get_tokens():
            name = token.name
            tokenType = token.tokenType
            if name in self._precedences:
                raise SpecError("Identical precedence/token names: %s" % tokenType.__doc__)
            if name in self._tokens:
                raise SpecError("Duplicate token name: %s" % tokenType.__doc__)
            if name in self._nonterms:
                raise SpecError("Identical nonterm/token names: %s" % tokenType.__doc__)
            self._tokens[name] = token
            self._sym2spec[tokenType] = token

        nonterms, userStart = adapter.get_nonterminals()
        for nonterm in nonterms:
            name = nonterm.name
            nontermType = nonterm.nontermType
            if name in self._precedences:
                raise SpecError("Identical precedence/nonterm names: %s" % \
                                nontermType.__doc__)
            if name in self._tokens:
                raise SpecError("Identical token/nonterm names: %s with %s" % (name, nontermType.__doc__))
            if name in self._nonterms:
                raise SpecError("Duplicate nonterm name: [%s]%s" % (name, nontermType.__doc__))
            self._nonterms[name] = nonterm
            self._sym2spec[nontermType] = nonterm

        self._userStartSym = userStart
        if not isinstance(self._userStartSym, NontermSpec):
            raise SpecError("No start symbol specified")

    def aux_nonterm(self, name):
        if name in self._aux_nonterms:
            return self._aux_nonterms[name]
        else:
            def list_add(self, lst, x):
                lst.append(x)
                return lst

            original_name = name[:-1]
            variant = name[-1]
            prec = self._precedences['none']
            nt_class = Nonterm

            module_name = nt_class.__module__
            qualified = '%s.%s' % (module_name, name)
            nonterm = NontermSpec(name, Nonterm, qualified, prec)
            try:
                sym = self._nonterms[original_name]
            except KeyError:
                sym = self._tokens[original_name]
            if variant == '?':
                rules_rhs = [[], [sym]]
                reducers = [lambda self: RETURN_NONE, lambda self, x: x]
            elif variant == '*':
                rules_rhs = [[], [nonterm, sym]]
                reducers = [lambda self: [], list_add]
            elif variant == '+':
                rules_rhs = [[sym], [nonterm, sym]]
                reducers = [lambda self, x: [x], list_add]
            else:
                assert False, variant
            # do stuff
            for i, (rhs, reducer) in enumerate(zip(rules_rhs, reducers)):
                prod = Production(
                    reducer, "%s._%d" % (qualified, i),
                    prec, nonterm, rhs)
                assert prod not in nonterm.productions
                nonterm.productions.append(prod)
                self._productions.append(prod)
            self._aux_nonterms[name] = nonterm
            return nonterm

    # Resolve all symbolic (named) references.
    def _references(self, logFile, graphFile):
        # Build the graph of Precedence relationships.
        self._resolvePrec(graphFile)

        # Resolve Token-->Precedence references.
        for token in six.itervalues(self._tokens):
            if type(token.prec) == str:
                token.prec = self._precedences[token.prec]

        # Resolve Nonterm-->Precedence references.
        for nonterm in six.itervalues(self._nonterms):
            if type(nonterm.prec) == str:
                nonterm.prec = self._precedences[nonterm.prec]

        # Resolve Nonterm-->{Nonterm,Token,Precedence} references.
        for nonterm in six.itervalues(self._nonterms):
            d = nonterm.nontermType.__dict__
            for member_name in d:
                v = d[member_name]
                if isinstance(v, types.FunctionType) and isinstance(v.__doc__, str):
                    dirtoks = v.__doc__.split(" ")
                    if dirtoks[0] == "%reduce":
                        rhs = []
                        rhs_terms = []
                        prec = None
                        for i in range(1, len(dirtoks)):
                            tok = dirtoks[i]
                            m = NontermSpec.token_re.match(tok)
                            if m:
                                # Symbolic reference.
                                if tok in self._tokens:
                                    # token
                                    rhs.append(self._tokens[tok])
                                    rhs_terms.append(self._tokens[tok])
                                elif tok in self._nonterms:
                                    # nonterminal
                                    rhs.append(self._nonterms[tok])
                                elif tok[-1] in '?+*' and (
                                                tok[:-1] in self._nonterms or
                                                tok[:-1] in self._tokens):
                                    # aux symbol
                                    rhs.append(self.aux_nonterm(tok))
                                else:
                                    raise SpecError(("Unknown symbol '%s' in reduction " \
                                                     + "specification: %s") % (tok, v.__doc__))
                            else:
                                m = NontermSpec.precedence_tok_re.match(tok)
                                if m:
                                    # Precedence.
                                    if i < len(dirtoks) - 1:
                                        raise SpecError(("Precedence must come last in " \
                                                         + "reduction specification: %s") % \
                                                        v.__doc__)
                                    if m.group(1) not in self._precedences:
                                        raise SpecError(("Unknown precedence in reduction " \
                                                         + "specification: %s") % v.__doc__)
                                    prec = self._precedences[m.group(1)]

                        if prec is None:
                            # Inherit the non-terminal's precedence.
                            if rhs_terms:
                                # Inherit the precedence of the last terminal symbol in rhs
                                prec = rhs_terms[-1].prec
                            else:
                                # Inherit the non-terminal's precedence.
                                prec = nonterm.prec

                        prod = Production(v, "%s.%s" % (nonterm.qualified, member_name), \
                                          prec, nonterm, rhs)
                        assert prod not in nonterm.productions
                        nonterm.productions.append(prod)
                        self._productions.append(prod)
        self._nonterms.update(self._aux_nonterms)
        if self._verbose:
            ntokens = len(self._tokens) - 1
            nnonterms = len(self._nonterms) - 1
            nproductions = len(self._productions) - 1
            print("Parsing.Spec: %d token%s, %d non-terminal%s, %d production%s" % \
                  (ntokens, ("s", "")[ntokens == 1], \
                   nnonterms, ("s", "")[nnonterms == 1], \
                   nproductions, ("s", "")[nproductions == 1]))

    # Build the graph of Precedence relationships.
    def _resolvePrec(self, graphFile):
        # Resolve symbolic references and populate equiv/dominators.
        for precA in six.itervalues(self._precedences):
            for precBName in precA.relationships:
                if precBName not in self._precedences:
                    raise SpecError(("Precedence '%s' specifies a relationship with " + \
                                     "unknown Precedence '%s'") % (precA, precBName))
                precB = self._precedences[precBName]
                rel = precA.relationships[precBName]
                if rel == "=":
                    if precB not in precA.equiv:
                        precA.equiv.add(precB)
                        precA.equiv.update(precB.equiv)
                        precA.dominators.update(precB.dominators)
                        for precC in precB.equiv.copy():
                            precC.equiv = precA.equiv
                            precC.dominators = precA.dominators
                        assert precB.equiv is precA.equiv
                        assert precB.dominators is precA.dominators
                elif rel == "<":
                    precA.dominators.add(precB)
                elif rel == ">":
                    precB.dominators.add(precA)
                else:
                    assert False

        equiv_classes = {}
        for prec in six.itervalues(self._precedences):
            equiv_classes[id(prec.equiv)] = next(iter(prec.equiv))

        # Write graphviz precedence graph to graphFile, if graphFile was
        # specified.
        if graphFile != None:
            f = open(graphFile, "w+")
            if self._verbose:
                print("Parsing.Spec: Writing graphviz precedence graph to '%s'..." \
                      % graphFile)
            print('digraph Precedence {', file=f)
            print('    graph [bgcolor=black, labeljust="l"]', file=f)
            print(('    node [shape=record, style=filled, color=black, ' + \
                   'fillcolor=gray, fontname=Helvetica, fontsize=10.0]'), file=f)
            print('    edge [color=gray]', file=f)
            for precA in six.itervalues(self._precedences):
                if precA is next(iter(precA.equiv)):
                    print(('    Precedence_%s [label="{%s}"]') % (precA.name, \
                                                                  "\\n".join(["%s (%s)" % (p.name, p.assoc) \
                                                                              for p in precA.equiv])), file=f)
                    for precB in precA.dominators:
                        print('    Precedence_%s -> Precedence_%s' % \
                              (next(iter(precB.equiv)).name, \
                               next(iter(precA.equiv)).name), file=f)
            print('}', file=f)
            f.close()

        # Iteratively build dominator sets until no more work can be done.
        done = False
        while not done:
            done = True
            for precA in six.itervalues(equiv_classes):
                for precB in precA.dominators.copy():
                    diff = precB.equiv - precA.dominators
                    if diff:
                        precA.dominators.update(diff)
                        done = False
                    for precC in precB.dominators:
                        diff = precC.equiv - precA.dominators
                        if diff:
                            precA.dominators.update(diff)
                            done = False

        # Check for cycles in the graph.
        cycles = []
        for precA in six.itervalues(self._precedences):
            for precB in set((precA,)) | precA.equiv:
                if precB in precA.dominators:
                    cycles.append( \
                        "Precedence relationship cycle involving '%s'" % \
                        precA.name)
        if len(cycles) > 0:
            raise SpecError("\n".join(cycles))

    # Store state to a pickle file, if requested.
    def _pickle(self, file, mode):
        if self._skinny:
            # Discard data that don't need to be pickled.
            del self._startSym
            del self._startProd
            del self._itemSets
            del self._itemSetsHash
            del self._startState

        if file != None and "w" in mode:
            if self._verbose:
                print("Parsing.Spec: Creating %s Spec pickle in %s..." % \
                      (("fat", "skinny")[self._skinny], file))
            f = open(file, "wb")
            six.moves.cPickle.dump(self, f, protocol=six.moves.cPickle.HIGHEST_PROTOCOL)
            f.close()

    # Restore state from a pickle file, if a compatible one is provided.  This
    # method uses the same set of return values as does _compatible().
    def _unpickle(self, file, mode):
        if file != None and "r" in mode:
            if self._verbose:
                print("Parsing.Spec: Attempting to use pickle from file \"%s\"..." \
                      % file)
            try:
                f = open(file, "rb")
            except IOError:
                if self._verbose:
                    error = sys.exc_info()
                    print("Parsing.Spec: Pickle open failed: Exception %s: %s" \
                          % (error[0], error[1]))
                return "incompatible"

            # Any exception at all in unpickling can be assumed to be due to
            # an incompatible pickle.
            try:
                spec = six.moves.cPickle.load(f)
            except:
                if self._verbose:
                    error = sys.exc_info()
                    print("Parsing.Spec: Pickle load failed: Exception %s: %s" \
                          % (error[0], error[1]))
                return "incompatible"

            compat = self._compatible(spec)
            if compat == "incompatible":
                if self._verbose:
                    print("Parsing.Spec: Pickle in \"%s\" is incompatible." % \
                          file)
                return compat

            if self._verbose:
                print("Parsing.Spec: Using %s pickle in \"%s\" (%s)..." \
                      % (("fat", "skinny")[spec._skinny], file, compat))

            if compat in ["compatible", "repickle"]:
                # Copy spec's data structures.
                self._precedences = spec._precedences
                self._action = spec._action
                self._goto = spec._goto
                if not self._skinny:
                    self._startState = spec._startState
                self._nActions = spec._nActions
                self._nConflicts = spec._nConflicts
                self._nImpure = spec._nImpure
            elif compat == "itemsets":
                # Precedences are incompatible, so great care has to be taken
                # when copying from the pickle.  Overwrite all precedence
                # specifications in spec with the new ones, then copy over all
                # of the new symbols/productions (but not the new precedences,
                # of course).  This still leaves table generation, which is
                # done by the _prepare() method later.

                # Nonterminals.
                for key in self._nonterms:
                    nontermSelf = self._nonterms[key]
                    nontermSpec = spec._nonterms[key]
                    nontermSpec.prec = nontermSelf.prec
                    # Productions.
                    for prodSelf in nontermSelf.productions:
                        for prodSpec in nontermSpec.productions:
                            if prodSelf.qualified == prodSpec.qualified:
                                prodSpec.prec = prodSelf.prec
                                break
                        assert prodSelf.qualified == prodSpec.qualified
                # Tokens.
                for key in self._tokens:
                    tokenSelf = self._tokens[key]
                    tokenSpec = spec._tokens[key]
                    tokenSpec.prec = tokenSelf.prec
            else:
                assert False

            # Copy spec data structures that are usable regardless of whether
            # the parsing tables need to be rebuilt.
            self._nonterms = spec._nonterms
            self._tokens = spec._tokens
            self._sym2spec = spec._sym2spec
            self._productions = spec._productions
            self._userStartSym = spec._userStartSym
            if not self._skinny:
                self._startSym = spec._startSym
                self._startProd = spec._startProd
                self._itemSets = spec._itemSets
                self._itemSetsHash = spec._itemSetsHash

            return compat
        else:
            return "incompatible"

    # Determine whether other is compatible with self.  Note that self is not
    # completely initialized; the idea here is to determine whether other's
    # data structures can be copied *before* doing the work of building parsing
    # tables.
    #
    # Itemsets and precedences are not directly related, other than that
    # symbols have precedences associated with them.  Therefore, we check for
    # the following cases:
    #
    #   "compatible" : Completely compatible.
    #
    #   "repickle" : Compatible, but pickle needs to be regenerated.
    #
    #   "itemsets" : Itemsets are compatible, but precedence specifications are
    #                not.
    #
    #   "incompatible" : No useful compatibility.
    def _compatible(self, other):
        ret = "compatible"

        if (not self._skinny) and other._skinny:
            return "incompatible"
        elif self._skinny != other._skinny:
            ret = "repickle"

        # Precedences.
        if len(self._precedences) != len(other._precedences):
            if self._verbose:
                print("Parsing.Spec: Unequal number of precedences (%d vs %d)" \
                      % (len(self._precedences), len(other._precedences)))
            ret = "itemsets"
        for key in sorted(self._precedences):
            if key not in other._precedences:
                if self._verbose:
                    print("Parsing.Spec: Missing precedence: %s" % key)
                ret = "itemsets"
                continue
            precA = self._precedences[key]
            precB = other._precedences[key]
            if precA.name != precB.name \
                    or precA.assoc != precB.assoc \
                    or len(precA.relationships) != len(precB.relationships):
                if self._verbose:
                    print("Parsing.Spec: Incompatible precedences: %r vs. %r" \
                          % (precA, precB))
                ret = "itemsets"
                continue
            for prec in precA.relationships:
                rel = precA.relationships[prec]
                if prec not in precB.relationships \
                        or precB.relationships[prec] != rel:
                    if self._verbose:
                        print("Parsing.Spec: Incompatible precedences: %r vs. %r" \
                              % (precA, precB))
                    ret = "itemsets"
                    break

        # Nonterminals.
        if len(self._nonterms) != len(other._nonterms):
            if self._verbose:
                print("Parsing.Spec: Unequal number of non-terminals (%d vs %d)" \
                      % (len(self._nonterms), len(other._nonterms)))
            return "incompatible"
        for key in self._nonterms:
            if key not in other._nonterms:
                if self._verbose:
                    print("Parsing.Spec: Missing non-terminal: %s" % key)
                return "incompatible"
            nontermA = self._nonterms[key]
            nontermB = other._nonterms[key]
            if nontermA.name != nontermB.name \
                    or nontermA.qualified != nontermB.qualified \
                    or nontermA.nontermType != nontermB.nontermType:
                if self._verbose:
                    print("Parsing.Spec: Incompatible non-terminals: %r vs. %r" \
                          % (nontermA, nontermB))
                return "incompatible"
            if nontermA.prec.name != nontermB.prec.name:
                if self._verbose:
                    print(("Parsing.Spec: Differing precedences for " + \
                           "non-terminal: %r") % nontermA)
                ret = "itemsets"

            # Productions.
            if len(nontermA.productions) != len(nontermB.productions):
                if self._verbose:
                    print("Parsing.Spec: Unequal number of productions (%d vs %d)" \
                          % (len(self._productions) - 1, \
                             len(other._productions) - 1))
                return "incompatible"
            for prodA in nontermA.productions:
                match = False
                for prodB in nontermB.productions:
                    if prodA.qualified == prodB.qualified \
                            and prodA.lhs.name == prodB.lhs.name \
                            and len(prodA.rhs) == len(prodB.rhs):
                        match = True
                        for i in range(len(prodA.rhs)):
                            if prodA.rhs[i].name != prodB.rhs[i].name:
                                match = False
                                if self._verbose:
                                    print(("Parsing.Spec: Incompatible" + \
                                           " productions: %r vs. %r") \
                                          % (prodA, prodB))
                                break
                        if prodA.prec.name != prodB.prec.name:
                            if self._verbose:
                                print(("Parsing.Spec: Differing precedences " + \
                                       "for production: %r") % prodA)
                            ret = "itemsets"
                if not match:
                    return "incompatible"

        # Tokens.
        if len(self._tokens) != len(other._tokens):
            if self._verbose:
                print("Parsing.Spec: Unequal number of tokens (%d vs %d)" \
                      % (len(self._tokens), len(other._tokens)))
            return "incompatible"
        for key in self._tokens:
            if key not in other._tokens:
                if self._verbose:
                    print("Parsing.Spec: Missing token: %s" % key)
                return "incompatible"
            tokenA = self._tokens[key]
            tokenB = other._tokens[key]
            if tokenA.name != tokenB.name \
                    or tokenA.tokenType != tokenB.tokenType:
                if self._verbose:
                    print(type(tokenA.tokenType), type(tokenB.tokenType), tokenA.tokenType == tokenB.tokenType)
                    print("Parsing.Spec: Incompatible tokens: %r vs. %r" \
                          % (tokenA, tokenB))
                return "incompatible"
            if tokenA.prec.name != tokenB.prec.name:
                if self._verbose:
                    print("Parsing.Spec: Differing precedences for token: %r" \
                          % tokenA)
                ret = "itemsets"

        # User start symbol.
        if self._userStartSym.name != other._userStartSym.name:
            if self._verbose:
                print("Parsing.Spec: Differing start symbols: %s vs. %s" \
                      % (self._userStartSym.name, other._userStartSym.name))
            return "incompatible"

        if other._skinny and ret == "itemsets":
            # The itemsets have to be regenerated, since they weren't pickled.
            ret = "incompatible"
        return ret

    # Check for unused prececence/token/nonterm/reduce specifications, then
    # throw a SpecError if any ambiguities exist in the grammar.
    def _validate(self, logFile):
        if self._verbose:
            print("Parsing.Spec: Validating grammar...")

        lines = []
        if self._nConflicts > 0:
            lines.append("Parsing.Spec: %d unresolvable conflict%s" % \
                         (self._nConflicts, ("s", "")[self._nConflicts == 1]))

        # Previous code guarantees that all precedence/token/nonterm names are
        # unique.  Therefore, we can build a single dictionary here that keys on
        # names.
        used = {}
        productions = []
        for itemSet in self._itemSets:
            for item in itemSet:
                productions.append(item.production)
                used[item.production.prec.name] = item.production.prec
                for sym in [item.production.lhs] + item.production.rhs:
                    used[sym.name] = sym
                    used[sym.prec.name] = sym.prec

                for token in six.iterkeys(item.lookahead):
                    used[token.prec.name] = token.prec

        nUnused = 0

        # Precedences.
        for prec in self._precedences:
            if prec not in [self._none.name, self._split.name]:
                if prec not in used:
                    nUnused += 1
                    lines.append("Parsing.Spec: Unused precedence: %r" % \
                                 self._precedences[prec])

        # Tokens.
        for token in self._tokens:
            if token not in [eoi.name, epsilon.name]:
                if token not in used:
                    nUnused += 1
                    lines.append("Parsing.Spec: Unused token: %s" % \
                                 self._tokens[token])

        # Nonterms.
        for nonterm in self._nonterms:
            if nonterm not in [self._startSym.name]:
                if nonterm not in used:
                    nUnused += 1
                    lines.append("Parsing.Spec: Unused nonterm: %s" % \
                                 self._nonterms[nonterm])

        # Productions.
        for production in self._productions:
            if production not in productions:
                nUnused += 1
                lines.append("Parsing.Spec: Unused production: %r" % production)

        if nUnused > 0:
            lines.insert((1, 0)[self._nConflicts == 0], \
                         "Parsing.Spec: %d unused definition%s" % \
                         (nUnused, ("s", "")[nUnused == 1]))

        # Write to logFile, if one was specified.
        if logFile != None:
            f = open(logFile, "w+")
            if self._verbose:
                print("Parsing.Spec: Writing log to '%s'..." % logFile)
            f.write("%s" % "\n".join(lines + ["%r" % self]))
            f.close()

        # Conflicts are fatal.
        if self._nConflicts > 0:
            raise SpecError("%s" % ("\n".join(lines)))

        # Make sure to let the user know about unused symbols if verbosity is
        # enabled, and there weren't any conflicts to cause notification via an
        # exception.
        if self._verbose:
            def count(n, noun):
                if n == 1:
                    return '%d %s'%(n, noun)
                else:
                    return '%d %ss'%(n, noun)
            ntokens = len(self._tokens) - 1
            nnonterms = len(self._nonterms) - 1
            nproductions = len(self._productions) - 1
            lines.append(
                "Parsing.Spec: %s, %s, %s"%(
                    count(ntokens, "token"),
                    count(nnonterms, "non-terminal"),
                    count(nproductions, "production")))
            sys.stdout.write("%s\n" % "\n".join(lines))

    # Compute the first sets for all symbols.
    def _firstSets(self):
        # Terminals.
        # first(X) is X for terminals.
        for sym in six.itervalues(self._tokens):
            sym.firstSetMerge(sym)

        # Non-terminals.
        #
        # Repeat the following loop until no more symbols can be added to any
        # first set.
        done = False
        while not done:
            done = True
            for name in self._nonterms:
                sym = self._nonterms[name]
                for prod in sym.productions:
                    # Merge epsilon if there is an empty production.
                    if len(prod.rhs) == 0:
                        if not sym.firstSetMerge(epsilon):
                            done = False

                    # Iterate through the RHS and merge the first sets into
                    # this symbol's, until a preceding symbol's first set does
                    # not contain epsilon.
                    for elm in prod.rhs:
                        containsEpsilon = False
                        for elmSym in elm.firstSet:
                            if not sym.firstSetMerge(elmSym):
                                done = False
                            if elmSym == epsilon:
                                containsEpsilon = True
                        if not containsEpsilon:
                            break

    # Compute the follow sets for all symbols.
    def _followSets(self):
        self._startSym.followSet = [epsilon]

        # Repeat the following loop until no more symbols can be added to any
        # follow set.
        done = False
        while not done:
            done = True
            for name in self._nonterms:
                sym = self._nonterms[name]
                for prod in sym.productions:
                    # For all A ::= aBb, merge first(b) into follow(B).
                    for i in range(len(prod.rhs) - 1):
                        for j in range(i + 1, len(prod.rhs)):
                            if not prod.rhs[i].followSetMerge(
                                    prod.rhs[j].firstSet):
                                done = False
                            if epsilon not in prod.rhs[j].firstSet:
                                break

                    # For A ::= ab, or A ::= aBb where first(b) contains <e>,
                    # merge follow(A) into follow(B).
                    for i in range(len(prod.rhs) - 1, -1, -1):
                        if not prod.rhs[i].followSetMerge(prod.lhs.followSet):
                            done = False
                        if epsilon not in prod.rhs[i].firstSet:
                            break

    # Compute the collection of sets of LR(1) items.
    def _items(self):
        # Add {[S' ::= * S $., <e>]} to _itemSets.
        tItemSet = ItemSet()
        tItem = Item(self._startProd, 0, [epsilon])
        tItemSet.append(tItem)
        tItemSet.closure()
        self._itemSets.append(tItemSet)

        # List of state numbers that need to be processed.
        worklist = [0]
        if self._verbose:
            nwork = len(worklist)
            print("Parsing.Spec: Generating LR(1) itemset collection... ", end=' ')
            sys.stdout.write("+")
            sys.stdout.flush()

        # itemSetsHash uses itemsets as keys.  A value is a list of _itemSets
        # indices; these itemsets are the ones referred to the key itemset.
        itemSetsHash = {tItemSet: [0]}

        syms = list(self._tokens.values()) + list(self._nonterms.values())
        while len(worklist) > 0:
            if self._verbose:
                if abs(len(worklist) - nwork) >= 10:
                    nwork = len(worklist)
                    sys.stdout.write("[%d/%d]" % \
                                     (len(worklist), len(self._itemSets)))
                    sys.stdout.flush()

            i = worklist.pop(0)
            itemSet = self._itemSets[i]
            for sym in syms:
                gotoSet = itemSet.goto(sym)
                if len(gotoSet) > 0:
                    merged = False
                    if gotoSet in itemSetsHash:
                        for j in itemSetsHash[gotoSet]:
                            mergeSet = self._itemSets[j]
                            if mergeSet.weakCompat(gotoSet):
                                merged = True
                                if mergeSet.merge(gotoSet):
                                    # Process worklist in MRU order.  This
                                    # causes a depth-first traversal.
                                    if j in worklist:
                                        worklist.remove(j)
                                    else:
                                        if self._verbose:
                                            sys.stdout.write(".")
                                            sys.stdout.flush()
                                    worklist.insert(0, j)
                                break
                    if not merged:
                        gotoSet.closure()
                        worklist.append(len(self._itemSets))
                        if gotoSet not in itemSetsHash:
                            itemSetsHash[gotoSet] = [len(self._itemSets)]
                        else:
                            itemSetsHash[gotoSet].append(len(self._itemSets))
                        self._itemSets.append(gotoSet)
                        if self._verbose:
                            sys.stdout.write("+")
                            sys.stdout.flush()

        if self._verbose:
            sys.stdout.write("\n")
            sys.stdout.flush()
        self._itemSetsHash = itemSetsHash

    # Compute LR parsing tables.
    def _lr(self):
        # The collection of sets of LR(1) items already exists.
        assert len(self._itemSets) > 0
        assert len(self._action) == 0
        assert len(self._goto) == 0
        assert self._startState is None
        assert self._nConflicts == 0

        if self._verbose:
            print("Parsing.Spec: Generating LR(1) parsing tables (%d state%s)... " \
                  % (len(self._itemSets), \
                     ("s", "")[len(self._itemSets) == 1]), end=' ')
            sys.stdout.flush()

        itemSetsHash = self._itemSetsHash

        for itemSet in self._itemSets:
            if self._verbose:
                sys.stdout.write(".")
                sys.stdout.flush()
            # ===================================================================
            # _action.
            state = {}
            self._action.append(state)
            for item in itemSet:
                # X ::= a*Ab
                if item.dotPos < len(item.production.rhs):
                    sym = item.production.rhs[item.dotPos]
                    if isinstance(sym, TokenSpec):
                        itemSetB = itemSet.goto(sym)
                        for i in itemSetsHash[itemSetB]:
                            itemSetC = self._itemSets[i]
                            if itemSetC.weakCompat(itemSetB):
                                self._actionAppend(state, sym, ShiftAction(i))
                                break

                    # Check if this is the start state.
                    if self._startState is None \
                            and item.production.lhs == self._startSym \
                            and item.dotPos == 0:
                        assert len(item.production.rhs) == 2
                        self._startState = len(self._action) - 1
                # X ::= a*
                elif item.dotPos == len(item.production.rhs):
                    for lookahead in six.iterkeys(item.lookahead):
                        self._actionAppend(state, lookahead, \
                                           ReduceAction(item.production))
                else:
                    assert False
            # ===================================================================
            # _goto.
            state = {}
            self._goto.append(state)
            for nonterm in six.itervalues(self._nonterms):
                itemSetB = itemSet.goto(nonterm)
                if itemSetB in itemSetsHash:
                    for i in itemSetsHash[itemSetB]:
                        itemSetC = self._itemSets[i]
                        if itemSetC.weakCompat(itemSetB):
                            assert nonterm not in state
                            state[nonterm] = i
                            break

        if self._verbose:
            sys.stdout.write("\n")
            sys.stdout.flush()

    # Add a symbol action to state, if the action doesn't already exist.
    def _actionAppend(self, state, sym, action):
        assert type(state) == dict
        assert isinstance(sym, SymbolSpec)
        assert isinstance(action, Action)
        if not hasattr(sym, 'seq'):
            import ipdb
            ipdb.set_trace()

        if sym not in state:
            state[sym] = [action]
        else:
            actions = state[sym]
            if action not in actions:
                state[sym].append(action)
        for k in state:
            k.seq

    # Look for action ambiguities and resolve them if possible.
    def _disambiguate(self):
        assert self._nActions == 0
        assert self._nConflicts == 0
        assert self._nImpure == 0

        if self._verbose:
            print("Parsing.Spec: Disambiguating LR(1) parsing tables... ", end=' ')
            sys.stdout.flush()

        for stateInd in range(len(self._action)):
            state = self._action[stateInd]
            if self._verbose:
                vRes = "."
                vNConflicts = 0
            for sym in state:
                nConflicts = 0
                acts = [act for act in state[sym]]
                # Construct a list that corresponds to acts; each element
                # indicates whether to preserve the action.
                actStats = [True] * len(acts)

                # Fill in the cells of actStats.
                for i in range(len(acts)):
                    actI = acts[i]
                    for j in range(i + 1, len(acts)):
                        actJ = acts[j]
                        res = self._resolve(sym, actI, actJ)
                        if res == "neither":
                            actStats[i] = False
                            actStats[j] = False
                        elif res == "old":
                            actStats[j] = False
                        elif res == "both":
                            pass
                        elif res == "new":
                            actStats[i] = False
                        elif res == "err":
                            actStats[i] = False
                            actStats[j] = False
                            nConflicts += 1
                        else:
                            assert False

                # Look for actions that can coexist or dominate all other
                # actions.
                newActs = []
                for j in range(len(acts)):
                    if actStats[j]:
                        newActs.append(acts[j])
                # Replace the action set if there exists a valid resolution
                # among the actions.
                if len(newActs) > 0 or nConflicts == 0:
                    if self._verbose:
                        if len(newActs) != len(acts):
                            vRes = "_"
                    state[sym] = newActs
                    nConflicts = 0
                elif self._verbose:
                    vNConflicts += nConflicts

                nActions = len(state[sym])
                if nActions > 1:
                    nImpure = nActions
                else:
                    nImpure = 0

                # Update summary stats.
                self._nActions += nActions
                self._nConflicts += nConflicts
                self._nImpure += nImpure

            if self._verbose:
                if vNConflicts == 0:
                    sys.stdout.write("%s" % vRes)
                else:
                    sys.stdout.write("[%d:%d]" % (stateInd, vNConflicts))
                sys.stdout.flush()

        if self._verbose:
            sys.stdout.write("\n")
            sys.stdout.flush()

    # Compute how to resolve an action conflict.
    #
    # ret: "neither" : Discard both.
    #      "old"     : Keep old.
    #      "both"    : Keep both.
    #      "new"     : Keep new.
    #      "err"     : Unresolvable conflict.
    def _resolve(self, sym, oldAct, newAct):
        if type(oldAct) == ShiftAction:
            oldPrec = sym.prec
        elif type(oldAct) == ReduceAction:
            oldPrec = oldAct.production.prec
        else:
            assert False

        if type(newAct) == ShiftAction:
            newPrec = sym.prec
        elif type(newAct) == ReduceAction:
            newPrec = newAct.production.prec
        else:
            assert False

        if oldPrec in newPrec.dominators:
            # Discard new action.
            ret = "old"
        elif newPrec in oldPrec.dominators:
            # Discard old action.
            ret = "new"
        elif oldPrec in newPrec.equiv:
            assert newPrec in oldPrec.equiv

            if oldPrec.assoc == "split" or newPrec.assoc == "split":
                ret = "both"
            elif type(newAct) == type(oldAct):
                assert type(newAct) == ReduceAction
                assert type(oldAct) == ReduceAction
                # Fatal reduce/reduce conflict.
                ret = "err"
            else:
                if oldPrec.assoc != "fail" and newPrec.assoc != "fail" \
                        and oldPrec.assoc != newPrec.assoc:
                    # Conflicting associativity.
                    ret = "err"
                else:
                    # Determine associativity.  If only one of the actions has
                    # %fail associativity, it is overridden by the other.
                    if oldPrec.assoc == "fail":
                        assoc = newPrec.assoc
                    else:
                        assoc = oldPrec.assoc
                    assert assoc in ["fail", "nonassoc", "left", "right"]

                    if assoc == "fail":
                        ret = "err"
                    elif assoc == "left":
                        if type(oldAct) == ShiftAction:
                            ret = "new"
                        else:
                            assert type(newAct) == ShiftAction
                            ret = "old"
                    elif assoc == "right":
                        if type(oldAct) == ShiftAction:
                            ret = "old"
                        else:
                            assert type(newAct) == ShiftAction
                            ret = "new"
                    elif assoc == "nonassoc":
                        ret = "neither"
                    else:
                        assert False
        else:
            if newPrec in oldPrec.equiv:
                print("%r <--> %r" % (oldPrec, newPrec))
            assert newPrec not in oldPrec.equiv
            # No specified relationship between precedences.
            ret = "err"

        return ret


class Lr(object):
    """
LR(1) parser.  The Lr class uses a Spec instance in order to parse
input that is fed to it via the token() method, and terminated via the
eoi() method.
"""

    def __init__(self, spec):
        """
        Constructs a new LR parser

        :param spec: a grammar specification
        :type spec: Spec
        """
        if __debug__:
            if type(self) == Lr:
                assert spec.pureLR
        assert spec._nConflicts == 0
        self._spec = spec
        self.reset()
        self._verbose = False

    def __getSpec(self):
        return self._spec

    def __setSpec(self, spec):
        raise AttributeError()

    spec = property(__getSpec, __setSpec)

    def __getStart(self):
        return self._start

    def __setStart(self, start):
        raise AttributeError

    start = property(__getStart, __setStart, doc="""
A list of parsing results.  For LR parsing, there is only ever one
result, but for compatibility with the Glr interface, start is a
list.
""")

    def __getVerbose(self):
        return self._verbose

    def __setVerbose(self, verbose):
        assert type(verbose) == bool
        self._verbose = verbose

    verbose = property(__getVerbose, __setVerbose)

    def reset(self):
        self._start = None
        self._stack = [(Epsilon(epsilon), 0)]

    def token_from_class(self, cls, *args, **kwargs):
        token_spec = kwargs.get('token_spec')
        if token_spec is None:
            token_spec = self._spec._sym2spec[cls]
        else:
            del kwargs['token_spec']
        token = cls(token_spec, *args, **kwargs)
        self._act(token, token_spec)

    def token(self, token, token_spec=None):
        """
Feed a token to the parser.
"""
        if token_spec is None:
            token_spec = self._spec._sym2spec[type(token)]
        self._act(token, token_spec)

    def eoi(self):
        """
Signal end-of-input to the parser.
"""
        grammar_eoi = self._spec._sym2spec[EndOfInput]
        token = EndOfInput(grammar_eoi)
        self.token(token, grammar_eoi)

        assert self._stack[-1][0] == token  # <$>.
        if self._verbose:
            self._printStack()
            print("   --> accept")
        self._stack.pop()

        top = self._stack[-1]
        self._start = [self._stack[1][0]]
        assert self._start[0].symSpec == self._spec._userStartSym

    def _act(self, sym, symSpec):
        """
        performs (reduce) actions until the symbol is shifted onto
        the stack, then returns.

        :param sym: a node
        :param symSpec: a symbol spec
        """
        if self._verbose:
            self._printStack()
            print("INPUT: %r" % sym)

        while True:
            top = self._stack[-1]
            if symSpec not in self._spec._action[top[1]]:
                print(symSpec, hex(id(symSpec)), hex(hash(symSpec)))
                for k in self._spec._action[top[1]]:
                    print("action:", k, hex(id(k)), hex(hash(k)), k == symSpec)
                if hasattr(self._spec, 'itemSets'):
                    print(self._spec._itemSets[top[1]])
                print(self._spec._action[top[1]])
                print(self._spec._goto[top[1]])
                try:
                    offset = sym.range[0]
                except AttributeError:
                    offset = None
                raise UnexpectedToken(
                    "Unexpected token: %r" % sym, offset, sym)

            actions = self._spec._action[top[1]][symSpec]
            assert len(actions) == 1
            action = actions[0]

            if self._verbose:
                print("   --> %r" % action)
            if type(action) == ShiftAction:
                self._stack.append((sym, action.nextState))
                break
            else:
                assert type(action) == ReduceAction
                self._reduce(action.production)

            if self._verbose:
                self._printStack()

    def _printStack(self):
        print("STACK:", end=' ')
        for node in self._stack:
            print("%r" % node[0], end=' ')
        print()
        print("      ", end=' ')
        for node in self._stack:
            print("%r%s" % (node[1], \
                            (" " * (len("%r" % node[0]) - len("%r" % node[1])))), end=' ')
        print()

    def _reduce(self, production):
        nRhs = len(production.rhs)
        rhs = []
        for i in range(len(self._stack) - nRhs, len(self._stack)):
            rhs.append(self._stack[i][0])

        r = self._production(production, rhs)

        for i in range(nRhs):
            self._stack.pop()

        top = self._stack[-1]
        self._stack.append((r, self._spec._goto[top[1]][production.lhs]))

    def _production(self, production, rhs):
        sym = production.lhs.nontermType(production.lhs)
        sym.type = production.lhs.name
        if rhs:
            try:
                first_idx = 0
                last_idx = len(rhs) - 1
                # skip epsilon productions, look into lists (for x* and x+)
                while last_idx >= first_idx and not rhs[last_idx]:
                    last_idx -= 1
                while last_idx >= first_idx and not rhs[first_idx]:
                    first_idx += 1
                if last_idx >= first_idx:
                    last_rhs = rhs[last_idx]
                    if isinstance(last_rhs, list):
                        last_rhs = last_rhs[-1]
                    first_rhs = rhs[first_idx]
                    if isinstance(first_rhs, list):
                        first_rhs = first_rhs[0]
                    if first_rhs.range is not None and last_rhs.range is not None:
                        sym.range = [first_rhs.range[0], last_rhs.range[1]]
                    else:
                        sym.range = None
            except AttributeError:
                pass
        nRhs = len(rhs)
        assert nRhs == len(production.rhs)
        r = production.method(sym, *rhs)

        # Python's method definition syntax makes returning self from %reduce
        # methods cumbersome, so translate None here.
        if r is None:
            r = sym
        elif r is RETURN_NONE:
            r = None

        return r


# ===============================================================================
# Begin graph-structured stack (GSS) classes.
#

class Gss(list):
    """Graph-structured stack."""

    def __init__(self, glr):
        list.__init__(self)

        self._glr = glr


class Gsse(object):
    """Graph-structured stack edge."""

    def __init__(self, below, above, value):
        self.node = below
        above._edges.append(self)
        self.value = value

    def __repr__(self):
        return "{%r}" % self.value

    def __eq__(self, other):
        if self.node != other.node \
                or self.value != other.value:
            return False
        return True


class Gssn(object):
    """Graph-structured stack node."""

    def __init__(self, below, value, nextState):
        assert isinstance(below, Gssn) or below is None

        self._edges = []
        if below != None:
            Gsse(below, self, value)
        self.nextState = nextState

    def __repr__(self):
        return "[%d]" % self.nextState

    def __getEdge(self):
        assert len(self._edges) == 1
        return self._edges[0]

    def __setEdge(self):
        raise AttributeError

    edge = property(__getEdge, __setEdge)

    def edges(self):
        for edge in self._edges:
            yield edge

    def nodes(self):
        for edge in self._edges:
            yield edge.node

    # Iterate over all paths of length pathLen.  Path length is measured as the
    # number of edges in the path, so a path of length 0 still consists of a
    # single node.
    #
    # Each path is encoded as a list that alternates between nodes and edges,
    # where the first and last elements are always nodes.
    #
    # <e>-grammars can cause cycles, which requires that we avoid infinite
    # recursion.
    def paths(self, pathLen=None):
        assert ((type(pathLen) == int and pathLen >= 0) or pathLen is None)

        for path in self._pathsRecurse(pathLen, []):
            yield path

    def _pathsRecurse(self, pathLen, path):
        path.insert(0, self)
        if pathLen is None and len(self._edges) == 0:
            yield path[:]
        elif pathLen != None and len(path) - 1 == pathLen * 2:
            yield path[:]
        else:
            for edge in self.edges():
                # Avoid infinite recursion due to <e>-production cycles.
                if len(path) < 3 or edge != path[1]:
                    path.insert(0, edge)
                    for x in edge.node._pathsRecurse(pathLen, path):
                        yield x
                    path.pop(0)
        path.pop(0)


#
# End graph-structured stack (GSS) classes.
# ===============================================================================

class Glr(Lr):
    """
GLR parser.  The Glr class uses a Spec instance in order to parse input
that is fed to it via the token() method, and terminated via the eoi()
method.
"""

    def __init__(self, spec):
        Lr.__init__(self, spec)

    def reset(self):
        self._start = None

        # Initialize with a stack that is in the start state.
        self._gss = Gss(self)
        top = Gssn(None, None, 0)
        self._gss.append(top)

        self._paths = []

    def token(self, token, tokenSpec=None):
        """
Feed a token to the parser.
"""
        if self._verbose:
            print("%s" % ("-" * 80))
            print("INPUT: %r" % token)
        if tokenSpec is None:
            tokenSpec = self._spec._sym2spec[type(token)]
        self._act(token, tokenSpec)
        if len(self._gss) == 0:
            try:
                offset = token.range[0]
            except AttributeError:
                offset = None
            raise UnexpectedToken("Unexpected token: %r" % token, offset, token)

    def token_from_class(self, cls, *args, **kwargs):
        token_spec = kwargs.get('token_spec')
        if token_spec is None:
            token_spec = self._spec._sym2spec[cls]
        else:
            del kwargs['token_spec']
        token = cls(token_spec, *args, **kwargs)
        prev_gss = self._gss[:]
        self._act(token, token_spec)
        if len(self._gss) == 0:
            print("unexpected:", token_spec, token_spec.tokenType)
            for gss in prev_gss:
                print(gss, gss.nextState)
                action = self._spec._action[gss.nextState]
                for k in action.keys():
                   print(k, k.tokenType)
            try:
                offset = token.range[0]
            except AttributeError:
                offset = None
            raise UnexpectedToken("Unexpected token: %r" % token, offset, token)

    def eoi(self):
        """
Signal end-of-input to the parser.
"""
        grammar_eoi = self._spec._sym2spec[EndOfInput]
        token = EndOfInput(grammar_eoi)
        self.token(token, grammar_eoi)

        # Gather the start symbols from the stacks.
        self._start = []
        for top in self._gss:
            for path in top.paths():
                assert len(path) == 5
                if self._verbose:
                    print("   --> accept %r" % path)
                edge = path[1]
                assert isinstance(edge.value, Nonterm)
                assert edge.value.symSpec == self._spec._userStartSym
                self._start.append(edge.value)

        if len(self._start) == 0:
            raise SyntaxError("Unexpected end of input")

        if self._verbose:
            print("Start: %r" % self._start)
            print("%s" % ("-" * 80))

    def _act(self, sym, symSpec):
        self._reductions(sym, symSpec)
        self._shifts(sym, symSpec)

    def _reductions(self, sym, symSpec):
        # epsilons is a dictionary that maps production-->[tops].  The purpose
        # is to avoid repeating the same epsilon production on a particular
        # stack top.  Ordinary productions do not require this care because we
        # can notice when a path has already been used for a production.
        epsilons = {}

        if self._verbose:
            nReduces = 0

        # Enqueue work.
        workQ = []
        i = 0
        while i < len(self._gss):
            top = self._gss[i]
            if symSpec not in self._spec._action[top.nextState]:
                # Unexpected token for this stack.
                self._gss.pop(i)
            else:
                for action in self._spec._action[top.nextState][symSpec]:
                    if type(action) == ReduceAction:
                        if len(action.production.rhs) == 0:
                            if action.production not in epsilons:
                                assert len([path for path in top.paths(0)]) == 1
                                path = [p for p in top.paths(0)][0]
                                epsilons[action.production] = [top]
                                workQ.append((path, action.production))
                                if self._verbose:
                                    print("   --> enqueue(a) %r" % \
                                          action.production)
                                    print("                  %r" % path)
                            elif top not in epsilons[action.production]:
                                assert len([path for path in top.paths(0)]) == 1
                                path = [p for p in top.paths(0)][0]
                                epsilons[action.production].append(top)
                                workQ.append((path, action.production))
                                if self._verbose:
                                    print("   --> enqueue(b) %r" % \
                                          action.production)
                                    print("                  %r" % path)
                        else:
                            # Iterate over all reduction paths through stack and
                            # enqueue them.
                            for path in top.paths(len(action.production.rhs)):
                                workQ.append((path, action.production))
                                if self._verbose:
                                    print("   --> enqueue(c) %r" % \
                                          action.production)
                                    print("                  %r" % path)
                i += 1

        # Process the work queue.
        while len(workQ) > 0:
            (path, production) = workQ.pop(0)

            if self._verbose:
                print("   --> reduce %r" % production)
                print("              %r" % path)
                nReduces += 1

            self._reduce(workQ, epsilons, path, production, symSpec)

        if self._verbose:
            if nReduces > 0:
                self._printStack()

    def _reduce(self, workQ, epsilons, path, production, symSpec):
        assert len(path[1::2]) == len(production.rhs)

        # Build the list of RHS semantic values to pass to the reduction action.
        rhs = [edge.value for edge in path[1::2]]

        # Call the user reduction method.
        r = self._production(production, rhs)

        below = path[0]
        done = False
        for top in self._gss:
            if top.nextState == \
                    self._spec._goto[below.nextState][production.lhs]:
                # top is compatible with the reduction result we want to add to
                # the set of stack tops.
                for edge in top.edges():
                    if edge.node == below:
                        # There is already a below<--top link, so merge
                        # competing interpretations.
                        if self._verbose:
                            print("   --> merge %r <--> %r" % (edge.value, r))
                        value = production.lhs.nontermType.merge(edge.value, r)
                        if self._verbose:
                            if value == edge.value:
                                print("             %s" % \
                                      ("-" * len("%r" % edge.value)))
                            else:
                                print("             %s      %s" % \
                                      ((" " * len("%r" % edge.value)), \
                                       "-" * len("%r" % r)))
                        edge.value = value
                        done = True
                        break
                if not done:
                    # Create a new below<--top link.
                    edge = Gsse(below, top, r)
                    if self._verbose:
                        print("   --> shift(b) %r" % top)

                    # Enqueue reduction paths that were created as a result of
                    # the new link.
                    self._enqueueLimitedReductions(workQ, epsilons, edge, \
                                                   symSpec)
                    done = True
                break
        if not done:
            # There is no compatible stack top, so create a new one.
            top = Gssn(below, r, \
                       self._spec._goto[below.nextState][production.lhs])
            self._gss.append(top)
            if self._verbose:
                print("   --> shift(c) %r" % \
                      self._spec._goto[below.nextState][production.lhs])
            self._enqueueLimitedReductions(workQ, epsilons, top.edge, symSpec)

    # Enqueue paths that incorporate edge.
    def _enqueueLimitedReductions(self, workQ, epsilons, edge, symSpec):
        for top in self._gss:
            if symSpec in self._spec._action[top.nextState]:
                for action in self._spec._action[top.nextState][symSpec]:
                    if type(action) == ReduceAction:
                        if len(action.production.rhs) == 0:
                            if self._spec._goto[top.nextState] \
                                    [action.production.lhs] == top.nextState:
                                # Do nothing, since enqueueing a reduction
                                # would result in performing the same reduction
                                # twice.
                                pass
                            elif action.production not in epsilons:
                                path = [top]
                                epsilons[action.production] = [top]
                                workQ.append((path, action.production))
                                if self._verbose:
                                    print("   --> enqueue(d) %r" % \
                                          action.production)
                                    print("                  %r" % path)
                            elif top not in epsilons[action.production]:
                                path = [top]
                                epsilons[action.production].append(top)
                                workQ.append((path, action.production))
                                if self._verbose:
                                    print("   --> enqueue(e) %r" % \
                                          action.production)
                                    print("                  %r" % path)
                        else:
                            # Iterate over all reduction paths through stack and
                            # enqueue them if they incorporate edge.
                            for path in top.paths(len(action.production.rhs)):
                                if edge in path[1::2]:
                                    workQ.append((path, action.production))
                                    if self._verbose:
                                        print("   --> enqueue(f) %r" % \
                                              action.production)
                                        print("                  %r" % path)

    def _shifts(self, sym, symSpec):
        prevGss = self._gss
        self._gss = Gss(self)

        if self._verbose:
            nShifts = 0

        for topA in prevGss:
            if symSpec in self._spec._action[topA.nextState]:
                for action in self._spec._action[topA.nextState][symSpec]:
                    if type(action) == ShiftAction:
                        merged = False
                        for topB in self._gss:
                            if topB.nextState == topA.nextState:
                                Gsse(topA, topB, sym)
                                merged = True
                                break
                        if not merged:
                            top = Gssn(topA, sym, action.nextState)
                            self._gss.append(top)
                            if self._verbose:
                                print("   --> shift(a) %d" % action.nextState)
                                nShifts += 1
        if self._verbose:
            if nShifts > 0:
                self._printStack()

    def _printStack(self):
        i = 0
        for top in self._gss:
            for path in top.paths():
                if i == 0:
                    print("STK 0:", end=' ')
                else:
                    print("    %d:" % i, end=' ')
                for elm in path:
                    print("%r" % elm, end=' ')
                print()
                i += 1
