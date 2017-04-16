from __future__ import print_function
import types
import six
import re
import sys
from parsing.ast import Token, is_token_factory
from parsing.errors import SpecError

class Precedence(object):
    """
Precedences can be associated with tokens, non-terminals, and
productions.  Precedence isn't as important for GLR parsers as for LR
parsers, since GLR parsing allows for parse-time resolution of
ambiguity.  Still, precedence can be useful for reducing the volume of
ambiguities that must be dealt with at run-time.

There are five precedence types: %fail, %nonassoc, %left, %right, and
%split.  Each precedence can have relationships with other precedences:
<, >, or =.  These relationships specify a directed acyclic graph (DAG),
which is used to compute the transitive closures of relationships among
precedences.  If no path exists between two precedences that are
compared during conflict resolution, parser generation fails.  < and >
are reflexive; it does not matter which is used.  Conceptually, the =
relationship causes precedences to share a node in the DAG.

During conflict resolution, an error results if no path exists in the
DAG between the precedences under consideration.  When such a path
exists, the highest precedence non-terminal or production takes
precedence.  Associativity only comes into play for shift/reduce
conflicts, where the terminal and the production have equivalent
precedences (= relationship).  In this case, the non-terminal's
associativity determines how the conflict is resolved.

The %fail and %split associativities are special because they can be
mixed with other associativities.  During conflict resolution, if
another action has non-%fail associativity, then the %fail (lack of)
associativity is overridden.  Similarly, %split associativity overrides
any other associativity.  In contrast, any mixture of associativity
between %nonassoc/%left/%right causes an unresolvable conflict.

       %fail : Any conflict is a parser-generation-time error.

               A pre-defined precedence, [none], is provided.  It has
               %fail associativity, and has no pre-defined precedence
               relationships.

   %nonassoc : Resolve shift/reduce conflicts by removing both
               possibilities, thus making conflicts a parse-time error.

       %left : Resolve shift/reduce conflicts by reducing.

      %right : Resolve shift/reduce conflicts by shifting.

      %split : Do not resolve conflicts; the GLR algorithm will split
               the parse stack when necessary.

               A pre-defined precedence, [split], is provided.  It has
               %split associativity, and has no pre-defined precedence
               relationships.

By default, all symbols have [none] precedence.  Each production
inherits the precedence of its left-hand-side nonterminal's precedence
unless a precedence is manually specified for the production.

Following are some examples of how to specify precedence classes:

  class P1(Parsing.Precedence):
      "%split p1"

  class p2(Parsing.Precedence):
      "%left" # Name implicitly same as class name.

  class P3(Parsing.Precedence):
      "%left p3 >p2" # No whitespace is allowed between > and p2.

  class P4(Parsing.Precedence):
      "%left p4 =p3" # No whitespace is allowed between = and p3.
"""
    assoc_tok_re = re.compile(r'([<>=])([A-Za-z]\w*)')

    @classmethod
    def create(cls, name=None, precedence='fail', before=None, after=None):
        relationships = {}
        prec = cls(name, 'left', relationships)
        if before is not None:
            before.dominators.add(prec)
        if after is not None:
            prec.dominators.add(after)
        return prec

    @classmethod
    def left(cls, **kwargs):
        return cls.create(precedence='left', **kwargs)

    @classmethod
    def right(cls, **kwargs):
        return cls.create(precedence='right', **kwargs)

    @classmethod
    def nonassoc(cls, **kwargs):
        return cls.create(precedence='nonassoc', **kwargs)

    def __init__(self, name, assoc, relationships):
        assert assoc in ["fail", "nonassoc", "left", "right", "split"]
        assert type(relationships) == dict

        self.name = name
        self.assoc = assoc
        self.relationships = relationships # Raw relationships specification.

        self.equiv = set((self,)) # Set.  Precedences that have equivalent precedence.
        self.dominators = set() # Set.  Precedences that have higher precedence.

    def __eq__(self, other):
        if self.name != other.name:
            return False
        if self.assoc != other.assoc:
            return False
        if self.relationships != other.relationships:
            return False
        return True

    def __ne__(self, other):
        return not self == other

    def __repr__(self):
        equiv = [prec.name for prec in self.equiv]
        equiv.sort()
        domin = [prec.name for prec in self.dominators]
        domin.sort()
        return "[%%%s %s ={%s} <{%s}]" % (self.assoc, self.name, \
          ",".join(equiv), ",".join(domin))

none = Precedence('none', 'fail', {})

class SymbolSpec(str):
    seq_cur = 0
    def __new__(cls, name=None, *args, **kwargs):
        if name is None:
            name = cls.__name__
        assert type(name) == str, name
        result = str.__new__(cls, name)
        return result

    def __eq__(self, other):
        if self.name != other.name:
            return False
        if self.seq != other.seq:
            return False
        if self.prec is not other.prec and self.prec != other.prec:
            return False
        return True

    def __init__(self, name, prec):
        assert type(name) == str
        self.seq = SymbolSpec.seq_cur
        SymbolSpec.seq_cur += 1

        self.name = name
        self.prec = prec
        self.firstSet = set()
        self.followSet = set()

    def __repr__(self):
        return "%s" % self.name
    __str__ = __repr__

    def __getnewargs__(self):
        return (self.name,)

    def firstSetMerge(self, sym):
        if sym not in self.firstSet:
            self.firstSet.add(sym)
            return False
        else:
            return True

    def followSetMerge(self, set):
        ret = True
        for sym in set:
            if sym != epsilon and sym not in self.followSet:
                self.followSet.add(sym)
                ret = False
        return ret

in_h = False

# AKA terminal symbol.
class TokenSpec(SymbolSpec):
    def __init__(self, name, tokenType, prec):
        assert is_token_factory(tokenType)
        assert type(name) == str
        assert isinstance(prec, Precedence) or type(prec) == str

        SymbolSpec.__init__(self, name, prec)
        self.tokenType = tokenType

# <$>.
class EndOfInput(Token): pass

class EndOfInputSpec(TokenSpec):
    def __init__(self):
        TokenSpec.__init__(self, '<$>', EndOfInput, "none")
eoi = EndOfInputSpec()

# <e>.
class Epsilon(Token): pass
class EpsilonSpec(TokenSpec):
    def __init__(self):
        TokenSpec.__init__(self, "<e>", Epsilon, "none")
epsilon = EpsilonSpec()


class NontermSpec(SymbolSpec):
    token_re = re.compile(r"([A-Za-z][?+*]?\w*|'[^']+')")
    precedence_tok_re = re.compile(r'\[([A-Za-z]\w*)\]')

    def __init__(self, name, nontermType, qualified, prec):
        # assert issubclass(nontermType, Nonterm) # Add forward decl for Lyken.

        SymbolSpec.__init__(self, name, prec)

        self.qualified = qualified
        self.nontermType = nontermType
        self.productions = [] # Set.

    @classmethod
    def from_class(cls, nt_subclass, name=None, module=None):
        if name is None:
            name = nt_subclass.__name__
        if module is None:
            module_name = nt_subclass.__module__
        else:
            module_name = module.__name__
        if nt_subclass.__doc__ is None:
            dirtoks = ['%nonterm', name]
        else:
            dirtoks = nt_subclass.__doc__.strip().split()
        is_start = (dirtoks[0] == '%start')
        if dirtoks[0][0] == '%' and dirtoks[0] not in ['%nonterm', '%start']:
            dirtoks = ['%nonterm', name]
        symbol_name = None
        prec = None
        i = 1
        while i < len(dirtoks):
            tok = dirtoks[i]
            #print("from_class dirtok:", tok)
            if tok[0] == '%':
                if tok not in ['%start', '%nonterm']:
                    #print("special:", tok, name)
                    break
            m = NontermSpec.precedence_tok_re.match(tok)
            if m:
                if i < len(dirtoks) - 1 and dirtoks[i+1][0] != '%':
                    raise SpecError(("Precedence must come last in " \
                                     + "non-terminal specification: %s") % \
                                    nt_subclass.__doc__)
                prec = m.group(1)
            else:
                m = NontermSpec.token_re.match(tok)
                if m:
                    symbol_name = m.group(1)
                else:
                    raise SpecError("Invalid non-terminal specification: %s" \
                                    % nt_subclass.__doc__)
            i += 1
        if symbol_name is None:
            symbol_name = name
        if prec is None:
            prec = "none"

        nonterm = NontermSpec(symbol_name, nt_subclass,
                              "%s.%s" % (module_name, name), prec)
        return nonterm, is_start

    @classmethod
    def find_literal_tokens(cls, nt_subclass, literal_tokens):
        d = nt_subclass.__dict__
        for k in d:
            v = d[k]
            if isinstance(v, types.FunctionType) and isinstance(v.__doc__, str):
                dirtoks = v.__doc__.split(" ")
                if dirtoks[0] == "%reduce":
                    for i in range(1, len(dirtoks)):
                        tok = dirtoks[i]
                        m = NontermSpec.token_re.match(tok)
                        if m and tok[0] == "'":
                            while tok[-1] in '?*+':
                                tok = tok[:-1]
                            #print("find_literal_tokens:", tok, " in ", k)
                            literal_tokens.add(tok)



class Production(int):
    cur_seq = 0

    def __new__(cls, *args, **kwargs):
        result = int.__new__(cls, Production.cur_seq)
        result.seq = Production.cur_seq
        Production.cur_seq += 1
        return result

    def __init__(self, method, qualified, prec, lhs, rhs):
        assert isinstance(prec, Precedence)
        assert isinstance(lhs, NontermSpec)
        if __debug__:
            for elm in rhs:
                assert isinstance(elm, SymbolSpec)

        self.method = method
        self.qualified = qualified
        self.prec = prec
        self.lhs = lhs
        self.rhs = rhs

    def __getstate__(self):
        return (self.qualified, self.prec, self.lhs, self.rhs, self.seq)

    def __setstate__(self, data):
        # Convert qualified name to a function reference.
        (qualified, prec, lhs, rhs, seq) = data
        elms = qualified.split(".")
        method = sys.modules[elms[0]]
        for elm in elms[1:]:
            method = method.__dict__[elm]

        # Set state.
        self.method = method
        self.qualified = qualified
        self.prec = prec
        self.lhs = lhs
        self.rhs = rhs
        self.seq = seq

    def __repr__(self):
        return "%r ::= %s. [%s]" % \
          (self.lhs, " ".join(["%r" % elm for elm in self.rhs]), self.prec.name)

    # Optional callback method.
    #
    # Called when a production is reduced.
    def reduce(self, lhs, *rhs): pass

class Start(Production):
    def __init__(self, startSym, userStartSym):
        Production.__init__(self, none, startSym, userStartSym)

class Item(int):
    def __new__(cls, production, dotPos, lookahead):
        assert isinstance(production, Production)
        assert type(dotPos) == int
        assert dotPos >= 0
        assert dotPos <= len(production.rhs)
        assert type(lookahead) == list
        if __debug__:
            for elm in lookahead:
                assert isinstance(elm, SymbolSpec)

        hash = (dotPos * Production.cur_seq) + production.seq
        result = int.__new__(cls, hash)
        result.hash = hash
        result.production = production
        result.dotPos = dotPos
        result.lookahead = dict(list(six.moves.zip(lookahead, lookahead)))
        return result

    def __repr__(self):
        strs = []
        strs.append("[%r ::=" % self.production.lhs)
        assert self.dotPos <= len(self.production.rhs)
        i = 0
        while i < self.dotPos:
            strs.append(" %r" % self.production.rhs[i])
            i += 1
        strs.append(" *")
        while i < len(self.production.rhs):
            strs.append(" %r" % self.production.rhs[i])
            i += 1
        syms = [sym for sym in six.iterkeys(self.lookahead)]
        syms.sort()
        strs.append("., %s] [%s]" % \
          ("/".join(["%r" % sym for sym in syms]), \
          self.production.prec.name))

        return "".join(strs)

    def lr0__repr__(self):
        strs = []
        strs.append("%r ::=" % self.production.lhs)
        assert self.dotPos <= len(self.production.rhs)
        i = 0
        while i < self.dotPos:
            strs.append(" %r" % self.production.rhs[i])
            i += 1
        strs.append(" *")
        while i < len(self.production.rhs):
            strs.append(" %r" % self.production.rhs[i])
            i += 1
        strs.append(". [%s]" % self.production.prec.name)

        return "".join(strs)

    def lookaheadInsert(self, sym):
        assert isinstance(sym, SymbolSpec)
        self.lookahead[sym] = sym

    def lookaheadDisjoint(self, other):
        sLookahead = self.lookahead
        oLookahead = other.lookahead

        for sSym in six.iterkeys(sLookahead):
            if sSym in oLookahead:
                return False

        for oSym in six.iterkeys(oLookahead):
            if oSym in sLookahead:
                return False

        return True

    def __getnewargs__(self):
        return (self.production, self.dotPos, list(self.lookahead.keys()))
