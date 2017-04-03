from builtins import object
from re import compile as re_compile, escape as re_escape

def is_symspec(symSpec):
    return hasattr(symSpec, 'name') and hasattr(symSpec, 'prec')

class Symbol(object):
    def __init__(self, symSpec):
        assert is_symspec(symSpec)
        self.symSpec = symSpec

    def __repr__(self):
        return "%r[]" % self.symSpec


class Nonterm(Symbol):
    """
Non-terminal symbols have sets of productions associated with them.  The
productions induce a parse forest on an input token stream.  There is
one special non-terminal, which is denoted via the %start directive,
whereas all other non-terminals are denoted via the %nonterm directive.
In addition to productions (%reduce directives associated with class
methods), the merge() method may be called during resolution of
ambiguous parses.  See the merge() documentation for further details.

Following are examples of how to specify non-terminal classes and their
associated productions:

  class E(Parsing.Nonterm):
      "%start E"
      def __init__(self):
          Parsing.Nonterm.__init__(self)
          # ...

      # Productions.
      def reduceA(self, E, plus, T):
          "%reduce E plus T [split]"
          print "%r ::= %r %r %r." % (self, E, plus, T)

      def reduceB(self, T):
          "%reduce T"

  class T(Parsing.Nonterm):
        "%nonterm" # Name implicitly same as class name.
      def reduceA(self, T, star, F):
          "%reduce T star F"

      def reduceB(self, F):
          "%reduce F [p1]"

  class F(Parsing.Nonterm):
      "%nonterm F [p2]"
      def reduceA(self, lparen, E, rparen):
          "%reduce lparen E rparen"

      def reduceB(self, id):
          "%reduce id"
"""
    def __init__(self, spec):
        # assert isinstance(parser, Lr)
        Symbol.__init__(self, spec)

    def __repr__(self):
        parts = []
        if hasattr(self, 'range'):
            parts.append('%d-%d'%(self.range[0], self.range[1]))
        if hasattr(self, 'val'):
            parts.append('val=%r'%(self.val,))
        return '%s[%s]'%(self.symSpec.name, ','.join(parts))

    def merge(self, other):
        """
Merging happens when there is an ambiguity in the input that allows
non-terminals to be part of multiple overlapping series of
reductions.  If no merge() method is specified, the parser will
throw a syntax error upon encountering an ambiguity that confounds
reduction processing.  However, it may be useful to either discard
one of the possible parses, or to explicitly record the ambiguity in
the data structures being created during parsing.  In both of these
cases, the non-terminal-specific merge() is the place to do the
work; merge() returns an object that is stored by the parser onto
the parse stack.  In the case where merge() discards one of the
possible parses, it need only return the parse that is to be
preserved (self or other).

If multiple merges are necessary, they cause a series of merge()
calls.  The first alternative (self) may be the result of a previous
merge() call, whereas other will not have not been merged yet
(unless as the result of merging further down in the parse forest).

The alternative that is discarded is never touched by the parser
again, so if any immediate cleanup is necessary, it should be done
in merge().
"""
        raise SyntaxError("No merge() for %r; merging %r <--> %r" % \
          (type(self), self, other))


class NontermStart(Nonterm):
    def reduce(self, userStartSym, eoi):
        pass



class Token(Symbol):
    """
Tokens are terminal symbols.  The parser is fed Token instances, which
is what drives parsing.  Typically, the user will define a class that
subclasses Parsing.Token and implement parser-specific machinery there,
then derive all actual token types from that class.

  class Token(Parsing.Token):
      def __init__(self, parser):
          Parsing.Token.__init__(self, parser)
          # ...

  class Plus(Token):
      "%token plus [p1]"

  class star(Token):
      "%token star [p2]" # Name implicitly same as class name.

  class lparen(Token):
      "%token [split]"

  class rparen(Token):
      "%token [none]" # [none] not necessary, since it's the default.

  class id(Token):
      "%token"
"""
    def __init__(self, spec):
        # assert isinstance(parser, Lr)
        Symbol.__init__(self, spec)

NOT_SET=object()

class ASTToken(Token):
    def __init__(self, spec, word, range, val=NOT_SET, **kwargs):
        Token.__init__(self, spec)
        self.type = spec.name
        self.word = word
        if val is NOT_SET:
            self.val = word
        else:
            self.val = val
        self.range = range
        self.__dict__.update(kwargs)

    def __repr__(self):
        return '%s[%d-%d,word=%s,val=%r]'%(
            self.symSpec.name, self.range[0], self.range[1],
            self.word, self.val)

class TokenBuilder(object):
    """
    carries infos for recognizing and building a token
    """
    def __init__(self, token_re, prec='none', convert=None, factory=None, keyword=None, name=None):
        self._re = token_re
        self._prec = prec
        self.convert = convert
        if factory is None:
            self.factory = ASTToken
        else:
            self.factory = factory
        self.keyword = keyword
        self.name = name

    def __hash__(self):
        return hash((
            self._re, self.name))

    def __eq__(self, other):
        """
        equality of two TokenBuilder objects, as needed for unpickling
        """
        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not self == other

    def __call__(self, symSpec, word, range, **kwargs):
        if self.convert is None:
            val = word
        else:
            val = self.convert(word)
        return self.factory(symSpec, word, range, val=val, **kwargs)

def is_token_factory(tokenType):
    if isinstance(tokenType, type) and issubclass(tokenType, Token):
        return True
    if isinstance(tokenType, TokenBuilder):
        return True
    return False

def mktoken(name, prec='none', re=None, s=None, tokens=None, keyword=None, convert=None):
    """
    creates a token class (that is then converted into a TokenSpec), i.e. this is
    a Token factory factory.

    :param name: the name of the token class
    :param prec: the precedence
    :param re: a regular expression describing the token
    :param s: a fixed string for the token
    :param tokens: a string containing a space-separated list of matching tokens
    :param keyword: a keyword is a string that is also matched by another RE
    :param convert: the function used to construct the semantic function
    :return:
    """
    token_re = None
    if re is not None:
        token_re = re
    elif s is not None:
        token_re = re_escape(s)
    elif tokens is not None:
        token_re = '(?:%s)'%('|'.join([re_escape(tok) for tok in tokens.split()]))
    else:
        token_re = None

    return TokenBuilder(token_re, prec, convert, keyword=keyword, name=name)

def print_ast(nonterm, indent=0):
    s_indent = ' ' * indent
    if isinstance(nonterm, Nonterm):
        print("%s%s[%s] %d-%d"%(
            s_indent, nonterm.type, type(nonterm).__name__,
            nonterm.range[0], nonterm.range[1]))
    else:
        print("%s%s[%s] %d-%d '%s'" % (
            s_indent, nonterm.type, type(nonterm).__name__,
            nonterm.range[0], nonterm.range[1],
            nonterm.word))
    for k in sorted(nonterm.__dict__.keys()):
        if k[0] != '_' and k not in ['type', 'range']:
            v = getattr(nonterm, k)
            if isinstance(v, Symbol):
                print('%s  %s:'%(s_indent, k))
                print_ast(v, indent+4)
            elif isinstance(v, list):
                print('%s  %s:'%(s_indent, k))
                for val in v:
                    if isinstance(val, Symbol):
                        print_ast(val, indent+4)
                    else:
                        print('%s - %r'%(s_indent, val))