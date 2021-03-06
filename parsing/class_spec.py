from __future__ import print_function
import re
from parsing.ast import Nonterm, TokenBuilder, is_token_factory
from parsing.grammar import Precedence, TokenSpec, NontermSpec, SpecError
from parsing.automaton import Spec, Lr
from parsing.scanner import Scanner
from parsing.ruledsl import interpret_docstring
from types import MethodType, FunctionType

from six import iteritems
from future.utils import with_metaclass

class GrammarMetaclass(type):
    def __init__(cls, name, bases, clsdict):
        for k, v in iteritems(clsdict):
            if hasattr(v, 'name'):
                if v.name is None:
                    v.name = k
                elif v.name != k:
                    assert "Names must match: %s / %s"%(v.name, k)
        type.__init__(cls, name, bases, clsdict)
        cls._nonterms = {}


class NontermMetaclass(type):
    def __init__(cls, name, bases, clsdict):
        more_stuff = {}
        if '__doc__' in clsdict:
            doc = clsdict['__doc__']
            interpret_docstring(doc, more_stuff, name)
        type.__init__(cls, name, bases, clsdict)
        gram_cls = cls._grammar_cls
        if name in gram_cls._nonterms:
            raise SpecError('duplicate Nonterm class %s'%(name,))
        for k, v in iteritems(more_stuff):
            #print(k, type(v), isinstance(v, FunctionType))
            setattr(cls, k, v)
        # the Nonterm base class is skipped
        if not (name == 'Nonterm' and len(
                    [x for x in list(clsdict.values()) if isinstance(x, MethodType)]) == 0):
            gram_cls._nonterms[name] = cls

keyword_re = re.compile('[a-z]+|[A-Z]+')

class Grammar(with_metaclass(GrammarMetaclass, object)):
    whitespace = '\s+'

    @classmethod
    def nonterm_base(cls):
        result = NontermMetaclass('Nonterm', (Nonterm,), {'_grammar_cls': cls})
        # register this as part of the grammar's module
        result.__module__ = cls.__module__
        return result

    @classmethod
    def get_precedences(cls):
        result = []
        for k, v in iteritems(cls.__dict__):
            if isinstance(v, Precedence):
                result.append(v)
        return result

    @classmethod
    def get_tokens(cls):
        if hasattr(cls, '_tokens'):
            return cls._tokens
        result = []
        for k, v in iteritems(cls.__dict__):
            if is_token_factory(v):
                if hasattr(v, '_prec'):
                    prec = v._prec
                else:
                    prec = 'none'
                result.append(TokenSpec(k, v, prec))
        literal_tokens = set()
        for k, v in iteritems(cls._nonterms):
            NontermSpec.find_literal_tokens(v, literal_tokens)
        for token in literal_tokens:
            clean_token = token[1:-1]
            if keyword_re.match(clean_token):
                keyword = clean_token
            else:
                keyword = None
            builder = TokenBuilder(re.escape(clean_token), keyword=keyword, name=token)
            result.append(TokenSpec(token, builder, 'none'))
        cls._tokens = result
        return result

    @classmethod
    def get_nonterminals(cls):
        result = []
        startSym = None
        for k, v in iteritems(cls._nonterms):
            nonterm, is_start = NontermSpec.from_class(v, k)
            result.append(nonterm)
            if is_start:
                if startSym is not None:
                    raise SpecError("Only one start non-terminal allowed: %s / %s" \
                                    % (v.__doc__, startSym))
                else:
                    startSym = nonterm
        return result, startSym

    @classmethod
    def get_scanner(cls):
        if hasattr(cls, '_scanner'):
            scanner = cls._scanner
        else:
            scanner = Scanner(cls.get_tokens(), cls.whitespace)
            cls._scanner = scanner
        return scanner

    @classmethod
    def feed(cls, string, parser):
        cls.get_scanner().scan(string, parser)

    @classmethod
    def spec(cls, *args, **kwargs):
        if hasattr(cls, '_spec'):
            return cls._spec
        else:
            spec = Spec(cls, *args, **kwargs)
            cls._spec = spec
            return spec

    @classmethod
    def parser(cls, *args, **kwargs):
        return Parser(cls, *args, **kwargs)

class Parser(Lr):
    def __init__(self, grammar, *args, **kwargs):
        self.grammar = grammar
        Lr.__init__(self, grammar.spec(*args, **kwargs))

    def last_state(self):
        top = self._stack[-1]
        prev = self._stack[-2]
        return (prev[0], prev[1], top[0], top[1])

    def expecting(self):
        prev = self._stack[-2]
        return self._spec._action[prev[1]]

    def expecting_goto(self):
        prev = self._stack[-2]
        return self._spec._goto[prev[1]]

    # Use the grammar-derived scanner.
    def scan(self, input):
        self.reset()
        self.grammar.feed(input, self)
        # Tell the parser that the end of input has been reached.
        self.eoi()
        return self.start[0]
