from __future__ import print_function
import re
from parsing.ast_objs import Nonterm, TokenBuilder, is_token_factory
from parsing.grammar import Precedence, TokenSpec, NontermSpec, SpecError
from parsing.lr_automaton import Spec, Lr
from parsing.scanner import Scanner
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

first_cap_re = re.compile('(.)([A-Z][a-z]+)')
all_cap_re = re.compile('([a-z0-9])([A-Z])')


def snake_case(name):
    s1 = first_cap_re.sub(r'\1_\2', name)
    return all_cap_re.sub(r'\1_\2', s1).lower()

postfix = {'?': '_opt', '*': '_opt_list', '+': '_list'}

def interpret_doc_dsl(docstring, clsdict):
    """
    This function is called with the docstrings of Nonterm classes in order
    to allow shorthand notations for a few common patterns in AST construction.
    :param docstring:
    :param clsdict:
    :return:
    """
    if '%' not in docstring:
        return
    tokens = docstring.split()
    if tokens[0] == '%choice':
        for i, name in enumerate(tokens[1:]):
            last_char = name[-1]
            if last_char not in "?*+'":
                arg_name = snake_case(name)
                fn_name = 'r_' + arg_name
            elif last_char in '?*+':
                arg_name = snake_case(name[:-1])
                fn_name = 'r_'+ arg_name + postfix[name[-1]]
            elif last_char == "'":
                arg_name = '_'
                fn_name = 'r_const_' + hex(hash(name))
            assert NontermSpec.token_re.match(name)
            fn_src = 'def %(fn_name)s(self, %(arg_name)s):\n  "%%reduce %(name)s"\n  return %(arg_name)s'%locals()
            #print(fn_src)
            exec(fn_src, globals(), clsdict)
    elif tokens[0] == '%reduce':
        print("Reduce shorthand:", docstring)
        ruleno = [1]
        def make_rule(lst):
            assert lst[0] == '%reduce'
            fn_name = 'reduce_%d'%(ruleno[0],)
            arg_names = []
            for i, rhs in enumerate(lst[1:]):
                last_char = rhs[-1]
                if last_char == "'":
                    arg_name = '_'
                elif last_char in '+*?':
                    arg_name = snake_case(rhs[:-1])
                    if last_char in '+*':
                        # CheeseDeclaration+ => cheese_declarations
                        arg_name += 's'
                elif last_char in [']']:
                    # precedence declaration
                    continue
                else:
                    arg_name = snake_case(rhs)
                arg_suffix = 1
                orig_arg_name = arg_name
                if arg_name in ['type', 'range']:
                    arg_name += '_'
                while arg_name in arg_names:
                    arg_suffix += 1
                    arg_name = '%s%d'%(orig_arg_name, arg_suffix)
                arg_names.append(arg_name)
            lst_rhs = ' '.join(lst)
            argspec = ', '.join(arg_names)
            fn_src = ['def %(fn_name)s(self, %(argspec)s):'%locals(),
                      '  "%(lst_rhs)s"'%(locals())]
            for arg_name in arg_names:
                if arg_name[0] != '_':
                    fn_src.append('  self.%(arg_name)s = %(arg_name)s'%{'arg_name':arg_name})
            print('\n'.join(fn_src))
            exec('\n'.join(fn_src), globals(), clsdict)
            ruleno[0] += 1
        lst = []
        for tok in tokens:
            if tok == '%reduce':
                if lst:
                    make_rule(lst)
                lst = []
            lst.append(tok)
        if lst:
            make_rule(lst)


class NontermMetaclass(type):
    def __init__(cls, name, bases, clsdict):
        more_stuff = {}
        if '__doc__' in clsdict:
            doc = clsdict['__doc__']
            interpret_doc_dsl(doc, more_stuff)
        type.__init__(cls, name, bases, clsdict)
        gram_cls = cls._grammar_cls
        if name in gram_cls._nonterms:
            raise SpecError('duplicate Nonterm class %s'%(name,))
        for k, v in iteritems(more_stuff):
            print(k, type(v), isinstance(v, FunctionType))
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
        print(result)
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
