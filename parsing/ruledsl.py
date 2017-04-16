import re
from grammar import NontermSpec

special_re = re.compile('%([a-z]+)')
first_cap_re = re.compile('(.)([A-Z][a-z]+)')
all_cap_re = re.compile('([a-z0-9])([A-Z])')


def snake_case(name):
    s1 = first_cap_re.sub(r'\1_\2', name)
    return all_cap_re.sub(r'\1_\2', s1).lower()

postfix = {'?': '_opt', '*': '_opt_list', '+': '_list'}


def is_reduce_instr(s):
    if s == '%reduce':
        return True
    elif s.startswith('%reduce:'):
        return True


def reduce_instr_type(s):
    if s == '%reduce':
        return None
    elif s.startswith('%reduce:'):
        return s[8:]

class Generator(object):
    def __init__(self, clsdict, name):
        self.clsdict = clsdict
        self.name = name
        self.ruleno = 1

    def numbered_method(self, prefix='reduce'):
        val = self.ruleno
        self.ruleno = val + 1
        return '%s_%d'%(prefix, val)

    def add_method(self, lines):
        text = '\n'.join(lines)
        #print(text)
        exec text in globals(), self.clsdict

    def compile_reduce(self, lst):
        """
        A reduce rule is the equivalent of a normal
        reduce rule, but with the added functionality
        that AST attributes are derived semi-intelligently
        from the RHS
        """
        assert is_reduce_instr(lst[0])
        fn_type = reduce_instr_type(lst[0])
        fn_name = self.numbered_method()
        rhs_parts = []
        arg_names = []
        for i, rhs in enumerate(lst[1:]):
            last_char = rhs[-1]
            if last_char == "'":
                arg_name = '_'
            elif last_char in [']']:
                rhs_parts.append(rhs)
                continue
            elif last_char in '+*?':
                arg_name = snake_case(rhs[:-1])
                if last_char in '+*':
                    # CheeseDeclaration+ => cheese_declarations
                    arg_name += 's'
            else:
                arg_name = snake_case(rhs)
            rhs_parts.append(rhs)
            arg_suffix = 1
            orig_arg_name = arg_name
            if arg_name in ['type', 'range']:
                arg_name += '_'
            while arg_name in arg_names:
                arg_suffix += 1
                arg_name = '%s%d' % (orig_arg_name, arg_suffix)
            arg_names.append(arg_name)
        lst_rhs = ' '.join(['%reduce'] + lst[1:])
        argspec = ', '.join(arg_names)
        fillers = {
            'fn_name': fn_name, 'argspec': argspec,
            'lst_rhs': lst_rhs
        }
        fn_src = ['def %(fn_name)s(self, %(argspec)s):' % fillers,
                  '  "%(lst_rhs)s"' % fillers]
        if fn_type is not None:
            fn_src.append('  self.type = "%s"' % (fn_type,))
        for arg_name in arg_names:
            if arg_name[0] != '_':
                fn_src.append('  self.%(arg_name)s = %(arg_name)s' % {'arg_name': arg_name})
            self.add_method(fn_src)

    def compile_choice(self, lst):
        for i, name in enumerate(lst[1:]):
            last_char = name[-1]
            if last_char not in "?*+'":
                arg_name = snake_case(name)
                fn_name = 'r_' + arg_name
            elif last_char in '?*+':
                arg_name = snake_case(name[:-1])
                fn_name = 'r_' + arg_name + postfix[name[-1]]
            elif last_char == "'":
                arg_name = '_'
                fn_name = 'r_const_' + hex(hash(name))
            assert NontermSpec.token_re.match(name)
            fillers = {
                'fn_name': fn_name, 'arg_name': arg_name,
                'name': name
            }
            fn_src = [
                x % fillers for x in [
                    'def %(fn_name)s(self, %(arg_name)s):',
                    '  "%%reduce %(name)s"',
                    '  return %(arg_name)s']]
            self.add_method(fn_src)

    def compile_list(self, lst):
        assert len(lst) == 3
        if lst[2].startswith("'"):
            # simple list with ignorable separator
            fn_src = [
                'def reduce_single(item):',
                '  "%%reduce %s"' % (lst[1],),
                '  return [item]',
                'def reduce_multiple(lst, sep, item):',
                '  "%%reduce %s %s %s"' % (
                    self.name,
                    lst[2], lst[1]),
                '  return lst + [item]']
        else:
            # list with non-ignorable separator
            fn_src = [
                'def reduce_single(item):',
                '  "%%reduce %s"' % (lst[1],),
                '  return [item]',
                'def reduce_multiple(lst, sep, item):',
                '  "%%reduce %s %s %s"' % (
                    self.name,
                    lst[2], lst[1]),
                '  return lst + [sep, item]']
        self.add_method(fn_src)

    def compile_start(self, lst):
        assert len(lst) == 1

    def compile(self, lst):
        m = special_re.match(lst[0])
        instr = m.group(1)
        method = getattr(self, 'compile_'+instr)
        method(lst)

def interpret_docstring(docstring, clsdict, name):
    """
    This function is called with the docstrings of Nonterm classes in order
    to allow shorthand notations for a few common patterns in AST construction.

    :param docstring: the docstring to be interpreted
    :param clsdict: the class namespace
    :param name: the name of the class (for recursive rules such as list
    """
    if '%' not in docstring:
        return
    generator = Generator(clsdict, name)
    tokens = docstring.split()
    cur_group = []
    for token in tokens:
        if token[0] == '%':
            if cur_group:
                generator.compile(cur_group)
            cur_group = [token]
        else:
            cur_group.append(token)
    if cur_group:
        generator.compile(cur_group)
