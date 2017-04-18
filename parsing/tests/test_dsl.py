import unittest
from parsing.ast import print_ast

def conforms(ast, description):
    if isinstance(description, str):
        if ast == description:
            return
        if hasattr(ast, 'val') and ast.val == description:
            return
        if hasattr(ast, 'type') and ast.type == description:
            return
        assert False, (description, ast)
    elif isinstance(description, int):
        if hasattr(ast, 'val') and ast.val == description:
            return
        assert False, (description, ast)
    if isinstance(description, list):
        assert isinstance(ast, list), ast
        assert len(ast) == len(description), (ast, description)
        for ast_part, descr_part in zip(ast, description):
            conforms(ast_part, descr_part)
        return
    if 'type' in description:
        assert description['type'] == ast.type
    for k, v in description.items():
        if k != 'type':
            assert hasattr(ast, k), (k, ast)
            conforms(getattr(ast, k), v)

class TestParsingDSL(unittest.TestCase):
    def test_dsl_m(self):
        from parsing.tests.specs.m import GrammarM

        parser = GrammarM.parser()
        ast = parser.scan("""
        public static int foo(bar, baz=2) {
            a = 1; b = foobar(1, 2, "ashanti");
        }
        """)
        #print_ast(ast, attribute_order=['name', 'id'])
        conforms(ast, {
            'type': 'CompilationUnit',
            'declarations': [
                {'type': 'FunctionDecl',
                 'name': 'foo',
                 'modifiers': ['PublicModifier', 'StaticModifier'],
                 'arg_list': [
                     {'type': 'ArgSpec', 'name': 'bar'},
                     {'type': 'ArgSpec', 'name': 'baz',
                      'expr': {'type': 'NumExpr', 'val': 2}}],
                 'body':{
                     'type': 'Body',
                     'commands': [
                         {'type': 'AssignCommand',
                          'name': 'a',
                          'expr': 'NumExpr'},
                         {'type': 'AssignCommand',
                          'name': 'b',
                          'expr': 'CallExpr'}
                     ]}
                }]
        })
