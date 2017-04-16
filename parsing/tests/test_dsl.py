import unittest
from parsing.ast import print_ast

def conforms(ast, description):
    if 'type' in description:
        assert description['type'] == ast['type']

class TestParsingDSL(unittest.TestCase):
    def test_dsl_m(self):
        from parsing.tests.specs.m import GrammarM

        parser = GrammarM.parser()
        ast = parser.scan("""
        public static int foo(bar, baz=2) {
            a = 1; b = foobar(1, 2, "ashanti");
        }
        """)
        print_ast(ast)