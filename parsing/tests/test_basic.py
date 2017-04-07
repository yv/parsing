import unittest
import parsing


class TestParsing(unittest.TestCase):
    def test_basic_a(self):
        class TestParser(parsing.Lr):
            def __init__(self, spec):
                parsing.Lr.__init__(self, spec)

        from parsing.tests.specs import a
        spec = parsing.Spec(a)

        parser = TestParser(spec)
        parser.token_from_class(a.TokenId)
        parser.token_from_class(a.TokenStar)
        parser.token_from_class(a.TokenId)
        parser.token_from_class(a.TokenPlus)
        parser.token_from_class(a.TokenId)
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, '[[ID * ID] + ID]')

        parser = TestParser(spec)
        parser.token_from_class(a.TokenId)
        parser.token_from_class(a.TokenPlus)
        parser.token_from_class(a.TokenId)
        parser.token_from_class(a.TokenStar)
        parser.token_from_class(a.TokenId)
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, '[ID + [ID * ID]]')

        parser = TestParser(spec)
        parser.token_from_class(a.TokenId)
        parser.token_from_class(a.TokenStar)
        parser.token_from_class(a.TokenLparen)
        parser.token_from_class(a.TokenId)
        parser.token_from_class(a.TokenPlus)
        parser.token_from_class(a.TokenId)
        parser.token_from_class(a.TokenRparen)
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, '[ID * ([ID + ID])]')

    def test_basic_b(self):
        class TestParser(parsing.Glr):
            def __init__(self, spec):
                parsing.Glr.__init__(self, spec)

        from parsing.tests.specs import b
        spec = parsing.Spec(b, skinny=False)

        parser = TestParser(spec)
        parser.token_from_class(b.id)
        parser.token_from_class(b.star)
        parser.token_from_class(b.id)
        parser.token_from_class(b.plus)
        parser.token_from_class(b.id)
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, '[[ID * ID] + ID]')

        parser = TestParser(spec)
        parser.token_from_class(b.id)
        parser.token_from_class(b.plus)
        parser.token_from_class(b.id)
        parser.token_from_class(b.star)
        parser.token_from_class(b.id)
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, '[ID + [ID * ID]]')

        parser = TestParser(spec)
        parser.token_from_class(b.id)
        parser.token_from_class(b.star)
        parser.token_from_class(b.lparen)
        parser.token_from_class(b.id)
        parser.token_from_class(b.plus)
        parser.token_from_class(b.id)
        parser.token_from_class(b.rparen)
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, '[ID * ([ID + ID])]')

    def test_basic_d(self):
        class TestParser(parsing.Glr):
            def __init__(self, spec):
                parsing.Glr.__init__(self, spec)

        from parsing.tests.specs import d

        spec = parsing.Spec(d, skinny=False)

        parser = TestParser(spec)
        parser.token_from_class(d.id)
        parser.token_from_class(d.star)
        parser.token_from_class(d.id)
        parser.token_from_class(d.plus)
        parser.token_from_class(d.id)
        parser.token_from_class(d.star)
        parser.token_from_class(d.id)
        parser.eoi()

        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, '[[ID * ID] + [ID * ID]]')

    def test_basic_h(self):
        import parsing.grammar
        import parsing.lr_automaton
        #parsing.lr_automaton.StringSpec.cache = {}
        parsing.grammar.in_h = True
        class TestGlrParser(parsing.Glr):
            def __init__(self, spec):
                parsing.Glr.__init__(self, spec)
        try:
            from parsing.tests.specs import h

            spec = parsing.Spec(h, logFile='h.log', skinny=False)

            parser = TestGlrParser(spec)
            parser.token_from_class(h.TokenI)
            parser.token_from_class(h.TokenPlus)
            parser.token_from_class(h.TokenI)
            parser.token_from_class(h.TokenStar)
            parser.token_from_class(h.TokenI)
            parser.eoi()
            self.assertEqual(len(parser.start), 1)
            self.assertEqual(repr(parser.start[0]), '(i + (i * i))')
        finally:
            parsing.grammar.in_h = False

    def test_basic_i(self):
        class TestGlrParser(parsing.Glr):
            def __init__(self, spec):
                parsing.Glr.__init__(self, spec)

        from parsing.tests.specs import i
        self.assertRaises(parsing.SpecError, parsing.Spec, i)

    def test_basic_pickle(self):
        class TestGlrParser(parsing.Glr):
            def __init__(self, spec):
                parsing.Glr.__init__(self, spec)

        from parsing.tests.specs import b

        spec = parsing.Spec(b, skinny=False)
        import six.moves.cPickle
        specPickle = six.moves.cPickle.dumps(spec)
        spec2 = six.moves.cPickle.loads(specPickle)

        parser = TestGlrParser(spec2)
        parser.token_from_class(b.id)
        parser.token_from_class(b.star)
        parser.token_from_class(b.id)
        parser.token_from_class(b.plus)
        parser.token_from_class(b.id)
        parser.eoi()
        self.assertEqual(len(parser.start), 1)
        self.assertEqual(parser.start[0].val, '[[ID * ID] + ID]')


if __name__ == '__main__':
    unittest.main()
