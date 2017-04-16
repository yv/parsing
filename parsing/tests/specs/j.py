import parsing

class GrammarJ(parsing.Grammar):
    # precedences
    p1 = parsing.Precedence.left()
    p2 = parsing.Precedence.left(after=p1)
    # tokens
    plus = parsing.mktoken('plus')
    star = parsing.mktoken('star')
    lparen = parsing.mktoken('lparen')
    rparen = parsing.mktoken('rparen', prec=p2)
    id = parsing.mktoken('id')

Nonterm = GrammarJ.nonterm_base()


# Non-terminal definitions.
class NontermE(Nonterm):
    "%start E"
    def reduceA(self, E, plus, T):
        "%reduce E plus T [p1]"
        self.val = '[%s + %s]' % (E.val, T.val)

    def reduceB(self, T):
        "%reduce T"
        self.val = T.val

class T(Nonterm):
    "%nonterm"
    def reduceA(self, T, star, F):
        "%reduce T star F"
        self.val = '[%s * %s]' % (T.val, F.val)

    def reduceB(self, F):
        "%reduce F"
        self.val = F.val

class NontermF(Nonterm):
    "%nonterm F [p2]"
    def reduceA(self, lparen, E, rparen):
        "%reduce lparen E rparen"
        self.val = '(%s)' % (E.val,)

    def reduceB(self, id):
        "%reduce id [split]"
        self.val = 'ID'
