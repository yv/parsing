from parsing import Grammar, mktoken

class GrammarM(Grammar):
    Id = mktoken('Id', re='[a-zA-Z][a-zA-Z0-9]*')
    Str = mktoken('Str', between='""')
    Num = mktoken('Num', re='[0-9]+', convert=int)

Nonterm = GrammarM.nonterm_base()

class CompilationUnit(Nonterm):
    """
    %start CompilationUnit
    %reduce Declaration+
    """

class Declaration(Nonterm):
    """
    %choice FunctionDecl
    """

class FunctionDecl(Declaration):
    """
    %reduce Modifier* Type Id '(' ArgList ')' Body
    """

class Modifier(Nonterm):
    """
    %enum 'static' 'public' 'private' 'protected'
    """

class ArgList(Nonterm):
    """
    %list ArgSpec ','
    """

class ArgSpec(Nonterm):
    """
    %reduce Name
    %reduce Name '=' Expr
    %reduce Name ':' Type
    %reduce Name ':' Type '=' Expr
    """

class Name(Nonterm):
    """
    %choice Id
    """

class Type(Nonterm):
    def r_name(self, name):
        "%reduce Id"
        self.type = "NamedType"
        self.name = name

class Expr(Nonterm):

    def r_num(self, num):
        "%reduce Num"
        self.type = "NumExpr"
        self.val = num
    
    def r_str(self, s):
        "%reduce Str"
        self.type = "StrExpr"
        self.val = s

    def r_name(self, name):
        "%reduce Name"
        self.type = "VarExpr"
        self.name = name
    
    def r_call(self, fn_name, _lp, args, _rp):
        "%reduce Name '(' CallArgs ')'"
        self.type = 'CallExpr'
        self.fn_name = fn_name
        self.args = args

class CallArgs(Nonterm):
    """
    %list CallArg ','
    """

class CallArg(Nonterm):
    """
    %reduce Name '=' Expr
    %reduce Expr
    """

class Body(Nonterm):
    """
    %reduce '{' Command* '}'
    """

class Command(Nonterm):
    """
    %reduce:AssignCommand Name '=' Expr ';'
    %reduce:ExprCommand Expr ';'
    """