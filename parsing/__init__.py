"""
The Parsing module implements an LR(1) parser generator, as well as the
runtime support for using a generated parser, via the Lr and Glr parser
drivers.  There is no special parser generator input file format, but the
parser generator still needs to know what classes/methods correspond to
various aspects of the parser.  This information is specified via
docstrings, which the parser generator introspects in order to generate a
parser.  Only one parser specification can be embedded in each module, but
it is possible to share modules between parser specifications so that, for
example, the same token definitions can be used by multiple parser
specifications.

The parsing tables are LR(1), but they are generated using a fast algorithm
that avoids creating duplicate states that result when using the generic
LR(1) algorithm.  Creation time and table size are on par with the LALR(1)
algorithm.  However, LALR(1) can create reduce/reduce conflicts that don't
exist in a true LR(1) parser.  For more information on the algorithm, see:

    A Practical General Method for Constructing LR(k) Parsers
    David Pager
    Acta Informatica 7, 249-268 (1977)

Parsing table generation requires non-trivial amounts of time for large
grammars.  Internal pickling support makes it possible to cache the most
recent version of the parsing table on disk, and use the table if the
current parser specification is still compatible with the one that was used
to generate the pickled parsing table.  Since the compatibility checking is
quite fast, even for large grammars, this removes the need to use the
standard code generation method that is used by most parser generators.

Parser specifications are encapsulated by the Spec class.  Parser instances
use Spec instances, but are themselves based on separate classes.  This
allows multiple parser instances to exist simultaneously, without requiring
multiple copies of the parsing tables.

See the :py:mod:`lr_automaton` module for more information about the parser
classes.
"""
from parsing.ast import Token, mktoken, ASTToken, Nonterm, print_ast
from parsing.grammar import Precedence
from parsing.class_spec import Grammar, SpecError
from parsing.scanner import ScannerError
from parsing.lr_automaton import Spec, Lr, Glr, UnexpectedToken