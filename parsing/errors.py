"""
The Parsing module implements the following exception classes:

  * SpecError - any error that is a problem with the grammar specification
  * ParsingError - any error that occurs with nonconformant input in parsing
  * UnexpectedToken - a syntactic token that the grammar cannot accept at that
      position
"""

#===============================================================================
# Begin exceptions.
#

class ParsingError(Exception):
    """
    Top level Parsing exception class, from which we derive all exceptions that
    occur during the parsing of an input string.
    """

class SpecError(ValueError):
    """
    Specification error exception.  SpecError arises when the Spec
    introspection machinery detects an error either during docstring parsing
    or parser specification generation.
    """

class UnexpectedToken(ParsingError, SyntaxError):
    """
    Parser syntax error.  UnexpectedToken arises when a Parser instance detects
    a syntax error according to the Spec it is using, for the input being
    fed to it.
    """
    def __init__(self, message, offset=None, token=None):
        SyntaxError.__init__(self, message)
        self.offset = offset
        self.token = token


#
# End exceptions.
#===============================================================================

