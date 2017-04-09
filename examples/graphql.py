# ===============================================================================
# Copyright (c) 2017 by Yannick Versley <yversley@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# ===============================================================================

from __future__ import print_function
from parsing import mktoken, Grammar, print_ast


"""
This example shows how to parse complex grammars with the shorthand notions
provided by the new parsing module for the creation of reduction rules without
writing code.

The GraphQL query language is described at http://facebook.github.io/graphql/
and the spec document is (c) 2015-2016 Facebook Inc.
"""

def convert_string_value(s):
    return s[1:-1]

class GraphQLGrammar(Grammar):
    whitespace = '\s+|#.*|\n|,'
    punctuator = mktoken('punctuator', tokens='= |')
    Name = mktoken('Name', re='[_A-Za-z][_0-9A-Za-z]*')
    IntValue = mktoken('IntValue', re='-?(?:0|[1-9]\d*)', convert=int)
    FloatValue = mktoken('FloatValue', re=r'-?(?:0|[1-9]\d*)(?:\.\d+(?:[eE]\d+)?|[eE]\d+)', convert=float)
    StringValue = mktoken('StringValue', re=r'"(?:[^"\\]+|\\u[0-9A-Fa-f]{4}|\\[\\"/bfnrt])*"', convert=convert_string_value)

Nonterm = GraphQLGrammar.nonterm_base()

class Document(Nonterm):
    "%start"
    def r_definitions(self, definitions):
        "%reduce Definition+"
        self.definitions = definitions

class Definition(Nonterm):
    "%choice OperationDefinition FragmentDefinition"

class OperationType(Nonterm):
    "%choice 'query' 'mutation'"

class OperationDefinition(Nonterm):
    def r_optype(self, op_type, name, var_defs, directives, selection_set):
        "%reduce OperationType Name? VariableDefinitions? Directives? SelectionSet"
        self.name = name
        self.op_type = op_type
        self.variable_definitions = var_defs
        self.directives = directives
        self.selection_set = selection_set

    def r_selection(self, selection_set):
        "%reduce SelectionSet"
        self.selection_set = selection_set

class SelectionSet(Nonterm):
    "%reduce '{' Selection+ '}'"

class Selection(Nonterm):
    """
    %choice
    Field FragmentSpread InlineFragment
    """

class AliasAndName(Nonterm):

    def r_name(self, name):
        "%reduce Name"
        self.alias = None
        self.name = name

    def r_alias(self, alias, _c, name):
        "%reduce Name ':' Name"
        self.alias = alias
        self.name = name

class Field(Nonterm):
    def r_field(self, alias_name, arguments, directives, selection_set):
        "%reduce AliasAndName Arguments? Directives? SelectionSet?"
        self.alias = alias_name.alias
        self.name = alias_name.name
        self.arguments = arguments
        self.directives = directives
        self.selection_set = selection_set

class Arguments(Nonterm):
    def r_args(self, _lp, args, _rp):
        "%reduce '(' Argument+ ')'"
        return args

class Argument(Nonterm):
    "%reduce Name ':' Value"

## Alias leads to a conflict, re-built as AliasAndName
##
## class Alias(Nonterm):
##    "%reduce Name ':'"

class FragmentSpread(Nonterm):
    "%reduce '...' FragmentName Directives?"

class FragmentDefinition(Nonterm):
    "%reduce 'fragment' FragmentName TypeCondition Directives? SelectionSet"

class FragmentName(Nonterm):
    def r_name(self, name):
        "%reduce Name"
        assert name != 'on'
        return name

class TypeCondition(Nonterm):
    "%reduce 'on' NamedType"

class InlineFragment(Nonterm):
    "%reduce '...' TypeCondition? Directives? SelectionSet"

class Value(Nonterm):
    """
    %choice
    Variable IntValue FloatValue StringValue
    """
    def r_true(self, _t):
        "%reduce 'true'"
        self.type = 'BooleanValue'
        self.val = True

    def r_false(self, _t):
        "%reduce 'false'"
        self.type = 'BooleanValue'
        self.val = False

    def r_null(self, _t):
        "%reduce 'null'"
        self.type = 'NullValue'
        self.val = None

    def r_enum(self, name):
        "%reduce Name"
        self.type = 'EnumValue'
        self.val = name

    def r_list(self, _lb, values, _rb):
        "%reduce '[' Value* ']'"
        self.type = 'ListExpression'
        self.val = values

    def r_object(self, _lb, object_fields, _rb):
        "%reduce '{' ObjectField* '}'"
        self.type = 'ObjectValue'
        self.fields = object_fields

class ObjectField(Nonterm):
    "%reduce Name ':' Value"

class Variable(Nonterm):
    "%reduce '$' Name"

class VariableDefinitions(Nonterm):
    "%reduce '(' VariableDefinition+ ')'"

class VariableDefinition(Nonterm):
    """
    %reduce Variable ':' Type DefaultValue?
    """

class DefaultValue(Nonterm):
    "%reduce '=' Value"

class NonNull(Nonterm):
    "%reduce '!'"

class NamedType(Nonterm):
    def r_named(self, name):
        "%reduce Name"
        self.type = 'NamedType'
        self.type_ref = name

class Type(Nonterm):
    def r_named_nonnull(self, name, non_null):
        "%reduce NamedType NonNull?"
        name.non_null = non_null
        return name

    def r_list(self, lst, non_null):
        "%reduce '[' Type ']' NonNull?"
        self.type = 'ListType'
        self.element_type = lst
        self.non_null = non_null

class Directives(Nonterm):
    "%reduce Directive+"

class Directive(Nonterm):
    "%reduce '@' Name Arguments?"

if __name__ == '__main__':
    from parsing.ast_objs import bcolors
    attribute_order = [
        'op_type', 'name', 'fragment_name', 'variable',
        'variable_definitions', 'type_condition']
    parser = GraphQLGrammar.parser(
        verbose=True, logFile='graphql.log', skinny=False)
    for snippet in [
        """
mutation {
  likeStory(storyID: 12345) {
    story {
      likeCount
    }
  }
}
        """,
        """
# `me` could represent the currently logged in viewer.
{
  me {
    name
  }
}

# `user` represents one of many users in a graph of data, referred to by a
# unique identifier.
{
  user(id: 4) {
    name
  }
}
        """,
        """
{
  user(id: 4) {
    id
    name
    smallPic: profilePic(size: 64)
    bigPic: profilePic(size: 1024)
  }
}
        """,
        """
query withFragments {
  user(id: 4) {
    friends(first: 10) {
      ...friendFields
    }
    mutualFriends(first: 10) {
      ...friendFields
    }
  }
}

fragment friendFields on User {
  id
  name
  profilePic(size: 50)
}
        """,
        """
query inlineFragmentTyping {
  profiles(handles: ["zuck", "cocacola"]) {
    handle
    ... on User {
      friends {
        count
      }
    }
    ... on Page {
      likers {
        count
      }
    }
  }
}
        """,
        """
query inlineFragmentNoType($expandedInfo: Boolean) {
  user(handle: "zuck") {
    id
    name
    ... @include(if: $expandedInfo) {
      firstName
      lastName
      birthday
    }
  }
}
        """]:
        print(bcolors.OKBLUE + snippet + bcolors.ENDC)
        try:
            #parser._verbose = True
            expr = parser.scan(snippet)
            print_ast(expr, attribute_order=attribute_order)
        except SyntaxError as e:
            print(e)
            print(parser._stack)
            print("Expecting:", parser.last_state(), parser.expecting(), parser.expecting_goto())
