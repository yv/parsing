# Inside parsing.py's implementation of LR(1) parsing

The construction of a parser follows three rough phases:

 * collecting the grammar rules by inspection - this is done by the modules
   `parsing.cls_adapt` and `parsing.mod_adapt`, and then followed up inside
   `parsing.lr_automaton.Spec` by resolving references in `_references`
 * constructing the LR(1) automaton using Pager's (1977) method of constructing
   and merging item sets, followed by the construction of `_action` and `_goto`
   tables and resolution of conflicts via precedence rules.
 * The resulting table can be used in the `Lr` and `Glr` parser classes, which
   perform deterministic parsing (Lr) or using Tomita's (1984) graph-structured
   stack method.
   
### Writing a Grammar

Declaring grammars the new way works as follows:
 
 1. subclassing `parsing.Grammar` in a class that declares:
    - precedences (as instances of `parsing.Precedence`)
    - the definition of whitespace (in `whitespace`)
    - tokens (as calls to `mktoken`)
 2. creating the declarative base for nonterminals using
    ```python
    Nonterm = MyGrammar.nonterm_base()
    ```
 3. declaring any nonterminal, together with its rules, as a subclass
    of the declarative base.
    - These nonterminal classes will be used to construct the AST that is
      the result of the parser
    - The start symbol must have a docstring `"%start"`
    - Any reduction method must have a docstring starting with `%reduce`
    - There are two mechanisms for shorthands - i.e. producing grammar rules
      without explicitly writing a method
      * If the class docstring of the Nonterminal starts with `%choice`, the
        following symbols will be used to create unary productions where the
        current class does not appear in the syntax tree.
      * If the class docstring of the Nonterminal starts with `%reduce`, the
        docstring will be used to create reduction rules with a default way
        of assigning AST attributes (see below)
    - In the right-hand side of a rule, you can use the following additional symbols:
      * A symbol `'sym'` for specifying punctuation or a keyword (as a literal)
      * Given a normal grammar symbol `Sym`, the variants `Sym+`, `Sym*` and `Sym?`
        - For `Sym+` and `Sym*`, the corresponding parameter for the reduction method
          will be a list of matched nodes
        - For `Sym?` the corresponding parameter will be either an AST node or `None`
        
Default methods for `%reduce` shorthand on a class:
 * any literal will be discarded
 * any other RHS symbol will be stored under its name converted to snake case
   - `BoolSpecifier` would be assigned to a `bool_specifier` property
   - `Node+` would be assigned to a `nodes` property

All of this can be seen in the `examples/graphql.py` example.
   
### Grammar and Automaton construction

**Spec**
 * init steps
 * generating the item sets
   * _firstSets()
   * _followSets()
   * _items()
     * starts from the initial state
     * stores items in itemSetHash / in the _itemSets list
     * looks for a weakly compatible state, merges into that state, and enqueues it
     * if not merged, state is added to worklist, itemSetsHash, _itemSets
 * generating the grammar automaton
  * _lr()
  * _disambiguate()

**SymbolSpec**
 * firstSet
 * followSet

**Production**
 * numbers are assigned sequentially

**Item**: dotted rule
 * "hash" derived from dotPos and production
   * current formula doesn't allow interleaving
     the creation of productions and items
 * members
   * production
   * dotPos
   * lookahead
     - lookahead is a dict from (k, k) entries - why not just a set() or frozenset?

**ItemSet**: set of Item_s
 * is a set of items with a set of lookaheads
 * goto(sym) computes a following itemset
 * merge(itemset) 
 * weakCompat
 * closure and _closeItems compute the closure of associated dotPos-0 (= added)items
   * closure computes all direct derived items from the kernel
   * _closeItems(items) computes all derived items from a given set
   * closure and _closeItems use StringSpec(String(...)) to memoize the firstSet construction