/******************************************************************************
 * Church - Functional Programming Language                                   *
 ******************************************************************************
 * Curch is a small untyped functional language compiler and G-Machine        *
 * interpreter based on the tutorial in [1] and enirely programmed with the   *
 * educational language (Open) Turing.                                        *
 * Church supports integers, strings, custom data constructors and lists. The *
 * pattern matching mechanism is limited to one level of data constructors    *
 * only. Pattern matching cannot dispatch integers or strings.                *
 * Additionally Church supports built-in if..then..else..end blocks, but      *
 * without elsif subblocks.                                                   *
 *                                                                            *
 * Special symbols ( = < > | - + * _ / ! : ? & % $ ~ # . @ ^ \ ) can be       *
 * defined to be either a postfix operator, an inifx operator (e.g. list      *
 * concatenation with h::t) or a postfix operator (e.g. faculty 4!).          *
 *                                                                            *
 * There is no operator precedence, so careful parenthesization is important. *
 * For instance 4! + 1 will be 120 (5!). The correct result can be obtained   *
 * either write (4!) + 1 or 1 + 4! because of the right associativity of the  *
 * primitve binary operators + - * / < > <= >= == != || &&.                   *
 * Application is left associative but is second in precedence after right    *
 * associative primitive binary operators.                                    *
 *                                                                            *
 * Over saturated functions may yield infinite computations. E.g. the twice   *
 * applied identity function I I 3 will not terminate. Additional parentheses *
 * resolve the problem: I (I 3).                                              *
 *                                                                            *
 * REFERENCES                                                                 *
 *  [1] Implementing Functional Languages: a tutorial,                        *
 *      Simon L Peyton Jones and David R Lester, March 23, 2000               *
 *  [2] ML for the Working Programmer, Lawrence C. Paulson, Second Edition,   *
 *      Cambridge University Press 1997, ISBN: 0 521 57050 6                  *
 *  [3] Java 2 Platform SE v1.4.2 Documentation                               *
 *      <http://docs.oracle.com/javase/1.4.2/docs/api/>                       *
 *  [4] The Haskell 98 Report - 8 Standard Prelude                            *
 *      <http://www.haskell.org/onlinereport/standard-prelude.html>           *
 ******************************************************************************
 *  Copyright (C) 2012  Hoertnagl Mathias <mathias.hoertnagl@gmail.com>       *
 *                                                                            *
 * This program is free software: you can redistribute it and/or modify it    *
 * under the terms of the GNU General Public License as published by the Free *
 * Software Foundation, either version 3 of the License, or (at your option)  *
 * any later version.                                                         *
 *                                                                            *
 * This program is distributed in the hope that it will be useful, but        *
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License   *
 * for more details.                                                          *
 *                                                                            *
 * You should have received a copy of the GNU General Public License along    *
 * with this program.  If not, see <http://www.gnu.org/licenses/>.            *
 ******************************************************************************/
unit class Parser
  inherit Stage in "../ui/Stage.tu"
  import HashMap       in "../util/HashMap.tu",
         LinkedList    in "../util/LinkedList.tu",
         Iterator      in "../util/Iterator.tu",
         Expr          in "ast/Expr.tu",
         ESc           in "ast/ESc.tu",
         EAp           in "ast/EAp.tu",
         EBinOp        in "ast/EBinOp.tu",
         ECon          in "ast/ECon.tu",
         EIf           in "ast/EIf.tu",
         EMatch        in "ast/EMatch.tu",
         EAlt          in "ast/EAlt.tu",
         ANum          in "ast/ANum.tu",
         AVar          in "ast/AVar.tu",
         ACon          in "ast/ACon.tu",
         AString       in "ast/AString.tu",
         Token         in "../lexer/tokens/Token.tu",
         AsgnToken     in "../lexer/tokens/AsgnToken.tu",
         BinOpToken    in "../lexer/tokens/BinOpToken.tu",
         DataToken     in "../lexer/tokens/DataToken.tu",
         EndToken      in "../lexer/tokens/EndToken.tu",
         IdToken       in "../lexer/tokens/IdToken.tu",
         LetToken      in "../lexer/tokens/LetToken.tu",
         NumToken      in "../lexer/tokens/NumToken.tu",
         PipeToken     in "../lexer/tokens/PipeToken.tu",
         StringToken   in "../lexer/tokens/StringToken.tu",
         AltToken      in "tokens/AltToken.tu",
         CaseToken     in "tokens/CaseToken.tu",
         CondToken     in "tokens/CondToken.tu",
         ListToken     in "tokens/ListToken.tu",
         SubExprToken  in "tokens/SubExprToken.tu"
  export parse, printAST
  
  /** Symbol table for all data constructors. Each constructor consists of a
      unique ID and a number indicating how many arguments it takes (arity). 
      This map only contains constructor prototypes of type ACon. The actual
      constructor nodes for the AST are of type ECon. */
  var cons : ^HashMap
  
  /** The current token. */
  var tok : ^Token
  
  /** Constant Nil constructor node. */
  var nilCon : ^ECon
  new nilCon
  nilCon->New(nilCon)
  nilCon->name := "[]"

  /**
   * Mandatory constructor. Initializes and resets the recognzed constructors.
   * Registers the '[]' and '::' primitve constructors for lists.
   *
   * @param obj  A refernece to this parser.
   */
  body proc New(obj : ^Object)
    this := obj
    new cons
    cons->New(cons)
    
    % Nil list constructor prototype.
    var nilCon : ^ACon
    new nilCon
    nilCon->name := "[]"
    nilCon->arity := 0
    
    % Cons list constructor prototype.
    var consCon : ^ACon
    new consCon
    consCon->name := "::"
    consCon->arity := 2

    cons->Put("[]", nilCon)
    cons->Put("::", consCon)
  end New
  
  /**
   * Prints an error message in red.
   *
   * @param msg  Error message string.
   */
  proc parserError(msg : string)
    error("Parser: ", msg)
  end parserError
  
  /**
   * Prints an info in green.
   *
   * @param msg  Info message string.
   */
  proc parserInfo(msg : string)
    info("Parser: ", msg)
  end parserInfo

  /**
   * Prints out the parsed AST.
   *
   * @param ast  The AST root node.
   */  
  proc printAST(ast : ^Expr)
    if ast ~= nil then
      put repeat("-", 80)
      put " Parser"
      put repeat("-", 80)
      put " " ..
      ast->print()
      put ""
      put repeat("-", 80)
      put ""
    end if
  end printAST

  /**
   * Turns a list of AST expressions into a left-associative application 
   * expression. If the list contains only one element, returns the element 
   * untouched. Returns nil if the list is empty.
   *
   * @param list  List of AST expressions.
   * @return  Application expressions tree.
   */ 
  fcn buildAp(list : ^LinkedList) : ^Expr
    
    var iter : ^Iterator := list->GetIterator()
    
    if not(iter->HasNext()) then
      result nil
    end if
    
    var ap : ^EAp
    var e0, e1 : ^Expr
    
    e0 := iter->Next()
    loop exit when not(iter->HasNext())      
      e1 := iter->Next()
      new ap
      ap->left := e0
      ap->right := e1
      e0 := ap
    end loop
    result e0
  end buildAp

  /**
   * Parse an integer number.
   *
   * @param t  Integer token.
   * @return  Integer node.
   */ 
  fcn parseNum(t : ^NumToken) : ^ANum
    var n : ^ANum
    new n
    n->value := t->value
    result n
  end parseNum

  /**
   * Parse a string constant.
   *
   * @param t  String constant token.
   * @return  String constant node.
   */ 
  fcn parseString(t : ^StringToken) : ^AString
    var s : ^AString
    new s
    s->value := t->value
    result s
  end parseString

  /**
   * Parse a variable.
   *
   * @param t  Identifer token.
   * @return  Variable node.
   */ 
  fcn parseVar(t : ^IdToken) : ^AVar
    var v : ^AVar
    new v
    v->name := t->value
    result v
  end parseVar
  
  forward fcn parseE(lexer : ^Iterator) : ^Expr
  forward fcn parseCE(lexer : ^Iterator) : ^Expr

  /**
   * Parse a constructor node.
   *
   * @param lexer  Token stream.
   * @param c      Constructor prototype.
   * @return  Constructor expression.
   */ 
  fcn parseCon(lexer : ^Iterator, c : ^ACon) : ^ECon
    
    var list : ^LinkedList
    new list
    
    var con : ^ECon
    new con
    con->name := c->name
    
    for i : 1 .. c->arity
      list->Add( parseCE(lexer) )
    end for
    
    %for i : 1 .. c->arity-1
    %  list->Add( parseCE(lexer) )
    %end for
    %list->Add( parseE(lexer) )
    
    con->params := list
    result con
  end parseCon

  /**
   * Parses an identifer. If the identifier is in the map of known constructor
   * prototypes, parses it as constructor expression. Else parses it as either
   * global function or locally bound variable (both are parsed the same way).
   *
   * @param lexer  The token stream.
   * @param id     Identifer token.
   * @return  Parsed identifer expression.
   */ 
  fcn parseId(lexer : ^Iterator, id : ^IdToken) : ^Expr
    
    % If this Id is a data constructor, load the constructor prototype.
    var c : ^ACon := cons->Get(id->value)
    
    if c = nil then      
      % A data constructor with this name does not exist: parse the IdToken as
      % a simple local variable or global function name.
      result parseVar(id)
    else
      % Build a data constructor tree. The next n expressions are parameters
      % of this constructor.
      result parseCon(lexer, c)
    end if 
  end parseId
 
  /**
   * Parses a primitve binary operator in a right-associative fashion.
   *
   * @param lexer  The token stream.
   * @param opt    Binary operator token.
   * @return  Binary operator subtree.
   */ 
  fcn parseBinOp(lexer : ^Iterator, opt : ^BinOpToken) : ^Expr
  
    var op : ^EBinOp
    new op
    op->op := opt->op
    
    % XXX: Ugly.
    if lexer->HasNext() then
      tok := lexer->Next()
      var list : ^LinkedList
      new list
      list->Add(tok)
      op->left := parseE(list->GetIterator())
    end if
    if lexer->HasNext() then
      op->right := parseE(lexer)
    end if
    result op
  end parseBinOp
  
  /**
   * Parses one alternative of a match-with expression.
   *
   * @param al  Match alternative token.
   * @return  Match alternative expression.
   */ 
  fcn parseAlt(al : ^AltToken) : ^EAlt
        
    var alt : ^EAlt
    new alt
    alt->New(alt)
    
    var iter : ^Iterator := al->tMatch->GetIterator()
    
    if iter->HasNext() then
      tok := iter->Next()
      if objectclass(tok) ~= IdToken then
        parserError("Parameters of a match alternative must be of type Id.")
      end if
      
      var c : ^ACon := cons->Get(IdToken(tok).value)
      if c = nil then
        parserError("First parameter of a match alternative must be a " + 
                    "constructor node. Token is '" + IdToken(tok).value + "'."
        )
      end if
      alt->name := c->name
    end if
    
    loop exit when not(iter->HasNext())
      tok := iter->Next()
      if objectclass(tok) ~= IdToken then
        parserError("Parameters of a match alternative must be of type Id.")
      end if
      var v : ^AVar := parseVar(tok)
      alt->vars->Put(v->name, v)
    end loop
    
    alt->expr := parseE(al->tExpr->GetIterator())
    result alt
  end parseAlt

  /**
   * Parse a match-with expression.
   *
   * @param cs  Case token tree.
   * @return  Match expression.
   */ 
  fcn parseMatch(cs : ^CaseToken) : ^EMatch
    
    var mtch : ^EMatch
    new mtch
    mtch->New(mtch)
    mtch->mexp := parseE(cs->tMatch->GetIterator())
    
    var iter : ^Iterator := cs->tAlts->GetIterator()
    loop exit when not(iter->HasNext())
      mtch->alts->Add( parseAlt(iter->Next()) )
    end loop
    result mtch
  end parseMatch

  /**
   * Recursivly parses the elements of of a list.
   *
   * @param lexer  Token stream.
   * @return  Constructor expression.
   */ 
  fcn parseListElements(lexer : ^Iterator) : ^ECon
    
    var expr : ^Expr

    var list : ^LinkedList
    new list
    
    var con : ^ECon
    new con
    con->name := "::"
        
    if lexer->HasNext() then
      var exprl : ^LinkedList := lexer->Next()
      list->Add( parseE(exprl->GetIterator()) )
    else
      result nilCon
    end if
    
    % Recursivly parse the next element. If there is no next element, 
    % add the empty list constructor '[]'.
    if lexer->HasNext() then
      list->Add( parseListElements(lexer) )
    else
      list->Add(nilCon)
    end if
    
    con->params := list
    result con
  end parseListElements

  /**
   * Parse a list.
   *
   * @pram list  List token tree.
   * @return  List expression.
   */ 
  fcn parseList(list : ^ListToken) : ^ECon
    result parseListElements(list->tElems->GetIterator())
  end parseList

  /**
   * Parse an if-then expression.
   *
   * @param cond  Conditional token tree.
   * @return  If expression.
   */ 
  fcn parseIf(cond : ^CondToken) : ^EIf
    
    var eif : ^EIf
    new eif
    
    eif->eCond := parseE(cond->tCond->GetIterator())
    eif->eThen := parseE(cond->tThen->GetIterator())
    eif->eElse := parseE(cond->tElse->GetIterator())
    result eif
  end parseIf

  /**
   * Parse a subexpression enclosed in '(' and ')'.
   *
   * @param se  Sub expression token tree.
   * @return  Subexpression.
   */ 
  fcn parseSubExpr(se : ^SubExprToken) : ^Expr
    result parseE(se->tExpr->GetIterator())
  end parseSubExpr 

  /**
   * Parse construtor parameter expression. A constructor expression is always
   * a single number, string, varaible expression, a sublist or a subexpression
   * in parentheses.
   *
   * @param lexer  Token stream.
   * @return  Construtor parameter expression.
   */ 
  body fcn parseCE(lexer : ^Iterator) : ^Expr
    
    if lexer->HasNext() then
      
      tok := lexer->Next()
      if objectclass(tok) = NumToken then
        result parseNum(tok)
      elsif objectclass(tok) = StringToken then
        result parseString(tok)
      elsif objectclass(tok) = IdToken then
        result parseId(lexer, tok)
      elsif objectclass(tok) = SubExprToken then
        result parseSubExpr(tok)
      elsif objectclass(tok) = ListToken then
        result parseList(tok)
      else
        parserError("Unexpected constructor parameter token '" + 
                    tok->ToString() + "'."
        )
      end if
    end if
    result nil
  end parseCE

  /**
   * Parse an expression.
   *
   * @param lexer  Token stream.
   * @return  Parsed expression.
   */ 
  body fcn parseE(lexer : ^Iterator) : ^Expr
    
    var list : ^LinkedList
    new list
    
    loop exit when not(lexer->HasNext())
      
      tok := lexer->Next()
      if objectclass(tok) = NumToken then
        list->Add( parseNum(tok) )
      elsif objectclass(tok) = StringToken then
        list->Add( parseString(tok) )
      elsif objectclass(tok) = IdToken then
        list->Add( parseId(lexer, tok) )
      elsif objectclass(tok) = BinOpToken then
        list->Add( parseBinOp(lexer, tok) )
      elsif objectclass(tok) = CaseToken then
        list->Add( parseMatch(tok) )
      elsif objectclass(tok) = CondToken then
        list->Add( parseIf(tok) )
      elsif objectclass(tok) = SubExprToken then
        list->Add( parseSubExpr(tok) )
      elsif objectclass(tok) = ListToken then
        list->Add( parseList(tok) )
      else
        parserError("Unexpected token '" + tok->ToString() + "'.")
      end if
    end loop
    result buildAp(list)
  end parseE

  /**
   * Parse the locally bound variable arguments of a supercombinator.
   *
   * @param lexer  The token stream.
   * @param sc  Supercombinator expression to add the arguments to.
   */
  fcn parseSc(lexer : ^Iterator) : ^ESc
  
    if not(lexer->HasNext()) then
      parserError("Unexpected end of statement.")
      result nil
    end if
    
    tok := lexer->Next()
    if objectclass(tok) ~= IdToken then
      parserError("Identifier expected. Token is '" + tok->ToString() + "'.")
      result nil
    end if
    
    var sc : ^ESc
    new sc
    sc->New(sc)
    sc->name := IdToken(tok).value

    var v : ^AVar
    
    loop exit when not(lexer->HasNext())
      tok := lexer->Next()
      exit when objectclass(tok) = AsgnToken
      if objectclass(tok) = IdToken then
        v := parseVar(tok)
        sc->vars->Put(v->name, v)
      else
        parserError("Local variable expected. Token is '" + 
                    tok->ToString() + "'."
        )
      end if
    end loop
    
    % TODO: Check for =.
    
    sc->expr := parseE(lexer) 
    if sc->expr = nil then
      result nil
    end if
    result sc
  end parseSc

  /**
   * Parse a data definition. Creates construtor prototypes for each encountered 
   * constructor definition and stores it in 'cons'.
   *
   * @param lexer  Token stream.
   */ 
  proc parseCn(lexer : ^Iterator)
    
    loop exit when not(lexer->HasNext())
    
      tok := lexer->Next()
      if objectclass(tok) ~= IdToken then
        parserError("Constructor identifier expected. Token is '" + 
                    tok->ToString() + "'."
        )
        return
      end if 
      
      var con : ^ACon
      new con
      con->name := IdToken(tok).value
      con->arity := 0
      
      loop exit when not(lexer->HasNext())
        tok := lexer->Next()
        exit when objectclass(tok) = PipeToken
        con->arity += 1
      end loop
      cons->Put(con->name, con)
      if debug then
        parserInfo("Constructor '" + con->name + "/" + intstr(con->arity) + 
                   "' added."
        )
      end if
    end loop
  end parseCn

  /**
   * Parses a token tree and creates an AST for a data definition or let 
   * expression. The parser expects the token tree to either beginn with a 'let'
   * or 'data' token. The token stream has to be preprocessed by the 
   * TokenTreeBulder stage.
   *
   * @param tokens  Token tree.
   * @return  AST.
   */ 
  fcn parse(tokens : ^LinkedList) : ^Expr

    if (tokens = nil) or (tokens->Size() = 0) then
      result nil
    end if

    var ast : ^Expr := nil
    
    var lexer : ^Iterator := tokens->GetIterator()
    if not(lexer->HasNext()) then
      result nil
    end if
    
    tok := lexer->Next()
    if objectclass(tok) = DataToken then
      parseCn(lexer)
      result nil
    elsif objectclass(tok) = LetToken then
      ast := parseSc(lexer)
      if debug then
        printAST(ast)
      end if
      result ast
    else  
      parserError("Unsupported statement type.")
      result nil
    end if
  end parse
  
end Parser