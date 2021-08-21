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
unit class TokenTreeBuilder
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
         ArrowToken    in "../lexer/tokens/ArrowToken.tu",
         AsgnToken     in "../lexer/tokens/AsgnToken.tu",
         BinOpToken    in "../lexer/tokens/BinOpToken.tu",
         CommaToken    in "../lexer/tokens/CommaToken.tu",
         DataToken     in "../lexer/tokens/DataToken.tu",
         ElseToken     in "../lexer/tokens/ElseToken.tu",
         EndToken      in "../lexer/tokens/EndToken.tu",
         IdToken       in "../lexer/tokens/IdToken.tu",
         IfToken       in "../lexer/tokens/IfToken.tu",
         LetToken      in "../lexer/tokens/LetToken.tu",
         LeftBraToken  in "../lexer/tokens/LeftBraToken.tu",
         LeftParToken  in "../lexer/tokens/LeftParToken.tu",
         MatchToken    in "../lexer/tokens/MatchToken.tu",
         NumToken      in "../lexer/tokens/NumToken.tu",
         PipeToken     in "../lexer/tokens/PipeToken.tu",
         RightBraToken in "../lexer/tokens/RightBraToken.tu",
         RightParToken in "../lexer/tokens/RightParToken.tu",
         StringToken   in "../lexer/tokens/StringToken.tu",
         ThenToken     in "../lexer/tokens/ThenToken.tu",
         WithToken     in "../lexer/tokens/WithToken.tu",
         AltToken      in "tokens/AltToken.tu",
         CaseToken     in "tokens/CaseToken.tu",
         CondToken     in "tokens/CondToken.tu",
         ListToken     in "tokens/ListToken.tu",
         SubExprToken  in "tokens/SubExprToken.tu"
  export build
  
  /** The current token. */
  var tok : ^Token

  /** Token stream. */
  var lexer : ^Iterator
  
  /** Result token stream. */
  var res : ^LinkedList

  % Static token objects.
  var letToken : ^LetToken
  new letToken
  
  var mainToken : ^IdToken
  new mainToken
  mainToken->value := "main"
  mainToken->style := IdToken.Style.PREFIX
  
  var asgnToken : ^AsgnToken
  new asgnToken
  
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

  forward fcn parseE() : ^LinkedList

  /**
   * Parse list. Empty list elements are ignored. This allows the parser to
   * accept lists like [,] | [1,] | [,1] | [1,,2] | [,1,,2,] | ...
   *
   * @return  List token tree.
   */
  fcn parseL() : ^Token
    
    var listToken : ^ListToken
    new listToken
    
    var elems : ^LinkedList
    new elems
    
    loop exit when objectclass(tok) = RightBraToken
      var elem : ^LinkedList := parseE()
      % Ignore empty entries.
      if elem->Size() > 0 then
        elems->Add(elem)
      end if
    end loop
    
    listToken->tElems := elems
    result listToken
  end parseL

  /**
   * Parse a subexpression of '(...)'.
   * TODO: Accepts (1+(2+3)
   *
   * @return Subexpression token.
   */
  fcn parseSE() : ^Token
    
    var subExprToken : ^SubExprToken
    new subExprToken
    
    subExprToken->tExpr := parseE()
    if objectclass(tok) ~= RightParToken then
      parserError("Missing ')' at '" + subExprToken->tExpr->ToString() + "'")
    end if
    
    result subExprToken
  end parseSE

  /**
   * Parse 'if .. then .. else .. end' block.
   *
   * @return  Conditional token.
   */
  fcn parseI() : ^Token
    
    var condToken : ^CondToken
    new condToken
    
    condToken->tCond := parseE()
    if objectclass(tok) ~= ThenToken then
      parserError("Missing 'then' at '" + condToken->tCond->ToString() + "'")
    end if
    
    condToken->tThen := parseE()
    if objectclass(tok) ~= ElseToken then
      parserError("Missing 'else' at '" + condToken->tThen->ToString() + "'")
    end if
    
    condToken->tElse := parseE()
    if objectclass(tok) ~= EndToken then
      parserError("Missing 'end' at '" + condToken->tElse->ToString() + "'")
    end if
    
    result condToken
  end parseI

  /**
   * Parse an ID. Rearranges the token if it is an infix or postfix identifier.
   * Swaps the last element of the list and the identifer token (turns the
   * infix or postfix id into a prefix version).
   *
   * @param list  List of parsed tokens.
   * @param id    Identifier token.
   */
  proc parseId(list : ^LinkedList, id : ^IdToken)
    
    var left : ^Token
    
    if id->style = IdToken.Style.PREFIX then
      list->Add(id)
    else
      if list->Size() > 0 then
        left := list->Remove(list->Size()-1)
        list->Add(id)
        list->Add(left)
      else
        parserError("Missing left hand side parameter for non-prefix id '" + 
                    id->ToString() + "'."
        )
      end if
    end if
  end parseId

  /**
   * Parse a primitive infix operator.
   *
   * @param list  List of parsed tokens.
   * @param op    Binary operator token.
   */
  proc parseP(list : ^LinkedList, op : ^BinOpToken)
    
    var left : ^Token 
    
    if list->Size() > 0 then
      left := list->Remove(list->Size()-1)
      list->Add(op)
      list->Add(left)  
    else
      parserError("Missing left hand side parameter for operator '" + 
                  op->ToString() + "'."
      )
    end if  
  end parseP

  /**
   * Parse one alternative branch of a 'match .. with .. end' block. The
   * alternative is follows the form 'f x1 x2 .. xn -> e1 |' or 
   * 'f x1 x2 .. xn -> e end'.
   *
   * @return Alternative token.
   */
  fcn parseA() : ^Token
    
    var altToken : ^AltToken
    new altToken
    
    altToken->tMatch := parseE()
    if objectclass(tok) ~= ArrowToken then
      parserError("Missing '->' at '" + altToken->tMatch->ToString() + "'")
    end if
    
    altToken->tExpr := parseE()  
    if (objectclass(tok) ~= EndToken) and (objectclass(tok) ~= PipeToken) then
      parserError("Missing 'end' or '|' at '" + altToken->tExpr->ToString() + "'")
    end if
    
    result altToken
  end parseA

  /**
   * Parse a 'match .. with .. end' block.
   *
   * @return  Case token.
   */
  fcn parseM() : ^Token
    
    var caseToken : ^CaseToken
    new caseToken
    
    var alts : ^LinkedList
    new alts
    
    caseToken->tMatch := parseE()
    if objectclass(tok) ~= WithToken then
      parserError("Missing 'with' at '" + caseToken->tMatch->ToString() + "'")
    end if
    
    loop exit when objectclass(tok) = EndToken
      alts->Add(parseA())
    end loop    
    
    caseToken->tAlts := alts
    result caseToken
  end parseM

  /**
   * Parse an expression.
   *
   * @return  List of tokens.
   */
  body fcn parseE() : ^LinkedList
        
    var list : ^LinkedList
    new list
    
    loop exit when not(lexer->HasNext())
    
      tok := lexer->Next()
      if objectclass(tok) = NumToken then
        list->Add(tok)
      elsif objectclass(tok) = StringToken then
        list->Add(tok)
      elsif objectclass(tok) = IdToken then
        parseId(list, tok)
      elsif objectclass(tok) = BinOpToken then
        parseP(list, tok)
      elsif objectclass(tok) = MatchToken then
        list->Add(parseM())
      elsif objectclass(tok) = LeftParToken then
        list->Add(parseSE())
      elsif objectclass(tok) = LeftBraToken then
        list->Add(parseL())
      elsif objectclass(tok) = IfToken then
        list->Add(parseI())
      elsif objectclass(tok) = ThenToken then
        exit
      elsif objectclass(tok) = ElseToken then
        exit
      elsif objectclass(tok) = EndToken then
        exit
      elsif objectclass(tok) = RightParToken then 
        exit
      elsif objectclass(tok) = RightBraToken then 
        exit
      elsif objectclass(tok) = CommaToken then 
        exit
      elsif objectclass(tok) = WithToken then 
        exit
      elsif objectclass(tok) = PipeToken then 
        exit
      elsif objectclass(tok) = ArrowToken then
        exit
      else
        parserError("Unexpected token '" + tok->ToString() + "'.")
        result list
      end if
    end loop
    
    result list
  end parseE

  /**
   * Turns a linear stream of tokens into a structured tree of token lists.
   * This stage identifies blocks like lists, if blocks , match blocks or sub
   * expressions and builds a tree representation of these blocks. 
   * This stage rearranges infix and postfix identifiers by swapping the 
   * identifer and the previous parsed expression in the list.
   * If the stream does not start with a 'let' or 'data' token, adds
   * 'let main = ...' to the beginning of the input stream, which will cause 
   * this statement to be executed by the G-Machine immediately.
   *
   * @param tokens  Token input stream.
   * @return  A tokentree as specified above.
   */
  fcn build(tokens : ^LinkedList) : ^LinkedList
  
    if (tokens = nil) or (tokens->Size() = 0) then
      result nil
    end if
    lexer := tokens->GetIterator()
    
    % Skip data constructor definitions.
    if objectclass(tokens->Get(0)) = DataToken then
      result tokens
    end if
    
    var tokenTree : ^LinkedList
    
    if objectclass(tokens->Get(0)) = LetToken then
      
      var list : ^LinkedList
      new list
      
      % Strip off 'let f a b .. =' part. If f is a infix or postfix operator
      % the parser would rearange it which should not happen for the first
      % definition of an infix or postfix operator defined with '_f_' or '_f'.
      loop exit when not(lexer->HasNext())
        tok := lexer->Next()
        list->Push(tok)
        exit when objectclass(tok) = AsgnToken
      end loop
      
      tokenTree := parseE()
      
      % Re-attach the 'let f a b .. =' part.
      var iter : ^Iterator := list->GetIterator()
      loop exit when not(iter->HasNext())
        tokenTree->Push(iter->Next())
      end loop

      result tokenTree    
    else
      tokenTree := parseE()
      tokenTree->Push(asgnToken)
      tokenTree->Push(mainToken)
      tokenTree->Push(letToken)
      result tokenTree      
    end if
  end build
  
end TokenTreeBuilder