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
unit class Lexer 
  inherit Stage in "../ui/Stage.tu"
  import LinkedList    in "../util/LinkedList.tu",
         Iterator      in "../util/Iterator.tu",
         HashMap       in "../util/HashMap.tu",
         Token         in "tokens/Token.tu",
         ArrowToken    in "tokens/ArrowToken.tu",
         AsgnToken     in "tokens/AsgnToken.tu",
         BinOpToken    in "tokens/BinOpToken.tu",
         CommaToken    in "tokens/CommaToken.tu",
         DataToken     in "tokens/DataToken.tu",
         ElseToken     in "tokens/ElseToken.tu",
         EndToken      in "tokens/EndToken.tu",
         IdToken       in "tokens/IdToken.tu",
         IfToken       in "tokens/IfToken.tu",
         LetToken      in "tokens/LetToken.tu",
         LeftBraToken  in "tokens/LeftBraToken.tu",
         LeftParToken  in "tokens/LeftParToken.tu",
         MatchToken    in "tokens/MatchToken.tu",
         NumToken      in "tokens/NumToken.tu",
         PipeToken     in "tokens/PipeToken.tu",
         RightBraToken in "tokens/RightBraToken.tu",
         RightParToken in "tokens/RightParToken.tu",
         StringToken   in "tokens/StringToken.tu",
         ThenToken     in "tokens/ThenToken.tu",
         WithToken     in "tokens/WithToken.tu"  
  export setup, lex, getTokens, getIds

  /** Modes of the lexer. Either in lexing mode or in comment mode where the
      lexer ignores all characters until it reads a '}'. */
  type Mode : enum (LEX, COMMENT)
  var mode : Mode
  
  /** Index position within the input string. */
  var i : nat
  
  /** Length of the input string. */
  var len : nat
  
  /** Map of lexed ids: Variables, functions and constructors. This map is
      important for the church pretty printer. */
  var ids : ^HashMap
  
  /** List of recognized tokens. */
  var tokens : ^LinkedList
  
  % Static token objects.
  var asgnToken : ^AsgnToken
  new asgnToken

  var leftParToken : ^LeftParToken
  new leftParToken

  var rightParToken : ^RightParToken
  new rightParToken

  var leftBraToken : ^LeftBraToken
  new leftBraToken

  var rightBraToken : ^RightBraToken
  new rightBraToken

  var pipeToken : ^PipeToken
  new pipeToken
  
  var addOpToken : ^BinOpToken
  new addOpToken
  addOpToken->op := "+"
  
  var subOpToken : ^BinOpToken
  new subOpToken
  subOpToken->op := "-"
  
  var mulOpToken : ^BinOpToken
  new mulOpToken
  mulOpToken->op := "*"
  
  var divOpToken : ^BinOpToken
  new divOpToken
  divOpToken->op := "/"
  
  var ltOpToken : ^BinOpToken
  new ltOpToken
  ltOpToken->op := "<"
  
  var gtOpToken : ^BinOpToken
  new gtOpToken
  gtOpToken->op := ">"
  
  var leOpToken : ^BinOpToken
  new leOpToken
  leOpToken->op := "<="
  
  var geOpToken : ^BinOpToken
  new geOpToken
  geOpToken->op := ">="
  
  var eqOpToken : ^BinOpToken
  new eqOpToken
  eqOpToken->op := "=="
  
  var neOpToken : ^BinOpToken
  new neOpToken
  neOpToken->op := "!="

  var andOpToken : ^BinOpToken
  new andOpToken
  andOpToken->op := "&&"
  
  var orOpToken : ^BinOpToken
  new orOpToken
  orOpToken->op := "||"

  var arrowToken : ^ArrowToken
  new arrowToken
  
  var letToken : ^LetToken
  new letToken
  
  var dataToken : ^DataToken
  new dataToken

  var matchToken : ^MatchToken
  new matchToken

  var withToken : ^WithToken
  new withToken
  
  var commaToken : ^CommaToken
  new commaToken
  
  var ifToken : ^IfToken
  new ifToken
  
  var thenToken : ^ThenToken
  new thenToken
  
  var elseToken : ^ElseToken
  new elseToken
  
  var endToken : ^EndToken
  new endToken
  
  % Predefined Nil list constructor.
  var nilToken : ^IdToken
  new nilToken
  nilToken->value := "[]"
  nilToken->style := IdToken.Style.PREFIX
  
  % Predefined Cons list constructor.
  var consToken : ^IdToken
  new consToken
  consToken->value := "::"
  consToken->style := IdToken.Style.INFIX
  
  /**
   * Mandatory constructor. Initializes and resets the recognzed ids.
   * Registers the '[]' and '::' primitve constructors for lists.
   *
   * @param obj  A refernece to this lexer.
   */
  body proc New(obj : ^Object)
    this := obj
    new ids
    ids->New(ids)

    ids->Put("[]", nilToken)
    ids->Put("::", consToken)
  end New

  /**
   * Prints a lexer error message in red.
   *
   * @param msg  The error message string.
   */
  proc lexError(msg : string)
  
    if mode = Mode.COMMENT then
      return
    end if
    error("Lexer[" + intstr(i) + "]: ", msg)
  end lexError
  
  /**
   * Prints the list of tokens.
   *
   * @param tok  List of tokens.
   */
  proc printTokens(tok : ^LinkedList)
    put repeat("-", 80)
    put " Lexer"
    put repeat("-", 80)
    var iter : ^Iterator := tok->GetIterator()
    loop exit when not(iter->HasNext())
      put " ", iter->Next()->ToString()
    end loop
    put repeat("-", 80)
    put ""
  end printTokens

  /**
   * Adds a token to the current stream iff the lexer is in Mode.LEX.
   *
   * @param tok  The token.
   */
  proc addToken(tok : ^Token)
  
    if mode = Mode.LEX then
      tokens->Add(tok)
    end if
  end addToken

  /**
   * Retruns true if the character is a whitespace.
   * Rule: ws -> ' ' | '\t' | '\n'
   *
   * @param c  The character.
   * @return  true if whitespace.
   */
  fcn isWhitespace(c : char) : boolean
    result index(" \t\n", c) ~= 0
  end isWhitespace
  
  /**
   * Returns true if the character is numeric.
   * Rule: num -> '0' .. '9'
   *
   * @param c  The character.
   * @return  true if the character is numeric.
   */
  fcn isNum(c : char) : boolean
    result (c >= '0') and (c <= '9')
  end isNum

  /**
   * Returns true if the character is a lower or upper case alphabetic char.
   * Rule: alpha -> 'a' .. 'z' | 'A' .. 'Z'
   *
   * @param c  The character.
   * @return  true if the character is an alphabetic char.
   */
  fcn isAlpha(c : char) : boolean
    result ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z'))
  end isAlpha
  
  /**
   * Returns true if the character is either alphabetic or numeric.
   * Rule: alphNum -> alpha | num | ' | _
   *
   * @param c  The character.
   * @return  true if the character is alphanumeric.
   */  
  fcn isAlphaNum(c : char) : boolean
    result isAlpha(c) or isNum(c) or (c = '\'') or (c = '_')
  end isAlphaNum

  /**
   * Returns true if the character is either one of "=<>|-+*_/!:?&%$~#.@\^\\".
   * Rule: special -> = | < | > | '|' | - | + | * | _ | / | ! | : | ? | & |% |
   *                  $ | ~ | # | . | @ | ^ | \
   *
   * @param c  The character.
   * @return  true if a special character.
   */
  fcn isSpecial(c : char) : boolean
    result index("=<>|-+*_/!:?&%$~#.@\^\\", c) ~= 0
  end isSpecial

  /**
   * Returns true if the character is one of "[,]".
   * Rule: list -> [ | , | ]
   *
   * @param c  The character.
   * @return  true if a list operational character.
   */
  fcn isList(c : char) : boolean
    result index("[,]", c) ~= 0
  end isList

  /**
   * Recognizes all numeric characters up to the first non-numeric character.
   * Creates a number token if the number fits into a 32bit signed integer.
   *
   * @param input  Character input stream.
   */
  proc lexNum(input : string)
    
    if mode = Mode.COMMENT then
      return
    end if
    
    var tok : ^NumToken
    new tok
    var numStr : string := input(i)
    
    loop exit when (i+1 > len) or not(isNum(input(i+1)))
      numStr += input(i+1)
      i += 1
    end loop
    
    if strintok(numStr) then
      tok->value := strint(numStr)
      addToken(tok)
    else
      lexError("Integer overflow.")
    end if
  end lexNum

  /**
   * Recognizes a string constant enclosed in '...'. To read string constants
   * defined in Church source files, enclose it with "'...'" double quotes 
   * additionally. This prevents Turing from chunking the string literal into
   * single words without whitespaces.
   *
   * @param input  Character input stream.
   */
  proc lexStringLiteral(input : string)
    
    var strToken : ^StringToken
    new strToken
    
    strToken->value := ""
    loop 
      i += 1
      exit when (i > len) or (input(i) = '\'')
      strToken->value += input(i)
    end loop
    addToken(strToken)
  end lexStringLiteral
  
  /**
   * Lexes the first alphabetic and all following characters until the first
   * occurence of a non-alphanumeric character.
   *
   * @param input  Character input stream.
   * @return  The recognized string.
   */
  fcn lexString(input : string) : string
  
    var str : string := input(i)
    
    loop exit when (i+1 > len) or not(isAlphaNum(input(i+1)))
      str += input(i+1)
      i += 1
    end loop   
    result str
  end lexString

  /**
   * Lexes a sequence of special characters until the next character is not a
   * special one.
   *
   * @param input  Character input stream.
   * @return  The recognized string. 
   */
  fcn lexSpecial(input : string) : string
  
    var str : string := input(i)

    loop exit when (i+1 > len) or not(isSpecial(input(i+1)))
      str += input(i+1)
      i += 1
    end loop
    result str
  end lexSpecial

  /**
   * Lexes list operational characters.
   * Rule: list -> [ | , | ] | []
   *
   * @param input  Character input stream.
   * @return  The recognized string. 
   */
  fcn lexList(input : string) : string
  
    var str : string := input(i)
    
    if str = "[" then    
      if (i < len) and input(i+1) = ']' then
        str += "]"
        i += 1
      end if
    end if
    result str
  end lexList

  /**
   * Strips off the underscores of an identifer. Underscores mark an identifer
   * as either PREFIX, POSTFIX or INFIX and will be encoded within the style
   * parameter of an IdToken.
   * 
   * @param str  Identifer string with or without leading or trailing 
   *             underscores.
   * @return  Identifer string without leading or trailing underscores.
   */
  fcn trimId(str : string) : string
    
    var id : string := str
    
    if str(1) = "_" then
      id := id(2 .. *)
    end if
    if str(*) = "_" then
      id := id(1 .. *-1)
    end if
    result id
  end trimId

  /**
   * Extracts the style of an identifer.
   * o PREFIX    f_ | f
   * o INFIX    _f_
   * o POSTFIX  _f
   */
  fcn getIdStyle(str : string) : IdToken.Style
  
    if str(1) = "_" then
      if str(*) = "_" then
        result IdToken.Style.INFIX
      else
        result IdToken.Style.POSTFIX
      end if
    else
      result IdToken.Style.PREFIX
    end if
  end getIdStyle
 
  /**
   * Creates an identifier token.
   *
   * @param str  Lexed identifier string.
   */
  proc lexId(str : string)
    
    if mode = Mode.COMMENT then
      return
    end if
    
    var id : string := trimId(str)
    var tok : ^IdToken := ids->Get(id)
    
    if tok = nil then
      new tok
      tok->value := id
      tok->style := getIdStyle(str)
      ids->Put(id, tok)
    end if 
    addToken(tok)
  end lexId
   
  /**
   * Resets the token list. 
   */
  proc setup()
  
    % Setup a new token stream, reset the index counter and clear any errorous
    % states of the lexer stage.
    new tokens
    % Initialze in lexing mode.
    mode := Mode.LEX
  end setup
  
  /**
   * Turns an input string into a list of tokens. Skips unknown characters and 
   * prints an error message.
   * Returns true if the input string conains a ':'. The remaining characters
   * after the ';' are ignored.
   *
   * @param input  Character input stream.
   * @return  True if input string contains a ';'.
   */
  fcn lex(input : string) : boolean
    
    var c   : char
    var str : string
    
    i := 1
    len := length(input)
    
    loop exit when i > len
    
      c := input(i)
      if isWhitespace(c) then
        % Skip all whitespace characters.
      elsif c = ';' then
        result (mode = Mode.LEX)
      elsif c = '{' then
        mode := Mode.COMMENT
      elsif c = '}' then
        mode := Mode.LEX
      elsif c = '(' then
        addToken(leftParToken)
      elsif c = ')' then
        addToken(rightParToken)
      elsif c = '\'' then
        lexStringLiteral(input)
      elsif isNum(c) then
        lexNum(input)
      elsif isAlpha(c) then
        % Grab the next string greedily.
        str := lexString(input)
        case str of
          label "if"   : addToken(ifToken)
          label "let"  : addToken(letToken)
          label "end"  : addToken(endToken)
          label "then" : addToken(thenToken)
          label "else" : addToken(elseToken)
          label "data" : addToken(dataToken)
          label "with" : addToken(withToken)
          label "match": addToken(matchToken)
          label        : lexId(str)
          % TODO: Infix, Postfix support.
        end case
      elsif isSpecial(c) then
        % Grab the next string greedily.
        str := lexSpecial(input)
        case str of
          label "=" : addToken(asgnToken)
          label "|" : addToken(pipeToken)
          label "-" : addToken(subOpToken)
          label "+" : addToken(addOpToken)
          label "*" : addToken(mulOpToken)
          label "/" : addToken(divOpToken)
          label "<" : addToken(ltOpToken)
          label ">" : addToken(gtOpToken)
          label "_" : lexError("'__' is an invalid identifier.")
          label "::": addToken(consToken)
          label "<=": addToken(leOpToken)
          label ">=": addToken(geOpToken)
          label "==": addToken(eqOpToken)
          label "!=": addToken(neOpToken)
          label "->": addToken(arrowToken)
          label "&&": addToken(andOpToken)
          label "||": addToken(orOpToken)
          label "__": lexError("'__' is an invalid identifier.")
          label     : lexId(str)
        end case
      elsif isList(c) then
        str := lexList(input)
        case str of
          label "[" : addToken(leftBraToken)
          label "]" : addToken(rightBraToken)
          label "," : addToken(commaToken)
          label "[]": addToken(nilToken)
        end case
      else
        lexError("Unknown character '" + input(i) + "'.")
      end if
      i += 1
    end loop
    result false
  end lex 

  /**
   * Returns the lexed tokens so far. Does not clear the token list. 
   * Call setup() before lexing a new statement.
   *
   * @return  A list of tokens.
   */
  fcn getTokens() : ^LinkedList
  
    if debug then
      printTokens(tokens)
    end if
    result tokens
  end getTokens

  /**
   * Returns the lexed identifiers so far.
   *
   * @return  A Map of lexed identifiers.
   */
  fcn getIds() : ^HashMap
    result ids
  end getIds
  
end Lexer