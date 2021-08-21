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
unit class Command
  inherit Object in "../util/Object.tu"
  import LinkedList in "../util/LinkedList.tu",
         CommandToken
  export lex
  
  /** Index position within the input string. */
  var i : nat
  
  /** Length of the input string. */
  var len : nat
  
  var tokens : ^LinkedList
  
  % Static token objects.
  var colonToken : ^CommandToken
  new colonToken
  colonToken->value := ":"

  /**
   * Retruns true if the character is a whitespace.
   * Rule: ws -> ' ' | '\t' | '\n'
   *
   * @param c  The character.
   * @return  true if whitespace.
   */
  fcn isWhitespace(c : char) : boolean
    result index(" \t", c) ~= 0
  end isWhitespace
  
  /**
   * Lexes the first alphabetic and all following characters until the first
   * occurence of a non-alphanumeric character.
   *
   * @param input  Character input stream.
   * @return  The recognized string.
   */
  fcn lexString(input : string) : string
  
    var str : string := input(i)
    
    loop exit when (i+1 > len) or isWhitespace(input(i+1))
      str += input(i+1)
      i += 1
    end loop   
    result str
  end lexString

  /**
   * Lexes a command word.
   *
   * @param input  Input string.
   * @return  List of command tokens.
   */
  fcn lex(input : string) : ^LinkedList
    
    var c : char 
    var str : string
    var tok : ^CommandToken
    
    new tokens
    len := length(input)
    i := 1

    loop exit when i > len
    
      c := input(i)
      if isWhitespace(c) then   
      elsif c = ':' then
        tokens->Add(colonToken)
      else
        new tok
        tok->value := lexString(input)
        tokens->Add(tok)
      end if
      i += 1
    end loop
    result tokens
  end lex
  
end Command