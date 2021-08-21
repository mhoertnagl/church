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
unit class Church
  inherit Stage in "../ui/Stage.tu"
  import Object           in "../util/Object.tu",
         LinkedList       in "../util/LinkedList.tu",
         Iterator         in "../util/Iterator.tu",
         HashMap          in "../util/HashMap.tu",
         Command          in "../ui/Command.tu",
         CommandToken     in "../ui/CommandToken.tu",
         Lexer            in "../lexer/Lexer.tu",
         TokenTreeBuilder in "../parser/TokenTreeBuilder.tu",
         Parser           in "../parser/Parser.tu",
         Compiler         in "../compiler/Compiler.tu",
         Machine          in "../machine/Machine.tu",
         IdToken          in "../lexer/tokens/IdToken.tu",
         Expr             in "../parser/ast/Expr.tu",
         Node             in "../machine/node/Node.tu",
         NNum             in "../machine/node/NNum.tu",
         NPack            in "../machine/node/NPack.tu",
         NString          in "../machine/node/NString.tu"
  export start

  /** States of the Church interpreter. */
  type State : enum (START, COMMAND, CMDLOAD, CMDDEBUG, CMDHELP, CMDHALT, EXECUTE)
  var state  : State

  /** Raw input line from the console. */
  var line : string
  
  /** Token stream for commands. */
  var cmdStream : ^Iterator
  
  /** Current command token. */
  var cmdToken : ^CommandToken
  
  /** True if the source file lexer read a end of statement ';'. */
  var stop : boolean
  
  /** Program code tokens after lexing. */
  var tokens : ^LinkedList
  
  /** After treeifying the token stream. */
  var tTree : ^LinkedList
  
  /** Abstract syntax tree after parsing. */
  var ast : ^Expr
  
  /** Global function node after compilation. */
  var glob : ^Node
  
  /** Result node after execution. */
  var res : ^Node

  /** Stages of the Church environment. */ 
  var command  : ^Command
  var lexer    : ^Lexer
  var tBuilder : ^TokenTreeBuilder
  var parser   : ^Parser
  var compiler : ^Compiler
  var machine  : ^Machine
  
  forward proc loadFile(filename : string)

  /**
   * Mandatory constructor. Initializes all stages of the interpreter. Loads
   * the standard CHURCH prelude 'Prelude.church'.
   *
   * @param obj  A pointer to this object.
   */  
  body proc New(obj : ^Object)
    this := obj
    
    new command
    new lexer
    lexer->New(lexer)
    new tBuilder
    new parser
    parser->New(parser)
    new compiler
    new machine
    machine->New(machine)
    
    % Initialize state machine.
    state := State.START
    % Load Prelude.
    loadFile("Prelude.church")
  end New

  /**
   * Prints an error message in red. Sets the state of the stage to ERROR.
   *
   * @param msg  Error message string.
   */
  proc churchError(msg : string)
    error(" Church: ", msg)
  end churchError

  /**
   * Prints a warning in blue.
   *
   * @param msg  Warning message string.
   */
  proc churchWarning(msg : string)
    warning(" Church: ", msg)
  end churchWarning

  /**
   * Prints an info in green.
   *
   * @param msg  Info message string.
   */
  proc churchInfo(msg : string)
    info(" Church: ", msg)
  end churchInfo
  
  forward proc printResult(n : ^Node)
  
  proc printPackParams(iter : ^Iterator)
    loop exit when not(iter->HasNext())
      printResult(iter->Next())
      if iter->HasNext() then
        put " " ..
      end if
    end loop
  end printPackParams
  
  /**
   * Pretty prints a constructor node.
   *
   * @param n  Constructor node.
   */ 
  proc printPack(n : ^NPack)

    var iter : ^Iterator := n->params->GetIterator()
    % Use the id map of the lexer to determine if the current construtor
    % is a prefix, infix or postfix constructor.
    var ids  : ^HashMap  := lexer->getIds()
    var id   : ^IdToken  := ids->Get(n->name)
    
    case id->style of
      label IdToken.Style.PREFIX :
        put n->name, " " ..
        printPackParams(iter)
      label IdToken.Style.INFIX  :
        if iter->HasNext() then
          printResult(iter->Next())
          put " " ..
        end if
        put n->name, " " ..
        printPackParams(iter)
      label IdToken.Style.POSTFIX:
        printPackParams(iter)
        put n->name, " " ..
    end case
  end printPack

  /**
   * Pretty prints a list.
   *
   * @param n  Constructor node.
   */  
  proc printList(n : ^NPack)

    if n->name = "::" then
      printResult(n->params->Get(0))
      var t : ^Node := n->params->Get(1)
      % Check if second parameter is a constructor again.
      if objectclass(t) = NPack then
        var p : ^NPack := t
        if p->name = "::" then
          put "," ..
          printList( p )
        end if
      end if
    end if
  end printList

  /**
   * Pretty prints the result node of the G-Machine execution.
   *
   * @param n  The result node.
   */  
  body proc printResult(n : ^Node)
    if n ~= nil then
      if objectclass(n) = NNum then
        n->print()
        %put " " ..
      elsif objectclass(n) = NString then
        n->print()
        %put " " ..
      elsif objectclass(n) = NPack then
        % Special case: lists.
        if NPack(n).name = "::" or NPack(n).name = "[]" then
          put "[" ..
          printList(n)
          put "]" ..
        else
          put "(" ..
          printPack(n)
          put ")" ..
        end if
      end if
    end if
  end printResult
  
  /**
   * Loads a CHURCH source file. Executes all satements that do not begin with 
   * 'data ...' or 'let main ...' immediately.
   * The source file location can be defined relativly with Linux style path
   * syntax. 
   * Examples:
   *  o C:/example/source.church
   *  o ../example/source.church
   *  o example/source.church
   *  o source.church
   *
   * @param filename  Path to souce file.
   */ 
  body proc loadFile(filename : string)

    var fid   : int
    var token : string
    
    open : fid, filename, get
    
    % File does not exist.
    if fid < 0 then
      churchError("File '" + filename + "' does not exist.")
      return
    end if
    
    loop exit when eof(fid)
      lexer->setup()
      % BUG: Attempt to read past eof, though we check it.
      loop exit when eof(fid)
        get : fid, token
        exit when lexer->lex(token)
      end loop
      
      tokens := lexer->getTokens()
      tTree  := tBuilder->build(tokens)
      ast    := parser->parse(tTree)
      glob   := compiler->compile(ast)
      
      %machine->addGlobal(glob)
      res := machine->run(glob)
      if res ~= nil then
        printResult(res)
        put ""
      end if
    end loop
    
    churchInfo("File '" + filename + "' loaded.")
    close : fid
  end loadFile

  /**
   * Start state.
   *
   * @return  The next state.
   */   
  fcn stateStart() : State
  
    put ">> " .. get line : *
    cmdStream := command->lex(line)->GetIterator()
    
    if cmdStream->HasNext() then
      cmdToken := cmdStream->Next()
      case cmdToken->value of
        label ":": result State.COMMAND
        label    : result State.EXECUTE
      end case
    end if
    result State.START
  end stateStart

  /**
   * Command words state. Accepted commands are:
   *  o load   Load a file.
   *  o debug  Enable/disable debug mode for some stage(s).
   *  o help   Prints the Help Screen.
   *  o halt   Halts the interpreter.
   *
   * @return  The next state.
   */   
  fcn stateCommand() : State
  
    if cmdStream->HasNext() then
    
      cmdToken := cmdStream->Next()
      case cmdToken->value of
        label "load" : result State.CMDLOAD
        label "debug": result State.CMDDEBUG
        label "help" : result State.CMDHELP
        label "halt" : result State.CMDHALT
        label        : churchWarning("Unknown command. Type ':help' for a " + 
                                     "list of valid commands."
                       )
      end case
    else
      churchError("Expecting command word. Type ':help' for a list of " + 
                  "valid commands."
      )
    end if
    result State.START
  end stateCommand

  /**
   * Load command state. Loads a file.
   *
   * @return  The next state.
   */    
  fcn stateLoad() : State
  
    if cmdStream->HasNext() then
      cmdToken := cmdStream->Next()
      loadFile(cmdToken->value)
    end if
    result State.START
  end stateLoad

  /**
   * Debug command state. Enables/disables debug mode for some stage(s).
   *
   * @return  The next state.
   */    
  fcn stateDebug() : State
  
    loop exit when not(cmdStream->HasNext())
    
      cmdToken := cmdStream->Next()
      case cmdToken->value of
        label "+l": lexer->enableDebug(true)
        label "-l": lexer->enableDebug(false)
        label "+p": parser->enableDebug(true)
        label "-p": parser->enableDebug(false)
        label "+c": compiler->enableDebug(true)
        label "-c": compiler->enableDebug(false)
        label "+m": machine->enableDebug(true)
        label "-m": machine->enableDebug(false)
        label        : churchWarning("Unknown command. Type ':help' for a " + 
                                     "list of valid commands."
                       )
      end case
    end loop
    result State.START
  end stateDebug

  /**
   * Print help screen command state.
   *
   * @return  The next state.
   */   
  fcn stateHelp() : State
  
    var fid  : int
    var line : string
    
    open : fid, "README.txt", get
    
    % File does not exist.
    if fid < 0 then
      churchError("'README.txt' does not exist.")
      result State.START
    end if
    
    loop exit when eof(fid)
      get : fid, line : *
      put line
    end loop
    result State.START
  end stateHelp

  /**
   * Execute a supercombinator with the name 'main'.
   *
   * @return  The next state.
   */   
  fcn stateExecute() : State
  
    lexer->setup()
    
    stop   := lexer->lex(line)
    tokens := lexer->getTokens()      
    tTree  := tBuilder->build(tokens)
    ast    := parser->parse(tTree)
    glob   := compiler->compile(ast)
    res    := machine->run(glob)
    
    if res ~= nil then
      printResult(res)
      put ""
    end if
    result State.START    
  end stateExecute  
 
  /**
   * Starts the interpreter state machine.
   */
  proc start()  
    loop
      case state of
        label State.START   : state := stateStart()
        label State.COMMAND : state := stateCommand()
        label State.CMDLOAD : state := stateLoad()
        label State.CMDDEBUG: state := stateDebug()
        label State.CMDHELP : state := stateHelp()
        label State.EXECUTE : state := stateExecute()
        label State.CMDHALT :
          churchInfo("Halted.")
          exit
      end case
    end loop
  end start
  
end Church