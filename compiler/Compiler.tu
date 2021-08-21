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
unit class Compiler
  inherit Stage in "../ui/Stage.tu"
  import LinkedList  in "../util/LinkedList.tu",
         Iterator    in "../util/Iterator.tu",
         HashMap     in "../util/HashMap.tu",
         ACon        in "../parser/ast/ACon.tu",
         ANum        in "../parser/ast/ANum.tu",
         AString     in "../parser/ast/AString.tu",
         AVar        in "../parser/ast/AVar.tu",
         Expr        in "../parser/ast/Expr.tu",
         EAlt        in "../parser/ast/EAlt.tu",
         EAp         in "../parser/ast/EAp.tu",
         EBinOp      in "../parser/ast/EBinOp.tu",
         ECon        in "../parser/ast/ECon.tu",
         EIf         in "../parser/ast/EIf.tu",
         EMatch      in "../parser/ast/EMatch.tu",
         ESc         in "../parser/ast/ESc.tu",
         IBinOp      in "instruction/IBinOp.tu",
         ICaseJump   in "instruction/ICaseJump.tu",
         IEval       in "instruction/IEval.tu",
         IIf         in "instruction/IIf.tu",
         IMakeAp     in "instruction/IMakeAp.tu",
         IPack       in "instruction/IPack.tu",
         IPop        in "instruction/IPop.tu",
         IPush       in "instruction/IPush.tu",
         IPushGlobal in "instruction/IPushGlobal.tu",
         IPushInt    in "instruction/IPushInt.tu",
         IPushString in "instruction/IPushString.tu",
         ISlide      in "instruction/ISlide.tu",
         IUnpack     in "instruction/IUnpack.tu",
         IUnwind     in "instruction/IUnwind.tu",
         IUpdate     in "instruction/IUpdate.tu", 
         NGlobal     in "../machine/node/NGlobal.tu"
  export compile
  
  /** Map of bound variables by the super combinator. */
  var varStack : ^LinkedList
  
  /** Compiled super combinator body source code. */
  var codeStack : ^LinkedList

  % Instruction objects that need not to be created more than once, since they
  % don't encapsulate context-sensitive data.  
  var iMakeAp : ^IMakeAp
  new iMakeAp 
  
  var iEval : ^IEval
  new iEval
  
  var iUnwind : ^IUnwind
  new iUnwind
  
  var iUnpack : ^IUnpack
  new iUnpack
  
  var iAdd : ^IBinOp
  new iAdd
  iAdd->op := "+"

  var iSub : ^IBinOp
  new iSub
  iSub->op := "-"
  
  var iMul : ^IBinOp
  new iMul
  iMul->op := "*"
  
  var iDiv : ^IBinOp
  new iDiv
  iDiv->op := "/"

  var iLt : ^IBinOp
  new iLt
  iLt->op := "<"
  
  var iGt : ^IBinOp
  new iGt
  iGt->op := ">"

  var iLe : ^IBinOp
  new iLe
  iLe->op := "<="
  
  var iGe : ^IBinOp
  new iGe
  iGe->op := ">="
  
  var iEq : ^IBinOp
  new iEq
  iEq->op := "=="
  
  var iNe : ^IBinOp
  new iNe
  iNe->op := "!="

  var iAnd : ^IBinOp
  new iAnd
  iAnd->op := "&&"
  
  var iOr : ^IBinOp
  new iOr
  iOr->op := "||"

  /**
   * Prints the compiled global code.
   *
   * @param g  A Global node.
   */
  proc printGlobal(g : ^NGlobal)
    if g ~= nil then
      put repeat("-", 80)
      put " Compiler"
      put repeat("-", 80)
      put " ", g->ToString()
      put repeat("-", 80)
      put ""
    end if
  end printGlobal
  
  /**
   * Prints a compiler error message in red.
   *
   * @param msg  The error message string.
   */
  proc compilerError(msg : string)
    error("Compiler: ", msg)
  end compilerError  

  /**
   * Peeks the top code stack list element.
   *
   * @return  Top code list on code stack.
   */
  fcn codePeek() : ^LinkedList
    result codeStack->Peek()
  end codePeek

  /**
   * Creates a new empty code list on the code stack.
   */  
  proc newCode()
    var code : ^LinkedList
    new code
    codeStack->Push(code)
  end newCode

  /**
   * Peeks the top map on the symbol table stack.
   *
   * @return  Top variable map on symbol table stack.
   */  
  fcn varPeek() : ^HashMap
    result varStack->Peek()
  end varPeek

  /**
   * Gets the i-th variable map on the symbol table stack.
   *
   * @param i  Index on the symbol table stack.
   * @return  i-th variable map on the symbol table stack.
   */  
  fcn varGet(i : nat) : ^HashMap
    result varStack->Get(i)
  end varGet
  
  /**
   * Compiles a single number(integer) token into a PushInt instruction.
   * Rule: ANum(n) -> PushInt n
   *
   * @param n  Atomic integer expression.
   */
  proc compileInt(n : ^ANum)
    var iPushInt : ^IPushInt
    new iPushInt    
    iPushInt->value := n->value
    codePeek()->Add(iPushInt)
  end compileInt

  /**
   * Compiles a string constant token into a PushString instruction.
   * Rule: AString(s) -> PushString s
   *
   * @param s  Atomic string expression.
   */  
  proc compileString(s : ^AString)
    var iPushString : ^IPushString
    new iPushString
    iPushString->value := s->value
    codePeek()->Add(iPushString)
  end compileString
  
  /**
   * Compiles a single variable token into a PushGlobal instruction.
   * Rule: AVar(g) -> PushGlobal g
   *
   * @param v  Atomic variable expression.
   */
  proc compileG(v : ^AVar)
    var iPushGlobal : ^IPushGlobal
    new iPushGlobal
    iPushGlobal->name := v->name
    codePeek()->Add(iPushGlobal)
  end compileG
 
  /**
   * Compiles a locally bound variable into a Push instruction.
   * 
   * @param idx  The position of the variable in the list of locally bound
   *             variables.
   * @param off  Offset to the top element on the stack.
   */
  proc compileLV(idx : int, off : int)  
    var iPush : ^IPush
    new iPush
    iPush->position := idx + off
    codePeek()->Add(iPush)   
  end compileLV
  
  forward proc compileE(e : ^Expr, off : int)
  
  /**
   * Create a MakeAp instruction. Compile the right and left subexpressions
   * first.
   * Rule: EAp(l, r) -> compile(r), compile(l), MakeAp
   *
   * @params ap   Application expression.
   * @params off  Offset to the top element on the stack.
   */
  proc compileAp(ap : ^EAp, off : int)  
    compileE(ap->right, off)
    compileE(ap->left, off + 1)
    codePeek()->Add(iMakeAp)
  end compileAp

  /**
   * Compile data constructor expression.
   *
   * @param con  Constructor expression.
   */
  proc compileCon(con : ^ECon)
    
    var iter : ^Iterator := con->params->GetIterator()
    var expr : ^Expr
    
    for i : 0 .. con->params->Size()-1
      expr := iter->Next()
      compileE(expr, i)
      if objectclass(expr) ~= ANum then
        codePeek()->Add(iEval)
      end if
    end for
    
    % Global hashmap of Pack instructions.
    var iPack : ^IPack
    new iPack
    iPack->name := con->name
    iPack->arity := con->params->Size()
    codePeek()->Add(iPack)
  end compileCon
 
  /**
   * Compile built-in primitive expression.
   *
   * @param op  Primitive operator expression.
   * @param off  Offset to the variables on the stack.
   */
  proc compileBinOp(op : ^EBinOp, off : int)
    
    compileE(op->right, off)
    if objectclass(op->right) ~= ANum then
      codePeek()->Add(iEval)
    end if
    
    compileE(op->left, off + 1)
    if objectclass(op->left) ~= ANum then
      codePeek()->Add(iEval)
    end if
    
    case op->op of
      label "+" : codePeek()->Add(iAdd)
      label "-" : codePeek()->Add(iSub)
      label "*" : codePeek()->Add(iMul)
      label "/" : codePeek()->Add(iDiv)
      label "<" : codePeek()->Add(iLt)
      label ">" : codePeek()->Add(iGt)
      label "<=": codePeek()->Add(iLe)
      label ">=": codePeek()->Add(iGe)
      label "==": codePeek()->Add(iEq)
      label "!=": codePeek()->Add(iNe)
      label "&&": codePeek()->Add(iAnd)
      label "||": codePeek()->Add(iOr)
      label     : compilerError("Unsupported binary operator '" + op->op + "'.")
    end case
  end compileBinOp  

  /**
   * Compile variable expression. Searches the symblo table stack for the 
   * variable. If it is not defined compile as global, else as locally bound
   * identifer.
   *
   * @param v  Atomic variable expression.
   * @param off  Offset to the variables on the stack.
   */
  proc compileEV(v : ^AVar, off : int)
  
    var vars : ^HashMap
    var idx : nat
    var varOff : int := off
    
    % Move down the symbol table stack, and check if the local variable 
    % is present (idx > 0).
    for i : 0 .. varStack->Size()-1
      vars := varGet(i)
      idx := vars->GetIndex(v->name)
      if idx > 0 then
        compileLV(idx-1, varOff)
        return
      end if
      % As we descend the symbol table stack, increment the offset according
      % to the size of each level of variables.
      varOff += vars->Size()
    end for
    % Else compile as global function.
    compileG(v)
  end compileEV
 
  /**
   * Compile match alternative expression.
   *
   * @param a  Match alternative expression.
   * @param cj  CaseJump instruction to which to attach the alternative.
   * @param off  Offset to the variables on the stack.
   */
  proc compileAlt(a : ^EAlt, cj : ^ICaseJump, off : int)
    
    varStack->Push(a->vars)
    newCode()
    
    codePeek()->Add(iUnpack)
    compileE(a->expr, off)
    var iSlide : ^ISlide
    new iSlide
    iSlide->count := a->vars->Size()
    codePeek()->Add(iSlide)
    
    cj->alts->Put(a->name, codeStack->Pop())
    var oldvar := varStack->Pop()
  end compileAlt

  /**
   * Compile 'match' expression.
   *
   * @param m  Match expression.
   * @param off  Offset to the variables on the stack.
   */
  proc compileM(m : ^EMatch, off : int)
  
    compileE(m->mexp, off)
    % Evaluate the match expression to WHNF.
    codePeek()->Add(iEval)
    
    var iCaseJump : ^ICaseJump
    new iCaseJump
    iCaseJump->New(iCaseJump)
    
    var iter : ^Iterator := m->alts->GetIterator()
    loop exit when not(iter->HasNext())
      compileAlt(iter->Next(), iCaseJump, off)
    end loop
    codePeek()->Add(iCaseJump)
  end compileM
 
  /**
   * Compile 'if' expression.
   *
   * @param e  If expression.
   * @param off  Offset to the variables on the stack.
   */
  proc compileIf(e : ^EIf, off : int)
    
    compileE(e->eCond, off)
    % Evaluate the if condition to WHNF.
    codePeek()->Add(iEval)
    
    var iIf : ^IIf
    new iIf
    
    newCode()
    compileE(e->eThen, off)
    iIf->cThen := codeStack->Pop()

    newCode()
    compileE(e->eElse, off)
    iIf->cElse := codeStack->Pop()
    
    codePeek()->Add(iIf)
  end compileIf

  /**
   * Compile expression.
   *
   * @param e  Expression.
   * @param off  Offset to the variables on the stack.
   */
  body proc compileE(e : ^Expr, off : int)
      
    if objectclass(e) = EAp then
      compileAp(e, off)
    elsif objectclass(e) = ECon then
      compileCon(e)
    elsif objectclass(e) = EMatch then
      compileM(e, off)
    elsif objectclass(e) = EIf then
      compileIf(e, off)
    elsif objectclass(e) = EBinOp then
      compileBinOp(e, off)
    elsif objectclass(e) = ANum then
      compileInt(e)
    elsif objectclass(e) = AString then
      compileString(e)
    elsif objectclass(e) = AVar then
      compileEV(e, off)
    end if
  end compileE

  /**
   * Compile supercombinator expression.
   *
   * @param e Expression.
   * @return  Source code list.
   */
  fcn compileR(e : ^Expr) : ^LinkedList
  
    compileE(e, 0)

    var iUpdate : ^IUpdate
    new iUpdate
    iUpdate->size := varPeek()->Size()
    codePeek()->Add(iUpdate)
    
    if varPeek()->Size() > 0 then
      var iPop : ^IPop
      new iPop
      iPop->size := varPeek()->Size()
      codePeek()->Add(iPop)
    end if
    codePeek()->Add(iUnwind)    
    result codeStack->Pop()
  end compileR

  /**
   * Compiles a supercombinator into a global node.
   *
   * @param sc  Supercombinator.
   * @return  Global node.
   */
  fcn compile(sc : ^ESc) : ^NGlobal
    
    if sc = nil then
      result nil
    end if
    
    var g : ^NGlobal
    new g
    g->New(g)
    
    new codeStack
    newCode()
    new varStack

    varStack->Push(sc->vars)
    g->name := sc->name
    g->arity := sc->vars->Size()
    g->code := compileR(sc->expr)
    
    if debug then
      printGlobal(g)
    end if
    result g    
  end compile
  
end Compiler