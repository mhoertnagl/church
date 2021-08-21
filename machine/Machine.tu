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
unit class Machine
  inherit Stage in "../ui/Stage.tu"
  import Iterator    in "../util/Iterator.tu",
         LinkedList  in "../util/LinkedList.tu",
         HashMap     in "../util/HashMap.tu",         
         Instruction in "../compiler/instruction/Instruction.tu",
         IBinOp      in "../compiler/instruction/IBinOp.tu",
         ICaseJump   in "../compiler/instruction/ICaseJump.tu",
         IEval       in "../compiler/instruction/IEval.tu",
         IIf         in "../compiler/instruction/IIf.tu",
         IMakeAp     in "../compiler/instruction/IMakeAp.tu",
         IPack       in "../compiler/instruction/IPack.tu",
         IPop        in "../compiler/instruction/IPop.tu",
         IPush       in "../compiler/instruction/IPush.tu",
         IPushGlobal in "../compiler/instruction/IPushGlobal.tu",
         IPushInt    in "../compiler/instruction/IPushInt.tu",
         IPushString in "../compiler/instruction/IPushString.tu",
         ISlide      in "../compiler/instruction/ISlide.tu",
         IUnpack     in "../compiler/instruction/IUnpack.tu",
         IUnwind     in "../compiler/instruction/IUnwind.tu",
         IUpdate     in "../compiler/instruction/IUpdate.tu",         
         Node        in "node/Node.tu",
         NAp         in "node/NAp.tu",
         NNum        in "node/NNum.tu",
         NGlobal     in "node/NGlobal.tu",
         NPack       in "node/NPack.tu",
        NString     in "node/NString.tu"
  export addGlobal, run
    
  /** Instruction counter. */
  var icnt : nat
  
  /** The current instruction */
  var ins : ^Instruction
  
  /** Instruction pointer to the next operation of execution. */
  var code : ^Iterator
  
  /** Stack of nodes. All operations are performed on it. */
  var stack : ^LinkedList
  
  /** A stack of pointers to instruction lists. The top list pointer references
      the next instruction to be executed by the function step(). */
  var dump : ^LinkedList
 
  /** Maps nodes and their hash codes. This additional indirection allows to
      replace (update) a node by another. The function update(^IUpdate) 
      substitutes a node by its computational result. */
  var heap : ^HashMap
  
  /** Map of all available global functions. The function addGlobal(^NGlobal) 
      adds new global functions to this map at runtime. */
  var globals : ^HashMap

  /**
   * Mandatory constructor. Initialzes all data structures.
   *
   * @param obj  Reference to this object.
   */
  body proc New(obj : ^Object)
    this := obj
    new stack
    stack->New(stack)
    new dump
    dump->New(dump)
    new heap
    heap->New(heap)
    new globals
    globals->New(globals)
  end New

  /**
   * Prints an error message in red.
   *
   * @param msg  Error message string.
   */
  proc machineError(msg : string)
    error("G-Machine: ", msg)
  end machineError
  
  /**
   * Prints a warning in blue.
   *
   * @param msg  Warning message string.
   */
  proc machineWarning(msg : string)
    warning("G-Machine: ", msg)
  end machineWarning

  /**
   * Prints an info in green.
   *
   * @param msg  Info message string.
   */
  proc machineInfo(msg : string)
    info("G-Machine: ", msg)
  end machineInfo

  forward fcn heapGet(n : ^Node) : ^Node

  /**
   * Prints out the stack contents.
   */
  proc printStack()
    
    var i : ^Iterator := stack->GetIterator()
    var n : ^Node
    
    put repeat("-", 80)
    if ins ~= nil then
      put " Stack after '", ins->ToString(), "' (" + intstr(icnt), " steps taken)"
    else
      put " Stack (", intstr(icnt), " steps taken)"
    end if
    put repeat("-", 80)
    loop exit when not(i->HasNext())
      n := heapGet(i->Next())
      if n ~= nil then
        put " " ..
        n->print()
        put ""
      end if
    end loop
    put repeat("-", 80) 
    put " Dump size: ", intstr(dump->Size())
    put repeat("-", 80) 
    put ""
  end printStack

  /**
   * Returns the heap opject associated with a nodes hash code.
   *
   * @param n  Node.
   * @return  Object on the heap at node's hash code.
   */
  body fcn heapGet(n : ^Node) : ^Node
  
    if n = nil then
      machineError("Node is nil.")
      result nil
    end if
    result heap->Get(n->HashCode())
  end heapGet
 
  /**
   * Pushes a node on the stack and creates a mapping of the nodes hash code
   * to itself on the heap.
   *
   * @param n  The node to be pushed on the stack and put on the heap.
   */
  proc pushPut(n : ^Node)
  
    if n = nil then
      machineError("Node is nil.")
      return
    end if
    
    stack->Push(n)
    heap->Put(n->HashCode(), n)
  end pushPut

  /**
   * Pops a node from the stack and returns the associated object on the heap.
   *
   * @return  Popped and heap redirected node.
   */
  fcn popGet() : ^Node
  
    var n : ^Node := heapGet(stack->Pop())
    
    if n = nil then
      machineError("Heap node of top stack node is nil.")
    end if
    result n
  end popGet

  /**
   * Peeks at the stack and returns the associated object on the heap.
   *
   * @return  Peeked and heap redirected node.
   */
  fcn peekGet() : ^Node
  
    var n : ^Node := heapGet(stack->Peek())
    
    if n = nil then
      machineError("Heap node of top stack node is nil.")
    end if
    result n
  end peekGet

  /**
   * Divides two integer numbers. Checks for division by 0.
   *
   * @param l  Left side parameter.
   * @param r  Right side parameter.
   * @return  l/r or 0 if division by 0.
   */ 
  fcn divide(l : ^NNum, r : ^NNum) : int
    if r->value = 0 then
      machineWarning("Division by 0. Returning 0.")
      result 0
    else
      result l->value div r->value
    end if
  end divide

  /**
   * Evaluates atomic arithmetic or comparison operations. Unsupported 
   * operations cause the machine to push an integer of 0 onto the stack.
   *
   * @param i  BinOp 'op' instruction.
   * @param l  Left operand node.
   * @param r  Right operand node.
   * @return  Result of an arithmetic or comparison operation.
   */
  proc execNum(i : ^IBinOp, l : ^NNum, r : ^NNum)
    
    var val : int := 0
    var s : ^NNum
    new s
    s->New(s)
    
    case i->op of
      label "+" : val := l->value  +  r->value
      label "-" : val := l->value  -  r->value
      label "*" : val := l->value  *  r->value
      label "/" : val := divide(l, r)
      label "<" : if l->value <  r->value then val := 1 else val := 0 end if
      label ">" : if l->value >  r->value then val := 1 else val := 0 end if
      label "<=": if l->value <= r->value then val := 1 else val := 0 end if
      label ">=": if l->value >= r->value then val := 1 else val := 0 end if
      label "==": if l->value =  r->value then val := 1 else val := 0 end if
      label "!=": if l->value ~= r->value then val := 1 else val := 0 end if
      label "&&": if (l->value ~= 0) and (r->value ~= 0) then val := 1 else val := 0 end if
      label "||": if (l->value = 0) and (r->value = 0) then val := 0 else val := 1 end if
      label     : machineError("Unsupported primitive.")
    end case
    s->value := val
    pushPut(s)
  end execNum

  /**
   * Operates on two strings. Currently supports string concatenation with the 
   * '+' operator and string comparison. Any other operator causes the machine 
   * to push the empty string on the stack and shows an error.
   *
   * @param i  BinOp 'op' instruction.
   * @param l  Left hand side parameter.
   * @param r  Right hadn side parameter.
   * @return  Concatenated string or Empty string if any other operator.
   */  
  proc execString(i : ^IBinOp, l : ^NString, r : ^NString)

    var s : ^NString
    new s
    s->New(s)
    s->value := ""

    if i->op = "+" then
      % Concatenation of thee two strings. The result is a string again.
      s->value := l->value + r->value
      pushPut(s)
    elsif (i->op =  "<") or (i->op =  ">") or (i->op = "<=") or 
          (i->op = ">=") or (i->op = "==") or (i->op = "!=") then
      % The remaining supported operations for two strings are comparison 
      % functions. The result will be an integer of 1 (true) or 0 (false).
      var val : int := 0
      var n : ^NNum
      new n
      n->New(n)   
      
      case i->op of
        label "<" : if l->value <  r->value then val := 1 else val := 0 end if
        label ">" : if l->value >  r->value then val := 1 else val := 0 end if
        label "<=": if l->value <= r->value then val := 1 else val := 0 end if
        label ">=": if l->value >= r->value then val := 1 else val := 0 end if
        label "==": if l->value =  r->value then val := 1 else val := 0 end if
        label "!=": if l->value ~= r->value then val := 1 else val := 0 end if
      end case
      
      n->value := val
      pushPut(n)
    else
      pushPut(s)
      machineError("Unsupported primitive for two strings.")
    end if
  end execString

  /**
   * Operates on left hand string and right hand integer. Currently only 
   * supports string concatenation with the '+' operator. Any other operator 
   * causes the machine to push the empty string on the stack and shows an 
   * error.
   *
   * @param i  BinOp 'op' instruction.
   * @param l  Left hand side string parameter.
   * @param r  Right hadn side integer parameter.
   * @return  Concatenated string or Empty string if any other operator.
   */  
  proc execStringNum(i : ^IBinOp, l : ^NString, r : ^NNum)
    
    var s : ^NString
    new s
    s->New(s)

    case i->op of
      label "+" : s->value := l->value + intstr(r->value) % Concatenation
      label     : s->value := ""
                  machineError("Unsupported primitive for string and integer.")
    end case
    pushPut(s)
  end execStringNum

  /**
   * Operates on left hand integer and right hand string. Currently only 
   * supports string concatenation with the '+' operator. Any other operator 
   * causes the machine to push the empty string on the stack and shows an 
   * error.
   *
   * @param i  BinOp 'op' instruction.
   * @param l  Left hand side integer parameter.
   * @param r  Right hadn side string parameter.
   * @return  Concatenated string or Empty string if any other operator.
   */  
  proc execNumString(i : ^IBinOp, l : ^NNum, r : ^NString)
    
    var s : ^NString
    new s
    s->New(s)

    case i->op of
      label "+" : s->value := intstr(l->value) + r->value % Concatenation
      label     : s->value := ""
                  machineError("Unsupported primitive for integer and string.")
    end case
    pushPut(s)
  end execNumString

  /**
   * Evaluates a binary operator. The left and right operands must be numeric
   * or string constants. Complex subexpressions must be evaluated (Eval) 
   * beforehand.
   *
   * @param i  BinOp 'op' instruction.
   */
  proc binOp(i : ^IBinOp)
  
    var l : ^Node := popGet()
    var r : ^Node := popGet()

    if (objectclass(l) = NNum) and (objectclass(r) = NNum) then
      execNum(i, l, r)
    elsif (objectclass(l) = NString) and (objectclass(r) = NString) then
      execString(i, l, r)
    elsif (objectclass(l) = NString) and (objectclass(r) = NNum)    then
      execStringNum(i, l, r)
    elsif (objectclass(l) = NNum)    and (objectclass(r) = NString) then
      execNumString(i, l, r)
    else
      machineError("Can add primitive numbers only.")
    end if
  end binOp

  /**
   * Decides which 'match' alternative to execute next.
   *
   * @param i  CaseJump [c0->e0, c1->e1, .. cn->en] instruction.
   * @param n  Constructor node.
   */
  proc caseJumpPack(i : ^ICaseJump, n : ^NPack)
    
    var c : ^LinkedList := i->alts->Get(n->name)
    
    if c = nil then
      machineError("Unhandled case for data constructor '" + n->name + "'.")
      return
    end if
    dump->Push(code)
    code := c->GetIterator()
  end caseJumpPack

  /**
   * Decides which 'match' alternative to execute next.
   *
   * @param i  CaseJump [c0->e0, c1->e1, .. cn->en] instruction.
   */
  proc caseJump(i : ^ICaseJump)
    
    var n : ^Node := peekGet() %heap->Get( stack->Peek()->HashCode() )
    
    if objectclass(n) ~= NPack then
      machineError("Expecting data constructor on stack.")
      return
    end if
    caseJumpPack(i, n)
  end caseJump

  forward proc unwind()

  /**
   * Push the pointer to the current instruction on the stack and unwind the
   * top element on the stack.
   */
  proc eval()
    dump->Push(code)
    % Unwind the top stack node.
    unwind()
  end eval

  /**
   * Conditional execution on integers. Executes the 'else' part if the top
   * stack element is 0. Every other numeric constant will be interpreted as 
   * true and triggers the execution of the 'then' subprogram.
   *
   * @param i  If instruction.
   * @param n  Number on top of the stack.
   */    
  proc condNum(i : ^IIf, n : ^NNum)
    dump->Push(code)
    case n->value of
      label 0: code := i->cElse->GetIterator()
      label  : code := i->cThen->GetIterator()
    end case
  end condNum

  /**
   * Execute an if condition. Supports integers as decission values only.
   *
   * @param i  If instruction.
   */    
  proc cond(i : ^IIf)
    
    var n : ^Node := popGet()
    
    if objectclass(n) ~= NNum then
      machineError("Conditional for 'if' expression must be numeric")
      return
    end if
    condNum(i, n)
  end cond
  
  /**
   * Combines the top two stack nodes into an application node and pushes it
   * onto the stack (creates a new heap entry as well).
   */
  proc makeAp()
    var ap : ^NAp
    new ap
    ap->New(ap)
    ap->left := stack->Pop()
    ap->right := stack->Pop()
    pushPut(ap)
  end makeAp  

  /**
   * Packs up the top n nodes into a data constructor and pushes the newly 
   * created constructor onto the stack (creates a new heap entry as well).
   *
   * @param i  Pack 'name' n instruction.
   */
  proc pack(i : ^IPack)
    
    var p : ^NPack
    new p
    p->New(p)
    
    p->name := i->name
    for c : 1 .. i->arity
      p->params->Push( popGet() )
    end for
    pushPut(p)
  end pack
  
  /**
   * Pops n elements from the stack.
   *
   * @param i  Pop n instruction.
   */
  proc pop(i : ^IPop)
    
    var n : ^Node
    
    for k : 1 .. i->size % Rename
      n := stack->Pop()
    end for
  end pop
  
  /**
   * Pushes the right side of an application node onto the stack.
   *
   * @param n  Application node.
   */
  proc pushAp(n : ^NAp)
  
    if n->right = nil then
      machineError("Right hand side of application node is nil.")
      return
    end if
    stack->Push(n->right)
  end pushAp
  
  /**
   * Pushes a pointer to the n-th element on the stack. If the n-th node is a
   * application node, pushes the right branch onto the stack.
   *
   * @param i  Push n instruction.
   */
  proc push(i : ^IPush)    
    stack->Push( heapGet(stack->Get(i->position)) )
  end push

  /**
   * Finds the global node if it exists and pushes it onto the stack.
   *
   * @param i  PushGlobal 'name' instruction.
   */
  proc pushGlobal(i : ^IPushGlobal)
  
    var gl : ^Node := heapGet( globals->Get(i->name) )
    
    if gl = nil then
      machineError("Function '" + i->name + "' not defined.")
      return
    end if
    stack->Push(gl)
  end pushGlobal

  /**
   * Pushes a new number node onto the stack (creates a new heap entry as well).
   *
   * @param i  PushInt n instruction.
   */
  proc pushInt(i : ^IPushInt)
    var num : ^NNum
    new num
    num->New(num)
    num->value := i->value
    pushPut(num)
  end pushInt

  /**
   * Pushes a string constant onto the stack (creates a new heap entry as well).
   *
   * @param i  PushString s instruction.
   */
  proc pushString(i : ^IPushString)
    var str : ^NString
    new str
    str->New(str)
    str->value := i->value
    pushPut(str)
  end pushString

  /**
   * Removes n nodes from the stack but leaves the top element unchanged.
   *
   * @param i  Slide n instruction.
   */
  proc slide(i : ^ISlide)
  
    var n : ^Node
  
    for k : 1 .. i->count
      n := stack->Remove(1)
    end for
    % Unwind the top stack node.
    unwind() 
  end slide
  
  /**
   * Moves all parameters of a data constructor onto the stack. The original
   * constructor remains as it was before. All parameter references are copied 
   * onto the stack.
   *
   * @param n  Constructor node.
   */
  proc unpackPack(n : ^NPack)
    
    var list : ^LinkedList
    new list
    
    % Reverse the parameters, so that they will be pushed onto the stack in 
    % correct order with the first parameter on top.
    var iter : ^Iterator := n->params->GetIterator()
    loop exit when not(iter->HasNext())
      list->Push( iter->Next() )
    end loop
    
    iter := list->GetIterator()
    loop exit when not(iter->HasNext())
      stack->Push( iter->Next() )
    end loop
  end unpackPack

  /**
   * Unpacks a data constructor node.
   */
  proc unpack()
    
    var n : ^Node := popGet()
    
    if objectclass(n) = NPack then
      unpackPack(n)
    else
      machineError("Expecting data constructor on stack.")
    end if
  end unpack
  
  /**
   * Unwinds a global. Sets the instruction pointer to the first operation of
   * the global's compiled function body.
   *
   * @param n  Global function node.
   */
  proc unwindGlobal(n : ^NGlobal)
  
    if n->arity > stack->Size() then
      machineError("Too few arguments for function '" + n->name +"'.")
      return
    end if
    
    if debug then
      put "Executing next: ", n->ToString()
    end if
    code := n->code->GetIterator()
  end unwindGlobal
  
  /**
   * Unwinds an application node, such that it pushes the left hand side of
   * the application on the stack and returns it.
   *
   * @param n  Application node. 
   * @return  The left hand sinde of the application node.
   */
  fcn unwindAp(n : ^NAp) : ^Node
  
    var n0 : ^Node := heapGet(n->left)
  
    stack->Push(n->right)
    result n0
  end unwindAp

  /**
   * Unwinds all application nodes until the node is either a global function,
   * an integer node or a data constructor node.
   * o If the current node is an integer or constructor node and there are still
   *   code pointers on the dump, continue execution at the next code pointer on 
   *   the dump.
   * o If the current node is a global function node, unwind the global by 
   *   executing the globals's compiled function body.  
   */
  body proc unwind()
  
    var n : ^Node := peekGet()
    
    loop exit when objectclass(n) ~= NAp
      n := unwindAp(n)
    end loop

    if objectclass(n) = NGlobal then
      unwindGlobal(n)
    else
      if dump->Size() > 0 then
        code := dump->Pop()
      end if
    end if
  end unwind

  /**
   * Redirects the n-th node pointer on the heap to the top node on the stack.
   * This function omits the creation of dedicated indirection nodes as proposed
   * in [1].
   *
   * @param i  Update n instruction.
   */
  proc update(i : ^IUpdate)
    
    var n : ^Node := stack->Pop()
    var m : ^Node := stack->Get(i->size)
    
    if (n = nil) or (m = nil) then
      machineError("Can not update nodes.")
      return
    end if
    % The hash code remains the same but the referenced object is substituted.
    heap->Put(m->HashCode(), n)
  end update

  /**
   * Takes a single step. Equivalent to processing a single instruction.
   */
  proc step()
  
    if code->HasNext() then
      ins := code->Next()
      if objectclass(ins) = IBinOp then
        binOp(ins)
      elsif objectclass(ins) = ICaseJump then
        caseJump(ins)
      elsif objectclass(ins) = IEval then
        eval()
      elsif objectclass(ins) = IIf then
        cond(ins)
      elsif objectclass(ins) = IMakeAp then
        makeAp()
      elsif objectclass(ins) = IPack then
        pack(ins)
      elsif objectclass(ins) = IPop then
        pop(ins)
      elsif objectclass(ins) = IPush then
        push(ins)
      elsif objectclass(ins) = IPushGlobal then
        pushGlobal(ins)
      elsif objectclass(ins) = IPushInt then
        pushInt(ins)
      elsif objectclass(ins) = IPushString then
        pushString(ins)
      elsif objectclass(ins) = ISlide then
        slide(ins)
      elsif objectclass(ins) = IUnpack then
        unpack()
      elsif objectclass(ins) = IUnwind then
        unwind()
      elsif objectclass(ins) = IUpdate then
        update(ins)
      else
        machineWarning("Unsupported instruction '" + ins->ToString() + "'.")
      end if
      icnt += 1
      
      if debug then
        printStack()
      end if
    end if
  end step

  /**
   * Register a new super combinator as a globally visible function. All added
   * functions will be available as long as this instance of Machine is alive.
   *
   * @param n  Global node.
   */
  proc addGlobal(n : ^NGlobal)
  
    if n ~= nil then
      globals->Put(n->name, n)
      heap->Put(n->HashCode(), n)
      if debug then
        machineInfo("Function '" + n->name + "/" + intstr(n->arity) + 
                    "' added."
        )
      end if
    end if
  end addGlobal

  /**
   * Runs a Church function until all instructions are executed and all code 
   * lists have been popped off the dump.
   *
   * @param n  Global node, that represents the function to be performed.
   * @return  Top node on the stack.
   */
  fcn run(n : ^NGlobal) : ^Node
  
    if n = nil then
      result nil
    end if
    
    if n->name ~= "main" then
      addGlobal(n)
      result nil
    end if
    
    % Reset the machine and unwind the statement node on the stack.
    icnt := 0
    pushPut(n)
    unwind()
    
    loop
      if code->HasNext() then
        step()
      else
        % If there are no code pointers on the stack and the current code has
        % been completed exit the loop.
        if dump->IsEmpty() then
          exit
        else
          code := dump->Pop()
        end if
      end if
    end loop   
   
    if debug then
      put "Steps taken: ", intstr(icnt)
    end if
    result popGet()
  end run
  
end Machine