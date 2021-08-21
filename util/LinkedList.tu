/******************************************************************************
 * Turing Standard Library                                                    *
 ******************************************************************************
 * A standard library closely related to the Java standard API.               *
 *                                                                            *
 * REFERENCES                                                                 *
 *  [1] Java 2 Platform SE v1.4.2 Documentation                               *
 *      <http://docs.oracle.com/javase/1.4.2/docs/api/>                       *
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

/**
 * Linked list implementation of the List interface. In addition to implementing 
 * the List interface, the LinkedList class provides methods to push onto , pop 
 * from and peek at the top of the list. These operations allow linked lists to 
 * be used as a stack.
 */
unit class LinkedList
  inherit List
  import Node
  
  var head, tail : ^Node := nil
  
  body proc Add(obj : ^Object)
    
    var node : ^Node
    new node
    
    node->object := obj
    node->next := nil
    
    if head = nil then
      head := node
      tail := node
    else
      tail->next := node
      tail := node
    end if    
    count += 1
  end Add

  body fcn Get(idx : int) : ^Object
  
    var ptr : ^Node := head
    
    %assert (idx >= 0) and (idx < count)
    
    if (idx < 0) or (idx > count-1) then
      result nil
    end if

    for i : 1 .. idx
      ptr := ptr->next
    end for    
    result ptr->object
  end Get

  body fcn GetIterator() : ^Iterator
  
    var iter : ^Iterator
    new iter
    iter->New(iter)
    iter->Init(head)
    
    result iter
  end GetIterator
  
  body fcn Peek() : ^Object
  
    if head = nil then
      result nil
    else
      result head->object
    end if
  end Peek
  
  body fcn Pop() : ^Object
    
    var node : ^Node := head
    var obj : ^Object := nil
    
    if count > 0 then
      obj := head->object
      head := node->next
      free node
      count -= 1
    end if
    result obj
  end Pop
  
  body proc Push(obj : ^Object)
    
    var node : ^Node
    new node
    
    node->object := obj
    node->next := head
    head := node    
    count += 1
  end Push
  
  body fcn Remove(idx : int) : ^Object
  
    var ptr : ^Node := head
    var dpt : ^Node           % Pointer to previous element.
    var obj : ^Object
    
    %assert (idx >= 0) and (idx < count)

    if (idx < 0) or (idx > count-1) then
      result nil
    end if

    % Update head node if we delete the first element or if there is only one 
    % element in this list.
    if (count = 1) or (idx = 0) then
      head := ptr->next
    else
      % Search for the element at position 'idx'.
      for i : 1 .. idx
        dpt := ptr
        ptr := ptr->next
      end for
      dpt->next := ptr->next
    end if
    
    % Update tail reference, if we remove the last node.
    if (count = 1) then
      tail := head
    elsif idx = count-1 then
      tail := dpt
    end if
    
    obj := ptr->object
    free ptr
    count -= 1
    result obj
  end Remove
  
  body proc Reverse()
    
    
    
  end Reverse
  
  body fcn ToString() : string
    
    var obj : ^Object
    var ptr : ^Node := head
    var str := "["
    
    for i : 2 .. count
      obj := ptr->object
      if obj ~= nil then
        str := str + obj->ToString() + ", "
      end if
      ptr := ptr->next
    end for    
    if count > 0 then
      obj := ptr->object
      if obj ~= nil then
        str := str + obj->ToString()
      end if
    end if
    result str + "]"
  end ToString
  
end LinkedList