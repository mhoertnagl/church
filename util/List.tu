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
 * An ordered collection (also known as a sequence). The user can access 
 * elements by their integer index (position in the list).
 *
 * The List interface provides an Iterator. An iterator allows to iterate over
 * all elements of the list starting with the element at head position.
 */
unit class List
  inherit Collection
  import Iterator
  export Add, GetIterator, Peek, Pop, Push, Remove, Get
 
  /**
   * Appends the specified element to the end of this list.
   *
   * @param obj  The object to append.
   */
  deferred proc Add(obj : ^Object) 
  
  /**
   * Returns the element at the specified position in this list.
   *
   * @param idx  The index of the element to be retrieved.
   * @return  The object at the specified index.
   */
  deferred fcn Get(idx : int) : ^Object 
  
  /**
   * Returns an iterator over the elements in this list in proper sequence.
   *
   * @return  An iterator over all elements.
   */
  deferred fcn GetIterator() : ^Iterator
  
  /**
   * Looks at the object at the top of this stack without removing it from the 
   * stack. Returns nil, if top element is nil.
   *
   * @return  The object on top of the stack.
   */
  deferred fcn Peek() : ^Object
  
  /**
   * Removes the object at the top of this stack and returns that object as the
   * value of this function.
   *
   * @return  The object on top of the stack.
   */
  deferred fcn Pop() : ^Object
  
  /**
   * Pushes an item onto the top of this stack.
   * 
   * @param obj  The object to be pushed onto the stack.
   */
  deferred proc Push(obj : ^Object)
  
  /**
   * Removes the element at the specified position in this list. Shifts any 
   * subsequent elements to the left (subtracts one from their indices). 
   * Returns the element that was removed from the list. 
   *
   * @param idx  The index of the element to be removed.
   * @return  The object that has been removed.
   */
  deferred fcn Remove(idx : int) : ^Object
  
  /**
   * Reverses the List inplace.
   */
  deferred proc Reverse()
  
end List