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
 * An iterator over a list. 
 */
unit class Iterator
  inherit Object
  import Node
  export Init, HasNext, Next
  
  /** Refenrence to the list's head node. */
  var head : ^Node := nil
  
  /**
   * Initializes the iterator to start at the root element of the list.
   * 
   * @param hd  The root node, on which to start iterating on.
   */
  proc Init(hd : ^Node)
    head := hd
  end Init
  
  /**
   * Returns true if the iteration has more elements. (In other words, returns 
   * true if next would return an element.)
   *
   * @return  true if there exists a next element in the list.
   */
  fcn HasNext() : boolean
    result (head ~= nil)
  end HasNext
  
  /**
   * Returns the next element in the interation, or nil of no such element 
   * exists.
   *
   * @return  The next element in the list.
   */
  fcn Next() : ^Object
  
    if head = nil then
      result nil
    else
      var obj : ^Object
      obj := head->object
      head := head->next
      result obj
    end if
  end Next
  
  body fcn ToString() : string
    result "Iterator@" + HashCode()
  end ToString
  
end Iterator