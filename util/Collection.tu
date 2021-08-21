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
 * The root interface in the collection hierarchy. A collection represents a 
 * group of objects, known as its elements. Some collections allow duplicate 
 * elements and others do not.
 * TODO: Add iterator support (for HashMap).
 */
unit class Collection
  inherit Object
  export IsEmpty, Size
  
  /** 
   * Reflects the curent number of elements in this collection. Every subclass 
   * should update this counter appropriatly.
   */
  var count : int := 0
  
  /**
   * Returns true if this collection contains no elements.
   *
   * @return  true if this collection is empty.
   */
  fcn IsEmpty() : boolean
    result (count = 0)
  end IsEmpty
  
  /**
   * Returns the number of elements in this collection.
   *
   * @return  The number of elements.
   */
  fcn Size() : int
    result count
  end Size
  
end Collection