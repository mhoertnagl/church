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
 * A node in a linked list.
 */
unit class Node
  inherit Object
  export var all
  
  /** The object that will be held in a list node. */
  var object : ^Object
  
  /** The next node in a list. */
  var next : ^Node
  
end Node