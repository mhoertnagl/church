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
 * An object that maps keys to values. A map cannot contain duplicate keys; each 
 * key can map to at most one value.
 */
unit class Map
  inherit Collection
  export Get, Put
  
  /**
   * Returns the value to which this map maps the specified key. Returns nil if 
   * the map contains no mapping for this key. A return value of nil does not 
   * necessarily indicate that the map contains no mapping for the key; it's 
   * also possible that the map explicitly maps the key to nil.
   *
   * @param key  Key whose associated value is to be returned. 
   * @return  The value to which this map maps the specified key, or null if the
   *          map contains no mapping for this key.
   */
  deferred fcn Get(key : string) : ^Object
  
  /**
   * Associates the specified value with the specified key in this map. If the 
   * map previously contained a mapping for this key, the old value is replaced 
   * by the specified value.
   *
   * @param key    Key with which the specified value is to be associated.
   * @param value  Value to be associated with the specified key. 
   * @return  Previous value associated with specified key, or nil if there was 
   *          no mapping for key. A null return can also indicate that the map 
   *          previously associated null with the specified key, if the 
   *          implementation supports null values. 
   */
  deferred proc Put(key : string, value : ^Object)
  
end Map