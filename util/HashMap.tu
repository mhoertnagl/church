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
 * Hash table based implementation of the Map interface. This implementation
 * utilizes the Open Turing IntHashMap to provide the string - int mapping and
 * provides no remove operation (Left as an exercise for the reader ;). 
 *
 * Each string is associated with a unique monotonic increasing integer, that is
 * also the index position of the persisted object in a flexible array. 
 */
unit class HashMap
  inherit Map
  export GetIndex
  
  /** Hash map unique identifier (for IntHashMap). */
  var id : int
  
  /** The base-2 size of the hash map. */
  var capacity : nat4 := 4
  
  /** Flexible array holding the real objects. */
  var map : flexible array 1 .. capacity of ^Object
  
  /**
   * Mandatory constructor. Initializes an IntHashMap instance.
   *
   * @param obj  A pointer to this object.
   */
  body proc New(obj : ^Object)
    this := obj
    id := IntHashMap.New()
  end New
  /*
  body proc Free()
  end Free
  */
  fcn GetIndex(key : string) : int
  
    var pos, r : int := 666
    
    r := IntHashMap.Get(id, key, pos)
    result pos
  end GetIndex
  
  body fcn Get(key : string) : ^Object
    
    var pos : int := GetIndex(key)
    
    if pos = 0 then
      result nil
    else
      result map(pos)
    end if
  end Get
  
  body proc Put(key : string, value : ^Object)
  
    if count = capacity then
      capacity *= 2
      new map, capacity
    end if
  
    count += 1
    IntHashMap.Put(id, key, count)
    map(count) := value
  end Put

  fcn objString(obj : ^Object, fin : boolean) : string
    
    var str : string
  
    if obj = nil then
      str := "(nil)"
    else
      str := obj->ToString()
    end if
    if not(fin) then
      str += ", "
    end if
    result str
  end objString
  
  body fcn ToString() : string
    
    var obj : ^Object
    var str : string := "["
    
    for i : 1 .. count-1
      str += objString(map(i), false)
    end for
    if count > 0 then
      str += objString(map(count), true)
    end if
    result str + "]"
  end ToString

end HashMap