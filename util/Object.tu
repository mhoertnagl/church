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
 * Class Object is the root of the class hierarchy. Every class has Object as a
 * superclass. All objects implement the methods of this class. 
 */
unit class Object
  export New, Free, Equals, HashCode, ToString
  
  /**
   * After the successful invocation of the internal constructor method, the 
   * variable 'this' points to this object.
   */
  var this : ^Object

  /**
   * Object constructor. Sets the internal variable 'this' to point to this 
   * object. Turing objects do not have an internal reference 'this' to 
   * themselves, thus the programmer needs to maintain this reference manually.
   * This procedure should be called in the following way:
   * 
   *                            obj->New(obj)
   *
   * If your code relies on the functions hashCode() or equals(Object), calling
   * the constructor procedure is vital, as these functions assume a properly
   * instantiated 'this' reference.
   *
   * @param obj  A pointer to this object.
   */
  proc New(obj : ^Object)
    this := obj
  end New
  
  /**
   * Object destructor.
   * For class Object, the destructor is empty.
   */
  proc Free()
  end Free

  /**
   * Indicates whether some other object is "equal to" this one.
   *
   * The equals method implements an equivalence relation:
   *
   * It is reflexive:  for any reference value x, x->equals(x) should return 
   *                   true.
   * It is symmetric:  for any reference values x and y, x->equals(y) should 
   *                   return true if and only if y->equals(x) returns true.
   * It is transitive: for any reference values x, y, and z, if x->equals(y) 
   *                   returns true and y->equals(z) returns true, then
   *                   x->equals(z) should return true.
   * It is consistent: for any reference values x and y, multiple invocations 
   *                   of x->equals(y) consistently return true or consistently 
   *                   return false, provided no information used in equals 
   *                   comparisons on the object is modified.
   *
   * For any non-nil reference value x, x->equals(nil) should return false. 
   *
   * The equals method for class Object implements the most discriminating 
   * possible equivalence relation on objects; that is, for any reference values 
   * x and y, this method returns true if and only if x and y refer to the same 
   * object (x=y has the value true).
   *
   * @parameter obj  The reference object with which to compare.
   * @return  true if this object is the same as the obj argument; 
   *          false otherwise.
   */
  fcn Equals(obj : ^Object) : boolean
    result (obj not= nil) and (this = obj)
  end Equals

  /**
   * Returns a hash code value for the object.
   *
   * The general contract of hashCode is:
   *
   * o Whenever it is invoked on the same object more than once during an 
   *   execution, the hashCode method must consistently return the same integer, 
   *   provided no information used in equals comparisons on the object is 
   *   modified. This integer need not remain consistent from one execution of 
   *   an application to another execution of the same application.
   * o If two objects are equal according to the equals(Object) method, then 
   *   calling the hashCode method on each of the two objects must produce the 
   *   same integer result.
   * o It is not required that if two objects are unequal according to the 
   *   equals(Object) method, then calling the hashCode method on each of the 
   *   two objects must produce distinct integer results. However, the 
   *   programmer should be aware that producing distinct integer results for 
   *   unequal objects may improve the performance of hashtables. 
   *
   * As much as is reasonably practical, the hashCode method defined by class 
   * Object does return distinct integers for distinct objects. (This is 
   * implemented by converting the address of the object into an integer)
   *
   * @return  The hash code of this object.
   */
  fcn HashCode() : string
    result natstr( cheat(nat4, this) )
  end HashCode
  
  /**
   * Returns a string representation of the object. In general, the toString 
   * method returns a string that "textually represents" this object. The result 
   * should be a concise but informative representation that is easy for a 
   * person to read. It is recommended that all subclasses override this method.
   *
   * The toString method for class Object returns a string equal to the value 
   * of: 
   *                   "Object@" + HashCode()
   *
   * @return  A string representation of this object.
   */
  fcn ToString() : string
    result "Object@" + HashCode()
  end ToString

end Object