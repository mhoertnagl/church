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
import Church in "ui/Church.tu"

View.Set("screen:500;82")

% Start the church interpreter console.
var church : ^Church
new church
church->New(church)
church->start()
