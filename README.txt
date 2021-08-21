********************************************************************************
*                 Church - Functional Programming Language                     *
********************************************************************************

Church is my first attempt to create a toy programming language. It is
programmed with Turing an ancient educational imperative language. Church is an
untyped functional compiler and G-Machine interpreter. I implemented most of the
ideas as presented in [1].

THE CONSOLE

The interpreter console supports the following commands:

:debug  ([+|-][l|p|c|m])*

   Enable or disable debuging information for one ore mor stages. The + enables,
   the - disables debugging. The letters l, p, c, m stand for the lexer, parser,
   compiler, machine stage respectively. For instance to enable debugging for
   the lexer and disable it for the G-Machine type: :debug +l -m.

:halt

   Halts the interpreter.

:help

   Prints the help screen, which is this tutorial.

:load <filename>

   Load a source file. The folders are seperated by </> (not  <\>). You can
   either specify absolute or relative paths and point to the parent folder
   with <../>.

INTRODUCTION

   Let us begin with some simple arithmetic. The interpreter does not require a
   final ; to terminate an expression. However, any function or data definition
   in a source file needs a final ;!

      >> 3 - 4
      -1

   The next expression might appear uncommon. Like APL, Church does not have
   operator precedences. The following expression

      >> 3 * 4 + 5
      27

   is the same as 3 * ( 4 + 5). All primitve binary operators are right
   associative and equal in precedence. The Lexer and the Parser are hand-
   crafted and lack some of the convinience of real implementations. To be sure
   about the result alway consider to add some more parentheses to aid the
   Parser. For instance:

      >> I I 3

   will be parsed as (I I) 3 which loops infinitly. To ensure correct inter-
   pretation and avoid over-saturated functions add once more some parentheses.

      >> I (I 3)
      3

STRINGS

   Strings can be concatenated and compared. There is no special treatment for
   chars, these are just strings of length 1.

      >> 'Hello, World!'
      'Hello, World!'

   Strings are enclosed in single quotes when typed directly in the console. In
   source files the same string has to be enclosed by additional double quotes
   "'Hello, World!'".

      >> 'Con' + 'catenate' + ' me.'
      'Concatenate me.'

   Concatenation can be achieved with the + operator. Concatenating a string and
   an integer returns a string e.g.

      >> 1 + '2' + 3
      '123'

   There exist overloaded operations for string comparison. Strings are ordered
   according to their ASCII code.

      >> 'a' < 'A'
      0
      >> 'a' == 'a'
      1
      >> 'aa' < 'ab'
      1

FUNCTIONS

   Defining new functions can be done with a let statement.

      >> let add a b = a + b

   This will add a new 2-parameter function add to the current session. All
   parameters of a function have to be specified, unlike other functional
   languages, where one can omit a parameter. The definition (with prefix
   addition \+)

      >> let succ = \+ 1

   for a successor function will not terminate when invoked.

LISTS

   We can define lists in 2 ways: the cumbersome one with data constructors, or
   the elegant [,] notation. First we define a list with data constructors

      >> 1::2::3::[]
      [1,2,3]

   The interpreter displays the result the elegant way. We can type it the same
   way

      >> [1,2,3]
      [1,2,3]

   and the result is the same. The Parser is a little bit easy-going and expects
   the following definition as well

      >> [,1,,2,3,,]
      [1,2,3]

   The prelude contains some important operations on lists. For instance

      >> len [1,2,3,4,5]
      5
      >> [1,2,3]@[4,5,6]
      [1,2,3,4,5,6]
      >> 3..6
      [3,4,5,6]

   Check out Prelude.church for a complete list of built-in functions.

PATTERN MATCHING

   Pattern matching is limited to one level of data constructors only.  The en-
   closed expression within match .. with, has to be evaluatable to a data con-
   structor to be applicable to pattern matching. Take a look at the sum
   function as defined in Prelude.church.

      let sum l = match l with
           []   -> 0
         | h::t -> h + sum t
      end;

   We can only inspect if it is an empty list [] or a list concatenator :: and
   access only the head h and the remaining list t. Patterns like h1::h2::t,
   where we'd like to check 2 elements at once of a list will not work (but the
   Parser will not complain ;). However one can nest match blocks.

IF INSTRUCTIONS

   To do conditional actions, Church supports if blocks. The function filter
   excludes all elements that do not satisfy a predicate p.

      let filter p l = match l with
           []   -> []
         | h::t -> if p h then h::(filter p t) else filter p t end
      end;

   As a condition if, interpretes a 0 as false and any other integer as true.
   Church does not have boolean datatypes (but one can define them), and resorts
   to integers like C. Hence p returns 0 if element h does not satisfy the
   predicate, and  any other integer else. Please note the end block to termi-
   nate the if and the match blocks.

DATA CONSTRUCTORS

   We can define custom data constructors. For instance a constructor for
   propositional logic.

      data Atom a
         | Not p
         | And p q
         | Or p q
      ;

   Since Church is untyped, we don't need to specify a name for the new data-
   type. The names of a constructor parameter are unimportant, only their
   quantity is crucial, since it determines the number of arguments the con-
   structor can take. Finally an example function, that utilizes the new data
   constructor.

      let nnf p = match p with
           Atom a  -> Atom a
         | Not q   -> match q with
                           Atom b  -> Not (Atom b)
                         | Not r   -> nnf r
                         | And r s -> nnf (Or (Not r) (Not s))
                         | Or r s  -> nnf (And (Not r) (Not s))
                      end
         | And q r -> And (nnf q) (nnf r)
         | Or q r  -> Or (nnf q) (nnf r)
      end;

   This function turns an arbitrary propositional formula into NNF.

SPECIAL SYMBOLS AS INFIX OR POSTFIX FUNCTIONS OR CONSTRUCTORS

   We can define special symbol function or data constructor identifiers to be
   either a prefix, a infix or a postfix operator. Special symbols are:

      = < > | - + * _ / ! : ? & % $ ~ # . @ ^ \

   The faculty is an example for a postfix function.

      let _! x = if x > 1 then x * (x-1)! else 1 end;

   The underscore in the definition tells the compiler that it is postfix.
   Postfix operators can only take one parameter. Infix operations can be de-
   fined analogous, e.g. function composition.

      let _._ f g x = f (g x);

   Underscores before and after the function name mark it as an infix operator.
   Infix operators can have exactly one parameter to the left and arbitrarily
   many parameters the the right.

REFERENCES

 [1] Implementing Functional Languages: a tutorial,
     Simon L Peyton Jones and David R Lester, March 23, 2000
 [2] ML for the Working Programmer, Lawrence C. Paulson, Second Edition,
     Cambridge University Press 1997, ISBN: 0 521 57050 6
 [3] Java 2 Platform SE v1.4.2 Documentation,
     <http://docs.oracle.com/javase/1.4.2/docs/api/>
 [4] The Haskell 98 Report - 8 Standard Prelude,
     <http://www.haskell.org/onlinereport/standard-prelude.html>