(* Three most important type building-blocks in any language

1. Each of: A t value contains values of each t1 t2 ... tn
2. One of: A t value contains values of one of t1 t2 ... tn
3. Self reference: A t value can refer to other t values *)

(* Examples:
- Tuples build each of types
-- int * bool contains an int and a bool

- Options build one-of types
-- int option contains an int or it contains no data

- Lists use all three building blocks
-- int list contains an int and another int list or it contains no data

 *)

(* Records are like named tuples *)

val x = {bar=1+2, foo=true, baz=(false,9) };
val my_nephew = {name="Fulton", id=1};
val a_record = {second=4+1, first=3+1};
val another_pair = {2=5, 1=6};

(* The truth about tuples

- They are actually records!!!
- Tuple syntax is just a different way to write certain records
- In other words, records with field names 1, 2, ... *)

(* Why is syntactic sugar called syntactic sugar ?
- Syntactic: can describe the semantics entirely by corresponding datatype syntax
- Sugar: They make the language sweeter :) *)

(* What are datatype bindings?
- A way to make one-of types *)

datatype mytype = TwoInts of int * int
		| Str of string
                | Pizza

val a = Str "hi";
val b = Str;
val c = Pizza;
val d = TwoInts(1+2, 3+4);
val e = a;

(* What are the values we make with datatype bindings?
- Any value of type datatype_name is made from one of the constructors
- What does the value contain?
-- A "tag" for "which constructor" (e.g. TwoInts)
-- The corresponding data (e.g. (7,9)) *)

(* So how do we actually use them?
- There are two aspects to accessing a datatype value
1. Check what variant it is (what constructor made it)
2. Extract the data (if that variant has any)
3. Using case expressions!!!

- Notice how our other one-of types used functions for this:
-- null and isSome check variants (e.g. empty list or empty option)
-- hd, tl, and valOf extract data (raise exception on wrong variant)
 *)

fun f (x : mytype) =
    case x of
	Pizza => 3
      | Str s => 8
      | TwoInts(i1,i2) => i1 + i2


datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

val c1 = (Diamond, Ace)
val c2 = (Spade, Queen)

fun is_Queen_of_Spades (c : card) =
    (#1 c) = Spade andalso (#2 c) = Queen

(* The "more general" rule:
A type t1 is more general than the type t2 if you can take t1,
replace its type variables consistently, and get t2
 *) 

fun nondecreasing xs = (* int list -> bool *)
    case xs of
	[] => true
      | _::[] => true
      | head::(neck::rest) => head <= neck andalso nondecreasing (neck::rest)

datatype sgn = P | N | Z

fun multsign (x1, x2) = (* int * int -> sgn *)
    let fun sign x = if x=0 then Z else if x > 0 then P else N
    in
	case (sign x1, sign x2) of
	    (Z, _) => Z			  
	  | (_, Z) => Z
	  | (P, P) => P
	  | (N, N) => P
	  | _ => N
    end


(* What is a tail call?
informally: When there is nothing left for caller to do
formally: If the result of f(x) is the "immediate result"
for the enclosing function body, then f(x) is a tail call *)

(* 
A function is tail recursive if it's a tail call
*)
(* Polymorphic datatypes *)