(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
(* put your solutions for problem 2 here *)

fun all_except_option (n, ns) =
    case ns of
	[] => NONE
      | n'::ns' => if same_string(n, n')
		   then SOME ns'
		   else case all_except_option(n, ns') of
			    NONE => NONE
			  | SOME (x::xs) => SOME (x::xs)

						 
fun get_substitutions1 (subs, s) =
    case subs of
	[] => []
      | [s']::subs' => case all_except_option(s, [s']) of
			 NONE => get_substitutions1(subs', s)
		       | SOME xs => xs @ get_substitutions1(subs', s)

							   
fun get_substitutions2 (subs, s) =
    let fun helper (xs, acc) =
	    case xs of
		[] => acc
	      | [x]::xs' => case all_except_option(s, [x]) of
				NONE => helper(xs', acc)
			      | SOME ys => helper(xs', acc @ ys)
    in case helper(subs, []) of
	   [] => []
	 | x::xs => x::xs
    end
	