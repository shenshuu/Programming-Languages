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
			  | SOME tail => SOME (n'::tail)

						 
fun get_substitutions1 (subs, s) =
    case subs of
	[] => []
      | s'::subs' => case all_except_option(s, s') of
			 NONE => []
		       | SOME xs => xs @ get_substitutions1(subs', s)

							   
fun get_substitutions2 (subs, s) =
    let fun helper (xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => case all_except_option(s, x) of
				NONE => []
			      | SOME ys => helper(xs', ys @ acc)
    in helper(subs, [])
    end

fun similar_names (xs, {first=x, middle=y, last=z}) =
    let fun helper ys =
	    case ys of
		[] => [{first=x, middle=y, last=z}]
	      | y'::ys' => {first=y', middle=y, last=z}::helper(ys')
    in helper(get_substitutions2(xs, x))
    end

fun card_color c =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

fun card_value c =
    case c of
	(_, Ace) => 11
      | (_, Num n) => n
      | _ => 10

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | c'::cs' => if c'=c
		   then cs'
		   else remove_card(cs', c, e)

fun all_same_color cs =
    case cs of
	[] => true
      | c1::[] => true
      | c1::c2::cs' => c1=c2 andalso all_same_color cs'
						    
fun sum_cards cs =
    let fun helper (xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => helper(xs', card_value(x) + acc)
    in helper(cs, 0)
    end
	
fun score (cs, goal) =
    let val sum = sum_cards(cs)
    in
	case (all_same_color(cs), sum > goal) of
	    (true, true) => (3 * (sum - goal)) div 2
	  | (true, false) => (goal - sum) div 2
	  | (false, true) => 3 * (sum - goal)
	  | (false, false) => goal - sum 
    end

fun officiate (cs, ms, goal) =
    let fun helper (held, cs, ms) =
	    if sum_cards(held) > goal
	    then score(held, goal)
	    else case (cs, ms) of		
		([], _) => score(held, goal)
	      | (_, []) => score(held, goal)
	      | (c::cs', Discard(c')::ms') =>
		helper(remove_card(held, c', IllegalMove), c::cs', ms')
	      | (c::cs', Draw::ms') => helper(c::held, cs', ms')
    in
	helper([], cs, ms)
    end