(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* Problem 1 *)
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* part a *)
fun all_except_option (str_to_filter, lst) = 
	let 
		fun filter_str_from_list (str, lst) =
			case lst of
					[] => []
				|	str' :: rest => if same_string(str, str') then rest 
								  else str' :: filter_str_from_list(str, rest)

			val filtered_lst = filter_str_from_list (str_to_filter, lst)
	in
		if lst = filtered_lst then NONE else SOME filtered_lst
	end

(* part b *)
fun get_substitutions1(substitutions, s) =
	case substitutions of
			[] => []
		|	x :: xs => case all_except_option(s, x) of
							NONE => get_substitutions1(xs, s)
						|	SOME lst => lst @ get_substitutions1(xs, s)

(* part c *)
fun get_substitutions2(substitutions, s) =
	let 
		fun get_substitution_helper (substitutions, acc_result) =
			case substitutions of
					[] => acc_result
				|	x :: xs =>  case all_except_option (s, x) of
									NONE => get_substitution_helper (xs, acc_result)
								|	SOME lst => get_substitution_helper (xs, acc_result @ lst)
	in
		get_substitution_helper (substitutions, [])
	end

(* part d *)
fun similar_names (substitutions, {first=x, middle=y, last=z}) =
	let
		val first_names = get_substitutions2 (substitutions, x)
		fun similar_names_helper (first_names, acc_result) =
			case first_names of
					[] => acc_result
				|	x' :: xs => similar_names_helper (xs, acc_result @ [{first=x', middle=y, last=z}])
	 in
	 	similar_names_helper (first_names, [{first=x, middle=y, last=z}])
	 end


(* Problem 2 *)
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* part a *)
fun card_color (suit, rank) = 
	case suit of
			(Spades|Clubs) => Black
		|	_ => Red

(* part b *)
fun card_value (suit, rank) =
	case rank of
			Num i => i
		|	Ace => 11
		|	_ => 10

(* part c *)
fun remove_card (cards, card, exp) =
	case cards of 
			[] => raise exp
		|	c :: cs => if c = card then cs else c :: remove_card (cs, card, exp)

(* part d *)
fun all_same_color cards = 
	case cards of
			[] => true
		|	c :: [] => true
		|	c :: c' :: cs => card_color (c) = card_color (c') andalso all_same_color (cs)


(* part e *)
fun sum_cards cards =
	let 
		fun sum_cards_helper (cards, current_sum) =
			case cards of
					[] => current_sum
				|	c :: cs => card_value (c) + sum_cards_helper(cs, current_sum)
	in
		sum_cards_helper (cards, 0)
	end

(* part f *)
fun score (held_cards, goal) = 
	let 
		val sum = sum_cards (held_cards)
		val prelim_score = if sum > goal then 3 * (sum - goal) else goal - sum
	in
		if all_same_color (held_cards) then prelim_score div 2 else prelim_score
	end

(* part g *)
fun officiate (card_list, moves, goal) =
	let 
		fun officiate_helper (card_list, held_cards, moves) =
			if card_list = [] orelse moves = [] orelse sum_cards (held_cards) > goal 
			then score (held_cards, goal) 
			else case (card_list, held_cards, moves) of
            			(c :: cs, _, (Draw) :: ms) => officiate_helper(cs, c :: held_cards, ms)
                	|	(c :: cs, _, (Discard c' ):: ms) => officiate_helper(cs, remove_card (held_cards, c, IllegalMove), ms)
                	|	(_, _, _) => score (held_cards, goal)
	in
		officiate_helper (card_list, [], moves)
	end