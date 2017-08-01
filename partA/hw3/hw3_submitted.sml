(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(*
	Definining questions 1-6 as a variable bindings instead of function to avoid unnecessary 
	function wrapping, though they should still behave like a function when invoked with argument of type string list
*)

(* Question 1 *)
val only_capitals = List.filter (fn str => Char.isUpper(String.sub(str, 0)))

(* Question 2 *)
val longest_string1 =
	List.foldl (fn (str, longest) => if String.size(str) > String.size(longest) then str else longest) ""

(* Question 3 *)
val longest_string2 =
	List.foldl(fn (str, longest) => if String.size(str) >= String.size(longest) then str else longest) ""

(* Question 4 *)
(* 
	Whie the test cases do NOT catch this, the autograder does expect longest_string_helper to take care of invoking
	String.size() and pass them onto the curried (int * int) -> bool function, supplied through partial application
*)
fun longest_string_helper f =
	List.foldl(fn (str, longest) => if f ( String.size(str), String.size(longest) ) then str else longest) ""

val longest_string3 = longest_string_helper (fn (l1, l2) => l1 > l2)
val longest_string4 = longest_string_helper (fn (l1, l2) => l1 >= l2)

(* Question 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* Question 6 *)
val rev_string = String.implode o List.rev o String.explode

(* Question 7 *)
fun first_answer f xs =
	case xs of
			[] => raise NoAnswer
		|	x::xs => case f (x) of
							SOME v => v
						|	NONE => first_answer f xs

(* Question 8 *)
fun all_answers f xs =
	let fun all_answers_helper (acc, xs) =
			case xs of
					[] => SOME acc
				|	x::xs => case f(x) of
									SOME v => all_answers_helper (acc @ v, xs)
								|	NONE => NONE
	in
		all_answers_helper ([], xs)
	end

(* Question 9 *)
(* part a *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* part b *)
val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size(s))

(* part c *)
fun count_some_var (str, p) = g (fn _ => 0) (fn s => if s = str then 1 else 0) p

(* Question 10 *)
fun check_pat p =
	let
		(* Extract strings from all Variables in a pattern, including nested Variables, into a string list *)
		fun all_strings_in_pattern pat =
			case pat of
					Variable x => [x]
				|	TupleP ps => List.foldl (fn (i, acc) => acc @ (all_strings_in_pattern i)) [] ps
				| 	ConstructorP (_, i) => all_strings_in_pattern i
				|	_ => []

		(* True if all strings in a string list are distinct, false otherwise *)
		fun all_distinct strs =
			case strs of
					[] => true
				|	x::xs => if List.exists (fn str => str = x) xs then false else all_distinct xs

	in
		all_distinct (all_strings_in_pattern p)
	end

(* Question 11 *)
fun match pair =
	case pair of
			(_, Wildcard) => SOME []
		|	(v, Variable s) => SOME [(s, v)]
		|	(Unit, UnitP) => SOME []
		|	(Const v, ConstP p) => if v = p then SOME [] else NONE
		| 	(Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps) then all_answers match (ListPair.zip (vs, ps)) else NONE
		|	(Constructor (s1, v), ConstructorP (s2, p)) => if s1 = s2 then match(v, p) else NONE
		|	_ => NONE

(* Question 12 *)
fun first_match v ps =
	SOME (first_answer (fn p => match(v, p)) ps)
	handle NoAnswer => NONE


