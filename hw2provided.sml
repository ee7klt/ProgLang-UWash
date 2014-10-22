(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)


(* ("Fred",["Cassie","Kendra","Fred","Ivan"])   Remember ["Cassie"]

("Fred",["Kendra","Fred","Ivan"])      Remember ["Cassie","Kendra"] 
("Fred",["Fred","Ivan"])               Remember ["Cassie","Kendra"]
("Fred",["Ivan"])                      Remember ["Cassie","Kendra","Ivan"]
("Fred,[])                             Remember ["Cassie","Kendra","Ivan"]

*)

fun all_except_option (nam,namList) =
   case namList of 
       [] => SOME []
      | x::rest => if same_string(x,nam)    (* ("Fred",["Fred","Ivan"])   *)  
                   then all_except_option(nam,rest)    (* ("Fred",["Ivan"]) *)
                   else case all_except_option(nam,rest) of   (* ("Fred",["Kendra","Fred","Ivan"]) *)
                        NONE =>  NONE
		      | SOME lst => SOME (x :: lst)

val test1 = all_except_option("string", ["string"]) = SOME []

(*
let val y = all_except_option(string,rest)
                     in case y of 
                        NONE => []
		      | SOME z => SOME (x::z)
	             end
*)




(* get_substitutions1: (string list list, string) -> string list  *)

fun get_substitutions1 (stringListList,s) = 
   case stringListList of 
     [] => []
    | x::xs  => case x of
                  [] => []
                 | y  => case all_except_option(s,y) of
                    SOME z => z@get_substitutions1(s,xs) 
		   |NONE =>  get_substitutions1(s,xs) 


(* val test2 = get_substitutions1([["foo"],["there"]], "foo") = [] *)

(*
fun similar_names (

*)
       

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
