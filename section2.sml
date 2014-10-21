 datatype mytype = TwoInts of int*int|Str of string|Pizza

val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInts(1+2,3+4)
val e = a


fun f x = 
   case x of 
       Pizza => 3
    | Str s => 8 
    | TwoInts(i1,i2)  => i1 + i2 
 


(*   | Pizza => 4; *)

fun g x = 
   case x of 
       Pizza => 3



datatype exp = Constant of int
	      | Negate of exp
              | Add of exp * exp
              | Multiply of exp * exp

fun eval e = 
    case e of 
     Constant i => i
     | Negate e2  => ~ (eval e2)
     | Add (e1,e2) => (eval e1) + (eval e2)
     | Multiply (e1, e2)  => (eval e1) * (eval e2)    

(* don't recurse repeatedly

fun max_constant e =
     case e of 
	Constant i => i
      | Negate e2  => max_constant e2
      | Add(e1,e2) => if max_constant e1 > max_constant e2
		      then max_constant e1
                      else max_constant e2
      | Multiply(e1,e2)  => if max_constant e1 > max_constant e2
                            then max_constant e1
                            else max_constant e2 

fun max_constant e =
     case e of 
	Constant i => i
      | Negate e2  => max_constant e2
      | Add(e1,e2) =>
	let val m1 = max_constant e1
	    val m2 = max_constant e2
        in if m1 > m2 then m1 else m2 end
      | Multiply(e1,e2)  => 
        let val m1 = max_constant e1
	    val m2 = max_constant e2
        in if m1 > m2 then m1 else m2 end




fun max_constant e =

     let fun max_of_two (e1,e2) =
	let val m1 = max_constant e1
	    val m2 = max_constant e2
        in if m1 > m2 then m1 else m2 end
in
     case e of 
	Constant i => i
      | Negate e2  => max_constant e2
      | Add(e1,e2) => max_of_two(e1,e2)

      | Multiply(e1,e2)  => max_of_two(e1,e2)
        
end


fun max_constant e =

     let fun max_of_two (e1,e2) =
	let val m1 = max_constant e1
	    val m2 = max_constant e2
        in Int.max(m1,m2) end
in
     case e of 
	Constant i => i
      | Negate e2  => max_constant e2
      | Add(e1,e2) => max_of_two(e1,e2)

      | Multiply(e1,e2)  => max_of_two(e1,e2)
        
end


fun max_constant e =

     let fun max_of_two (e1,e2) =
	Int.max(max_constant e1, max_constant e2)
in
     case e of 
	Constant i => i
      | Negate e2  => max_constant e2
      | Add(e1,e2) => max_of_two(e1,e2)

      | Multiply(e1,e2)  => max_of_two(e1,e2)
        
end

*)
fun max_constant e =

    
     case e of 
	Constant i => i
      | Negate e2  => max_constant e2
      | Add(e1,e2) => Int.max(max_constant e1, max_constant e2)

      | Multiply(e1,e2)  => Int.max(max_constant e1, max_constant e2)
        




val test_exp : exp = Add (Constant 19, Negate (Constant 40))
val nineteen = max_constant test_exp



datatype suit = Club|Diamond|Heart|Spade
datatype rank = Jack|Queen|King|Ace|Num of int
type card = suit * rank

fun isQueenofSpades (c: card) = 
   #1 c = Spade andalso #2 c = Queen

fun isQoS c =
   case c of 
    (Spade,Queen) => true
    | _ => false  



 val fiveofdiamonds : card = (Diamond,Num 5)
 val queenofspades : card = (Spade,Queen)

datatype my_int_list = Empty
			    | Cons of int*my_int_list

val x = Cons(4,Cons(23,Cons(2008,Empty)))

fun append_my_list (xs,ys) =
   case xs of 
     Empty => ys
    | Cons(x,xs') => Cons(x, append_my_list(xs',ys)) 



fun sum_list xs = 
   case xs of 
     [] => 0
    | x::xs' => x + sum_list xs'

fun inc_or_zero intoption = 
   case intoption of 
    NONE => 0
    | SOME i  =>  i+1 
   





fun full_name r = 
   case r of 
     {first=x, middle=y, last=z} =>
        x ^ " " ^ y ^ " " ^ z


datatype mytype = TwoInts of int * int
| Str of string
| Pizza


fun f x = 
   case x of 
    Pizza => 3
    | TwoInts(i1,i2) => i1-i2
    | Str s  => String.size s  


fun append (xs, ys) =
   case xs of 
     [] => ys
    | x::xs' => x :: append(xs',ys) 

fun sum_triple triple = 
   case triple of 
    (x,y,z) => x + y + z

fun sum_triple triple = 
   let val (x,y,z) = triple
   in 
    x+y+z
   end

fun sum_triple (x,y,z) =
   x+y+z


fun full_name1 {first=x,middle=y,last=z} =
   x ^ " " ^ y ^ " " ^ z


fun is_three x = 
   if x=3 then "yes" else "no"


fun same_thing (x,y) = 
   if x=y then "yes" else "no"

exception ListLengthMismatch

fun zip3 list_triple = 
   case list_triple of ([],[],[]) => []
    | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3) ::zip3(tl1,tl2,tl3)
    | _ => raise ListLengthMismatch  

(*
fun nondecreasing xs = 
  case xs of 
    [] => true
   | x::xs' => case xs' of
                    [] => true
		 | y::ys' => x <=y andalso nondecreasing xs'  

*)
fun nondecreasing xs = 
  case xs of 
    [] => true
       | _::[] => true
	       | head::(neck::rest) => head <= neck andalso nondecreasing (neck::rest)  
 



datatype sgn = P | N | Z

fun multsign (x1,x2) = 
    let fun sign x = if x=0 then Z else if  x>0 then P else N
    in
      case (sign x1, sign x2) of
        (Z,_) => Z
	| (_,Z) => Z 
       | (P,P) => P
       | (N,N) => P
       | _ => N
     (*  | (N,P) => N
       | (P,N) => N *)   
    end



fun len xs = 
   case xs of 
     [] => 0
    | _::xs' => 1 + len xs' 



fun hd xs = 
   case xs of 
    [] => raise List.Empty
    | x::_ => x 


exception MyUndesirableCondition

fun mydiv (x,y) =
   if y=0
   then raise MyUndesirableCondition
   else x div y


exception MyOtherException of int*int


fun maxlist (xs,ex) = 
   case xs of 
     [] => raise ex
    | x::[]  => x
    | x::xs' => Int.max(x,maxlist(xs',ex))  


val x = maxlist([],MyUndesirableCondition)
	       handle MyUndesirableCondition => 42

(*val y =  maxlist([],MyUndesirableCondition)*)


fun fact n = if n=0 then 1 else n*fact(n-1)
val x = fact 3


fun fact n = 
   let fun aux(n,acc) = 
      if n=0
      then acc
      else aux(n-1, acc*n)
  in
    aux(n,1)
  end

fun rev xs = 
   case xs of 
    [] => []
    |  x::xs'  => (rev xs') @ [x] 

let val x = 5
      val lst = SOME [3,6,1]
  in
      SOME (x :: valOf lst)
  end;
