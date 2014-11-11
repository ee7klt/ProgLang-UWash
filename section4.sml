fun f x = (* f: t1-> t2 since all functions can take only one argument, x: t1 *)
    
(* y : t3, z: t4 *)

    let val (y,z) = x in   (* t1 = t3*t4 *)  
	(abs y) + z   (* abs : int -> int, therefore t3: int, t4: int because can only add int to int *)
    end   (* therefore t1 = int*int *)
 (* so let-expression: int, so body of function : int, therefore t2 : int *)
(* f: int*int -> int *)




(* 
    sum : t1 -> t2   //one argument
    xs : t1
    x: t3
    xs': t3 list   //pattern match t1
    t1 = t3 list
    *)
fun sum xs = 
   case xs of 
       [] => 0  (* t2: int //0 might be returned *)
    | x::xs' => x + (sum xs')   (* t3: int because x:int and x is added to something. ignore that + works for real as well for now *)
(* xs : int list *)
(* f: int list -> int *)

(* make a mistake *)
fun sum xs = 
   case xs of 
       [] => 0  (* t2: int //0 might be returned *)
    | x::xs' => x + (sum x)  (* trying to call something that takes int list with an int. error. *)
