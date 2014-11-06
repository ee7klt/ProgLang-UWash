(*

fun double x = 2*x
fun incr x= x+1
val a_tuple = (double,incr, double(incr 7))
val eighteen = (#1 a_tuple) 9


fun increment_n_times_lame (n,x) = (* silly example, this is addition (n+x) *)
   if n=0
   then x
   else 1 + increment_n_times_lame(n-1,x)

fun double_n_times_lame (n,x) = 
   if n=0
   then x
   else 2 * double_n_times_lame(n-1,x)

fun nth_tail_lame (n,xs) =
   if n=0
   then xs
   else tl (nth_tail_lame(n-1,xs)) 

*)
(*
fun n_times (f,n,x) =   (* useful higher order function *)
   if n=0
   then x
   else f(n_times (f,n-1,x))

(*     f(f(f(f(....(x)))))   *)

(*helper functions (FCF) *)

fun increment x = x+1
fun double x = x+x

(* Use FCFs in the n_times HOF *)
fun addition(n,x) = n_times(increment,n,x)
fun double_n_times(n,x) = n_times(double,n,x)
fun nth_tail(n,xs) = n_times(tl,n,xs)


val x1 = n_times(double,4,7)
val x2 = n_times(increment,4,7)
val x3 = n_times(tl,2,[4,8,16,32])

(*
fun triple x = 3*x

fun triple_n_times(n,x) = n_times(triple,n,x)
*)

(*  define helper function only where we need it i.e.  within triple_n_times*)

(*
fun triple_n_times (n,x) =
   let 
     fun triple x = 3*x
   in
      n_times(triple,n,x)
   end
*)

(*  result of let is the function defined viz triple*)

fun triple_n_times (n,x) =
     n_times( let fun triple x = 3*x in triple end, n,x)



(* anonymous *)


(*
fun triple_n_times  (n,x) =
   n_times(fun triple x = 3*x,n,x)   (*will not work because binding, not expression*)
*)


(* fun triple_n_times  (n,x) =
   n_times(fn  x => 3*x,n,x) 
*)

(* desugaring, poor style *)
val triple_n_times = fn (n,x) => n_times (fn x => 3*x,n,x)

(*

fun nth_tail(n,xs) = n_times((fn y => tl y),n,xs)
*)


(* string list -> string list *)


(* (a' -> b', a' list) -> (b' list) *)
(* returns list of values with f acted upon each element of xs *)
fun map(f,xs) =
   case xs of 
     [] => []
    | x::xs'  => (f x)::map(f,xs') 



(* filter out elements of xs for which f returns true *)
(* ('a -> bool, a' list) -> (a' list) *)
fun filter (f,xs) =
    case xs of 
      [] => []
     | x::xs' => if f x then x::filter(f,xs') else filter(f,xs') 


fun is_even v =
    (v mod 2 = 0)

(* return all even numbers from list *)
fun all_even xs = filter(is_even, xs)

(* take list of pairs, second thing has to be even *)

(* 
fn (_,v) => is_even v) 
(int*int) -> bool
*)
(* return all pairs where second in pair is even*)
(* int list -> int list *)
fun all_even_snd xs = filter(fn (_,v) => is_even v, xs)

(* double_or_triple takes in a function and returns a function *)
(* (int->bool) -> (int -> int) *)
(* f is (int -> bool) *)
fun double_or_triple f = 
   if f 7
   then fn x => 2*x
   else fn x => 3*x



(* since 7-3 is equal to 4, double_or_triple returns the function fn x => 2*x
which is bound to double. So now double is (int -> int). It takes a number and doubles it *)
val double = double_or_triple (fn x => x -3 =4)
*)
(*
datatype exp = Constant of int
|Negate of exp
|Add of exp * exp
|Multiply of exp * exp

(*given an exp, is every constant in it an even number?*)

(* (int -> bool) * exp -> bool *)
fun true_of_all_constants(f,e) =   (*does f return true for every constant? *)
   case e of 
     Constant i => f i
    | Negate e1  => true_of_all_constants(f,e1)
    | Add(e1,e2) => true_of_all_constants(f,e1) andalso true_of_all_constants(f,e2)
  | Multiply(e1,e2) => true_of_all_constants(f,e1) andalso true_of_all_constants(f,e2)

fun all_even e = true_of_all_constants ((fn x => x mod 2 = 0),e)



val x =1
fun f y = x+y 
(* defined in an environment when x was already defined. *)
(*so always adds 1 to the argument, no matter where it is called *)


val x = 2 
(* this shadows previous x, but has no effect on the 1 in the function. *)
(* x is still 1 in the function *)

val y = 3

val z = f (x+y)

(* 2+3=5 is passed to the argument, and 1 is added to it, so z maps to 6 *)


val x = 6   (*irrelavant, because already defined within the function *)

(* function returns a function *)
fun f y = 
   let val x = y+1
   in fn z => x+y+z   (*2y+1+z*)
   end


val x = 3 (*irrelevant. takes the x that was defined within f *)
(* because the anonymous was defined within f *)
val g = f 4


(* function f takes a function g *)

fun f g =
   let val x = 3  (* irrelevant *)
   in g 2
   end

val x = 4
fun h y = x+y   (* add 4 to its argument *)
val z = f h  (* 4 + 2 not 3 + 2. x is being looked up in this env, not the env within f *)
(* because h was defined here, not within f *)



fun filter (f,xs) =
    case xs of 
      [] => []
     | x::xs' => if f x then x::filter(f,xs') else filter(f,xs') 


fun greaterThanX x = fn y => y > x   (* int -> (int->bool) *)
fun noNegatives xs = filter(greaterThanX ~1, xs)  (* greaterThanX ~1 is int -> bool *)
fun allGreater (xs,n) = filter(fn x => x > n,xs)


fun allShorterThan1 (xs,s) =   (* string list * string -> string list *)
   filter (fn x =>  String.size x < (print "!"; String.size s), xs)




fun allShorterThan2 (xs,s) =
   let 
    val i = (print "!"; String.size s)
    in filter (fn x => String.size x < i, xs)
    end


fun fold (f,acc,xs)  =   (* fold left *)
   case xs of 
      [] => acc
    | x::xs' => fold(f,f(acc,x),xs')


fun f1 xs = fold ((fn(x,y) => x+y), 0, xs)   (* sum elements in list *)
fun f2 xs = fold ((fn (x,y) => x andalso y >=0), true ,xs)    (* are all list elements non-negative? *)

fun f3 (xs,lo,hi) =   (* how many values in xs are between lo and hi (inclusive)?*)
    fold (( fn (x,y) =>
       x + (if y >= lo andalso y<= hi then 1 else 0)),0,xs)

fun f4 (xs,s) = (* are all string sizes less than the size of string s?*)
   let 
     val i = String.size s
     in fold((fn(x,y) => x andalso String.size y <i ),true,xs)
     end

fun f5 (g,xs) = fold((fn(x,y) => x andalso g y), true, xs)   (* do they all pass the test g *)
(* do they produce true when passed to g *)

fun f4again (xs, s) =
    let val i = String.size s
    in f5 (fn y => String.size y < i, xs )   (* don't need to use fold again *)
    end
*)



fun compose (f,g) = fn x => f(g x)   (* composite f and g *) 
(* ('b -> 'c) * ('a -> 'b) -> ('a -> 'c) *)


(* int -> real *)
(* boring method *)
fun sqrt_of_abs i = Math.sqrt (Real.fromInt (abs i))

(* using function composition *)
fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i

 
(* map *)
val sqrt_of_abs = Math.sqrt o Real.fromInt o abs


(* |> !> *)
(*create own infix operator *)

infix !>   (* pipeline operator *)
fun x !> f = f x

fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt   


(* run f, if its the right thing, return it, but if its not the right thing, return the result of g*)
fun backup1 (f,g) = fn x => case f x of 
				NONE => g x
			     | SOME y => y
(* ('a -> 'b option) * ('a -> 'b) -> ('a -> 'b) *)

fun backup2 (f,g) = fn x => f  x handle _ => g x
(* ('a -> 'b) * ('a -> 'b) -> ('a -> 'b) *)


(* currying *)


(* old way of getting effect of multiple arguments *)
fun sorted3_tupled(x,y,z) = z >= y andalso y >= x

val t1 = sorted3_tupled (7,9,11)

(* new way: currying *)
val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

(* fun sorted3 x = fn y => fn z ... *)

val t2 = ((sorted3 7) 9) 11

fun sorted3_nicer x y z = z>=y andalso y>=x



(* fold using tuples *)
fun fold (f,acc,xs)  =   (* fold left *)
   case xs of 
      [] => acc
    | x::xs' => fold(f,f(acc,x),xs')


(* curried fold *)

fun fold f acc xs  =   (* fold left *)
   case xs of 
      [] => acc
    | x::xs' => fold f (f(acc,x)) xs' 


fun sum xs = fold (fn(x,y) => x+y) 0 xs



(* partial application *)

val is_nonnegative = sorted3 0 0

val sum = fold (fn (x,y) => x+y) 0

(* does function predicate return true for _any_ element of list xs? *)
fun exists predicate xs =
 case xs of 
   [] => false
  | x::xs' => predicate x orelse exists predicate xs' 


val hasZero = exists (fn x => x=0)   (* do any of the elements equal zero? *)

val incrementAll = List.map (fn x => x+1)   (* increment each element by 1 *)


(* tupled but we wish it were curried *)
fun range (i,j) = if i> j then [] else i :: range( i+1 ,j )

(* val countup = range 1 (*doesn't work, partial application*)  *)

(* take function expecting tuple, returns curried version *)
fun curry f = fn x => fn y => f (x,y)
fun curry f x y = f(x,y)  (* sugared version *)
fun uncurry f(x,y) = f x y
fun other_curry f x y = f y x (* switch args *)

val countup = ( curry range ) 1   (* works, now that range is curried, can use PA *)


val xs = countup 7


(* mutation *)

val x = ref 42
(*create a new location x whose contents can change, initialize to 42, *)
(*return reference to that thing *)   

val y = ref 42
(* another (separate) reference *)

val z = x
(* z refers to the same reference as x (alias) *)


val _ = x := 43
(* val _  means do it and i don't care about the result *)
(* result is not gonna be bound to any variable *)
(* first box updated to 43 *)

val w = (!y) + (!x)   (*85*)
(* cannot do x + y because they are t ref, not t *)


(* Section 3 practice problems *)
(* ('a -> 'b) -> 'a list -> 'b list *)
val c = foldr (fn (x,acc) => x::acc) [] [1,2,3]
fun fold_map f  = foldr (fn(x,acc) => f x :: acc) []
val a = fold_map (fn x => x+1)
val b = a [1,2,3] 


val d = foldr (fn (x,acc) => (if x = 2 then x::acc else acc)) [] [1,2,3]
fun fold_filter f = foldr (fn (x,acc) => (if f x then x::acc else acc)) []
val d = fold_filter (fn x => x > 1) [1,2,~9,8]


(*  ('a -> ('a * 'b) option) -> 'a -> 'b list *)
fun unfold f state = 
    case f state of
      NONE => []
     | SOME (state', x) => x::(unfold f state') 



val test = unfold (fn x => if (x > 3) then NONE else SOME (x+1,x)) 0

fun factorial n = foldl (fn (x,acc) => x*acc) 1 (unfold (fn x=> if x < 1 then NONE else SOME (x-1,x)) n)


fun unfold_map f = 
    let fun helper param = 
            case param of  
               [] => NONE
		  | x::xs => SOME (xs, f x) 
    in unfold helper
    end
