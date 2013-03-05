(*
	random()
	TYPE: 		unit -> int
	PRE:		(none)
	POST:		A random number.
	EXAMPLE: 	random() = 121191
*)
(*
	INFO:		Makes a "random" number depending on nanoseconds. 
*)
local
    fun rmod x y = x - y * Real.realFloor (x / y);
    val a = Real.fromInt(Time.toNanoseconds(Time.now()) mod 168351);
    val m = 2147483647.0
    val random_seed = ref 1.0;
in
    fun random () =
    let
        val r = rmod (a * ! random_seed) m
    in
        random_seed := r;
        Real.floor r
    end
end;
(*
	sh_removeElement l, e
	TYPE:		''a list * ''a -> ''a list
	PRE:		(none)
	POST:		An ''a list withouth the element e.
	EXAMPLE:	sh_removeElement([5,4,3,2], 5) = [4,3,2]
*)
(*
	INFO: 		Removes an element from a list. 
*)
fun sh_removeElement ([], _) = []
  | sh_removeElement (first::rest, x) =
    if first = x then
        rest
    else
        first::sh_removeElement (rest, x);
(*
	shuffle l
	TYPE:		''a list -> ''a list
	PRE:		(none)
	POST:		l as an ''a list with random order of elements.
	EXAMPLE: 	shuffle([1,2,3,4,5]) = [2,5,4,1,3]: int list
*)
(*
	INFO:		Shuffles the elements in a list into a new list. 
*)
fun shuffle [] = []
  | shuffle l =
    let
        val index = random () mod (length l)
        val element = List.nth (l, index)
        val l = sh_removeElement (l, element)
    in
        element::shuffle l
    end;
