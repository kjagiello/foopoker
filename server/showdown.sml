datatype sidepot = Sidepot of (int * int * int) list * int
	
	
(*
	sh_emptyPot
	TYPE: 		sidepot
	VALUE: 		An empty sidepot. 
*)
val sh_emptyPot = Sidepot ([], 0);

(*	
	sh_winners l
	TYPE:		('a * int * 'b) list -> ('a * int * 'b) list
	PRE:		(none)
	POST:		
	EXAMPLE:	sh_winners([(0, 1, 500), (3, 1600, 700), (7, 5068, 700)]) = [(0, 1, 500)] 
*)
(*
	INFO:		Collects all winners (from a list of players p contributed to the pot) to a list.
	USED BY: 	sh_winners, sh_winnersMinPot(w), cashTowinners(w, l)
*)
fun sh_winners [] = [] 
| sh_winners ((x as (p, h, m))::xs) =
	let 
		fun sh_winners'([], bestPl, _) = bestPl
		| sh_winners'((x' as (p', h', m'))::xs, bestPl, bestHa) =
			if h' < bestHa then
				sh_winners'(xs, [x'], h')
			else if h' = bestHa then
				sh_winners'(xs, x'::bestPl, bestHa)
			else
				sh_winners'(xs, bestPl, bestHa)
	in
		sh_winners'(xs, [x], h)
	end;
(*
	sh_winnersMinPot l
	TYPE: 		(int * int * int) list -> sidepot
	PRE:		(none)
	POST:		A new Sidepot. 
	EXAMPLE: 	sh_winnersMinPot([(0, 1, 500), (1, 1, 1500)]) = Sidepot ([(0, 1, 500)], 500): sidepot 
*)
(*
	INFO:		Get all the sh_winners from sh_winners(l) with minimum pot into a sidepot.
	USED BY: 	sh_cashFromLosers(l, p), rmMinPotWinner(m, l)
*)
fun sh_winnersMinPot([]) = []
| sh_winnersMinPot((x as (p, h, m))::xs) =
	let
		fun sh_winnersMinPot'([], leastPl, _) = leastPl
		| sh_winnersMinPot'((x' as (p', h', m'))::xs, leastPl, least) =
			if m' < least then
				sh_winnersMinPot'(xs, [x'], m')
			else if m' = least then
				sh_winnersMinPot'(xs, x'::leastPl, least)
			else
				sh_winnersMinPot'(xs, leastPl, least)
	in
		sh_winnersMinPot'(xs, [x], m)
	end;

(*
	sh_cashFromLosers 
	TYPE:		('a * ''b * int) list * ('c * ''b * int) list -> int
	PRE:
	POST:
	EXAMPLE:
*)
fun sh_cashFromLosers([], _) = 0
| sh_cashFromLosers((p, h, m)::xs, org) = 
	let
		fun sh_cashFromLosers'([], _, _, totMoney) = totMoney
		| sh_cashFromLosers'((p', h', m')::xs', hand, money, totMoney) =
				if m' >= money then
					sh_cashFromLosers'(xs', hand, money, totMoney+money)
				else 
					sh_cashFromLosers'(xs', hand, money, totMoney+m')
	in
		sh_cashFromLosers'(org, h, m,  0)
	end;
(*
	sh_mkSidePot
	TYPE:		(int * int * int) list * int -> sidepot
	PRE:
	POST:
	EXAMPLE:
*)		
fun sh_mkSidePot([], _) = sh_emptyPot 
| sh_mkSidePot(sh_winners, tot) =
	let
		val players = length sh_winners
		val cashEach = tot div players
		
		fun sh_mkSidePot'([], _, sh_winners, tot) = Sidepot(sh_winners, tot)
		| sh_mkSidePot'((p, h, m)::xs, cashEach, sh_winners, tot) =
			sh_mkSidePot'(xs, cashEach, (p, h, cashEach)::sh_winners, tot)
	in
		sh_mkSidePot'(sh_winners, cashEach, [], tot)
	end;

(*
	sh_mkNewList
	TYPE:		('a * 'b * int) list * ('c * 'd * int) list -> ('a * 'b * int) list
	PRE:		(none)
	POST:
	EXAMPLE:
*)
fun sh_mkNewList(org, (p, h, m)::xs) = 
	let
		fun sh_mkNewList'([], _) = [] 
		| sh_mkNewList'((p', h', m')::xs', money) =
			if m' <= money then
				sh_mkNewList'(xs', money)
			else
				(p', h', m'-money)::sh_mkNewList'(xs', money)				
	in
		sh_mkNewList'(org, m)
	end;
(*
	showDown
	TYPE:		(int * int * int) list -> sidepot list 
	PRE:
	POST:
	EXAMPLE:
*)
fun showDown([]) = []
| showDown p = 
	let
		val sh_winners = sh_winners(p)							(*sh_winners -> list*)
		val minPot = sh_winnersMinPot(sh_winners)					(*Takes the winner's min pot*)
		val loserCash = sh_cashFromLosers(minPot, p)			(*Grabs losers money-minPot*)
		val sh_mkSidePot = sh_mkSidePot(sh_winners, loserCash)		(*Makes a sidepot out of the sh_winners*)
		val sh_mkNewList = sh_mkNewList(p, minPot)				(*Subtract minPot from original list, remove players with money = 0 *)
	in
		sh_mkSidePot::showDown(sh_mkNewList)						(*Cons Sidepot and repeat process.*)
	end;