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
	USED BY: 	sh_cashFromPlayers(l, p), rmMinPotWinner(m, l)
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
	sh_cashFromPlayers 
	TYPE:		('a * ''b * int) list * ('c * ''b * int) list -> int
	PRE:
	POST:
	EXAMPLE:
*)
fun sh_cashFromPlayers([], _) = 0
| sh_cashFromPlayers((p, h, m)::xs, org) = 
	let
		fun sh_cashFromPlayers'([], _, _, totMoney) = totMoney
		| sh_cashFromPlayers'((p', h', m')::xs', hand, money, totMoney) =
			if m' >= money then
				sh_cashFromPlayers'(xs', hand, money, totMoney+money)
			else 
				sh_cashFromPlayers'(xs', hand, money, totMoney+m')
	in
		sh_cashFromPlayers'(org, h, m,  0)
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
		val sh_winners = sh_winners(p)								(*sh_winners -> list*)
		val minPot = sh_winnersMinPot(sh_winners)					(*Takes the winner's min pot*)
		val loserCash = sh_cashFromPlayers(minPot, p)				(*Grabs losers money-minPot*)
		val sh_mkSidePot = sh_mkSidePot(sh_winners, loserCash)		(*Makes a sidepot out of the sh_winners*)
		val sh_mkNewList = sh_mkNewList(p, minPot)					(*Subtract minPot from original list, remove players with money = 0 *)
	in
		sh_mkSidePot::showDown(sh_mkNewList)						(*Cons Sidepot and repeat process.*)
	end;
	
	

datatype ipsidepot = IpSidepot of int * (int * int * int) list * int * bool;

val emptyIpSidepot = IpSidepot(0, [], 0, false);

(*
	mkSidepot l, id, h, m, full
	TYPE: 		ipsidepot list * int * int * int * bool -> ipsidepot list
	PRE:		m >= 0
	POST: 		
	EXAMPLE: 	
*)
fun mkSidepot([], _, _, _, _) = []
| mkSidepot(l as iSp::xs, id, h, m, full) =
	let 
		(*
			findPlayer pl, id
			TYPE:		(int*int*int) list * int -> bool
			PRE:
			POST:
			EXAMPLE:
		*)
		fun findPlayer(pl, id') =
			let
				fun findPlayer'([], _) = false 
				| findPlayer'((id'', h'', m'')::xs, id') =
					if id'' = id' then
						true
					else
						findPlayer'(xs, id')
			in
				findPlayer'(pl, id')
			end
		(*
			updNr l, nr
			TYPE:		ipsidepot list * int -> ipsidepot list
			PRE:
			POST:
			EXAMPLE:
		*)
		fun updNr(l', nr) =
			let
				fun updNr'([], _) = [] 
				| updNr'((iSp' as IpSidepot(nr', pl', pot', full'))::xs, nr) =
					if nr' <= nr' then
						iSp' :: updNr'(xs, nr)
					else
						IpSidepot(nr'+1, pl', pot', full') :: updNr'(xs, nr)
			in
				updNr'(l', nr)
			end
		
		(*
			chOldSidepot s, id, h, m, full
			TYPE:		ipsidepot * int * int * int * bool -> ipsidepot
			PRE:
			POST:
			EXAMPLE:
		*)
		fun chOldSidepot(IpSidepot(nr'', pl'', pot'', full''), id', h', m', full') =
			let
				fun chOldSidepot'([], id', h', m') = [(id', h', m')]
				| chOldSidepot'((id'', h'', m'')::xs'', id', h', m') =
					if m'' >= m' then
						(id'', h'', m')::chOldSidepot'(xs'', id', h', m')
					else
						(id'', h'', m'')::chOldSidepot'(xs'', id', h', m')
			in
				IpSidepot(nr'', chOldSidepot'(pl'', id', h', m'), m', full')
			end
		(*
			mkNewSidepot s, m
			TYPE:		ipsidepot * -> ipsidepot
			PRE:
			POST:
			EXAMPLE:
		*)
		fun mkNewSidepot(IpSidepot(nr'', pl'', pot'', full''), m') =
			let
				fun mkNewSidepot'([], m''') = []
				| mkNewSidepot'((id'', h'', m'')::xs'', m') =
					(id'', h'', m''-m')::mkNewSidepot'(xs'', m')
			in
				IpSidepot(nr''+1, mkNewSidepot'(pl'', m'), pot''-m', false)
			end
		(*
			mkFull s, m
			TYPE:		ipsidepot list * bool -> ipsidepot list
			PRE:
			POST:
			EXAMPLE:
		*)		
		fun mkFull([], _) = []
		| mkFull(IpSidepot(nr'', pl'', pot'', full'')::xs, full') = 
			if xs <> [] then
				IpSidepot(nr'', pl'', pot'', full')::mkFull(xs, full')
			else
				IpSidepot(nr'', pl'', pot'', false)::mkFull(xs, full')
		(*
			mkSidepot' l, id, h, m
			TYPE:		ipsidepot list * int * int * int -> ipsidepot list
			PRE:		m > 0
			POST:
			EXAMPLE:
		*)
		(*
		Algoritm: 
		1. Finns jag i n? 
		2. Om nej => 
		3. Är m >= sidopotten lägg mig i sidopotten, och gå till n+1. Om m < sidopotten, skapa en ny sidopott med m'-m och en med m. 
		4. Om ja => gå till n+1
		*)
		
		fun mkSidepot'([], iSp' as IpSidepot(nr', pl', pot', full'), id, h, m) = 
			if full' = false then
				if findPlayer(pl', id) = true then 
					(print("1a. ");IpSidepot(nr', pl', pot', full')::[])
				else
					if m = pot' then
						(print("2a. ");IpSidepot(nr', (id, h, m)::pl', pot', full')::[])
					else if m > pot' then
						if pot' <> 0 then
							(print("3a1. ");IpSidepot(nr', (id, h, pot')::pl', pot', full')::IpSidepot(nr'+1, [(id, h, m-pot')], m-pot', false)::[])		
						else
							(print("3a2. ");IpSidepot(nr', (id, h, m)::pl', m, false)::[])
					else
						(print("4a. ");chOldSidepot(iSp', id, h, m, full')::mkNewSidepot(iSp', m)::[])
			else
				(print("5a. ");iSp'::[])
				
		| mkSidepot'(l as iSp'::xs, iSp as IpSidepot(nr', pl', pot', full'), id, h, m) =		
			if full' = false then
				if findPlayer(pl', id) = true then 
					(print("1b. ");IpSidepot(nr', pl', pot', full')::mkSidepot'(xs, iSp', id, h, m))
				else
					if m = pot' then
						(print("2b. ");IpSidepot(nr', (id, h, m)::pl', pot', full')::xs)
					else if m > pot' then
						(print("3b. ");IpSidepot(nr', (id, h, if pot' <> 0 then pot' else m)::pl', if pot' <> 0 then pot' else m, full')::mkSidepot'(xs, iSp', id, h, m-pot'))		
					else
						(print("4b. ");chOldSidepot(iSp, id, h, m, full')::mkNewSidepot(iSp, m)::updNr(l, nr'))
			else
				(print("5b. ");IpSidepot(nr', pl', pot', full')::mkSidepot'(xs, iSp', id, h, m))
		
	in
		if full = true then
			mkFull(mkSidepot'(xs, iSp, id, h, m), full)
		else
			mkSidepot'(xs, iSp, id, h, m)
	end;
	
val a = mkSidepot([emptyIpSidepot], 0, 9999, 1000, true);
val b = mkSidepot(a, 1, 9999, 500, true); 
val c = mkSidepot(b, 2, 9999, 200, false); 
val d = mkSidepot(c, 3, 9999, 1000, false); 
val e = mkSidepot(d, 4, 9999, 1500, false); 
val f = mkSidepot(e, 5, 9999, 1200, false); 
val g = mkSidepot(f, 6, 9999, 1500, false); 
val h = mkSidepot(g, 3, 9999, 500, true); 
(*

0 1000 allin 
[IpSidepot ([(0, 9999, 1000)], 0, false)]
1 500 allin 
[IpSidepot ([(1, 9999, 500), (0, 9999, 500)], 0, false), 
IpSidepot ([(0, 9999, 500)], 500, false)]
2 200 allin 
[IpSidepot ([(2, 9999, 200), (1, 9999, 200), (0, 9999, 200)], 0, false), 
IpSidepot ([(1, 9999, 300), (0, 9999, 300)], 300, false),
IpSidepot ([(0, 9999, 500)], 500, false)]
3 1000 call
[IpSidepot ([(3, 9999, 200), (2, 9999, 200), (1, 9999, 200), (0, 9999, 200)], 0, false), 
IpSidepot ([(3, 9999, 300), (1, 9999, 300), (0, 9999, 300)], 300, false),
IpSidepot ([(3, 9999, 500), (0, 9999, 500)], 0, false)]
4 1500 all in 
[IpSidepot ([(4, 9999, 200), (3, 9999, 200), (2, 9999, 200), (1, 9999, 200), (0, 9999, 200)], 0, false), 
IpSidepot ([(4, 9999, 300), (3, 9999, 300), (1, 9999, 300), (0, 9999, 300)], 300, false),
IpSidepot ([(4, 9999, 500), (3, 9999, 500), (0, 9999, 500)], 500, false), 
IpSidepot ([(4, 9999, 500)], 500, false)]
5 1200 all in 
[IpSidepot ([(5, 9999, 200), (4, 9999, 200), (3, 9999, 200), (2, 9999, 200), (1, 9999, 200), (0, 9999, 200)], 0, false), 
IpSidepot ([(5, 9999, 300), (4, 9999, 300), (3, 9999, 300), (1, 9999, 300), (0, 9999, 300)], 300, false),
IpSidepot ([5, 9999, 500), (4, 9999, 500), (3, 9999, 500), (0, 9999, 500)], 500, false), 
IpSidepot ([5, 9999, 200), (4, 9999, 200)], 500, false)
IpSidepot ([(4, 9999, 300)], 500, false)]
6 1500 call
[IpSidepot ([(6, 9999, 200), (5, 9999, 200), (4, 9999, 200), (3, 9999, 200), (2, 9999, 200), (1, 9999, 200), (0, 9999, 200)], 0, false), 
IpSidepot ([(6, 9999, 300), (5, 9999, 300), (4, 9999, 300), (3, 9999, 300), (1, 9999, 300), (0, 9999, 300)], 300, false),
IpSidepot ([(6, 9999, 500), (5, 9999, 500), (4, 9999, 500), (3, 9999, 500), (0, 9999, 500)], 500, false), 
IpSidepot ([(6, 9999, 200), (5, 9999, 200), (4, 9999, 200)], 200, false),
IpSidepot ([(6, 9999, 300), (4, 9999, 300)], 300, false)]
3 500 call 
[IpSidepot ([(6, 9999, 200), (5, 9999, 200), (4, 9999, 200), (3, 9999, 200), (2, 9999, 200), (1, 9999, 200), (0, 9999, 200)], 0, false), 
IpSidepot ([(6, 9999, 300), (5, 9999, 300), (4, 9999, 300), (3, 9999, 300), (1, 9999, 300), (0, 9999, 300)], 300, false),
IpSidepot ([(6, 9999, 500), (5, 9999, 500), (4, 9999, 500), (3, 9999, 500), (0, 9999, 500)], 500, false), 
IpSidepot ([(3, 9999, 200), (6, 9999, 200), (5, 9999, 200), (4, 9999, 200)], 200, false),
IpSidepot ([(3, 9999, 300), (6, 9999, 300), (4, 9999, 300)], 300, false)]

En insats som ligger under en tidigare sidepott kommer bara att påverka den sidopotten, inga andra. 
Den kommer däremot att splitta den tidigare sidopotten. 

funktioner: 
getLeastMain
findPlayer

Algoritm: 
1. Finns jag i n? 
2. Om nej => 
3. Är m >= sidopotten lägg mig i sidopotten, och gå till n+1. Om m < sidopotten, skapa en ny sidopott med m'-m och en med m. 
4. Om ja => gå till n+1 

*)

