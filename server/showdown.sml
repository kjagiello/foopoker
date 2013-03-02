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
	
	
(*
if allin then create sidepot

IpSidepot([(0, 50), (1, 50), (2, 50)], 0, false)
IpSidepot([(3, 20)], 20, false)

val a = [IpSidepot([(0, 50), (1, 50), (2, 50)], 0, false)]: ipsidepot list

val b = mkIpSidepot(a, 3, 20) = [IpSidepot([(0, 30), (1, 30), (2, 30)], 0, false), 
						 		 IpSidepot([(0, 20), (1, 20), (2, 20), (3, 20)], 20, false)]
						
val c = mkIpSidepot(b, 4, 40) = [IpSidepot([(0, 10), (1, 10), (2, 10)], 0, false), 
						 		 IpSidepot([(0, 20), (1, 20), (2, 20), (3, 20), (4, 20)], 20, true), 
								 IpSidepot([(0, 20), (1, 20), (2, 20), (4, 20)], 40, true)]

if length a > 1 then

For every call/raise:

Algoritm: 
1. Finns jag i n? 
2. Om nej => 
3. Är m >= sidopotten lägg mig i sidopotten, och gå till n+1. Om m < sidopotten, skapa en ny sidopott med m'-m och en med m. 
4. Om ja => gå till n+1

*)
datatype ipsidepot = IpSidepot of int * (int * int * int) list * int * bool;

val emptyIpSidepot = IpSidepot(0, [], 0, false);

fun mkSidepot([], _, _, _, _) = []
| mkSidepot(l as iSp::xs, id, h, m, full) =
	let 
		(*Sorts a falling list*)
		fun quickSort (nil) = nil
		|   quickSort (l') = 
			let 
				val IpSidepot(nr'', pl'', pot'', full'') = List.nth(l', length(l') div 2)
		    in
				quickSort(List.filter(fn IpSidepot(v, x, y, z) => v < nr'')(l')) @ (IpSidepot(nr'', pl'', pot'', full'') :: quickSort(List.filter(fn IpSidepot(v, x, y, z) => v > nr'')(l')))
			end
		
		(*Find player*)
		fun findPlayer(pl''', id''') =
			let
				fun findPlayer'([], _) = false 
				| findPlayer'((id''', h''', m''')::xs''', id'') =
					if id'' = id''' then
						true
					else
						findPlayer'(xs''', id'')
			in
				findPlayer'(pl''', id''')
			end
		
		(*Update numbers*)
		fun updNr(l', nr'') =
			let
				fun updNr'([], _) = [] 
				| updNr'((iSp'' as IpSidepot(nr'', pl'', pot'', full''))::xs, nr''') =
					if nr'' <= nr''' then
						iSp'' :: updNr'(xs, nr''')
					else
						IpSidepot(nr''+1, pl'', pot'', full'') :: updNr'(xs, nr''')
			in
				updNr'(l', nr'')
			end
		(*Change old sidepot*)
		fun chOldSidepot(IpSidepot(nr''', pl''', pot''', full'''), id'', h'', m'', full'''') =
			let
				fun chOldSidepot'([], id''', h''', m''') = [(id''', h''', m''')]
				| chOldSidepot'((id'''', h'''', m'''')::xs'''', id''', h''', m''') =
					if m'''' >= m''' then
						(id'''', h'''', m''')::chOldSidepot'(xs'''', id''', h''', m''')
					else
						(id'''', h'''', m'''')::chOldSidepot'(xs'''', id''', h''', m''')
			in
				IpSidepot(nr''', chOldSidepot'(pl''', id'', h'', m''), m'', full'''')
			end
			
		(*Make new sidepot*)
		fun mkNewSidepot(IpSidepot(nr''', pl''', pot''', full'''), m'', full'''') =
			let
				fun mkNewSidepot'([], m''') = []
				| mkNewSidepot'((id'''', h'''', m'''')::xs'''', m''') =
					(id'''', h'''', m''''-m''')::mkNewSidepot'(xs'''', m''')
			in
				IpSidepot(nr'''+1, mkNewSidepot'(pl''', m''), pot'''-m'', full'''')
			end
		
			(*
			Algoritm: 
			1. Finns jag i n? 
			2. Om nej => 
			3. Är m >= sidopotten lägg mig i sidopotten, och gå till n+1. Om m < sidopotten, skapa en ny sidopott med m'-m och en med m. 
			4. Om ja => gå till n+1
			*)
		
		fun mkSidepot'([], iSp' as IpSidepot(nr', pl', pot', full'), id, h, m, full) = 
		
		if findPlayer(pl', id) = true then 
			(print("1a. ");iSp'::[])
		else
			if m = pot' then
				(print("2a. ");IpSidepot(nr', (id, h, m)::pl', pot', full')::[])
			else if m > pot' then
				(print("3a. ");IpSidepot(nr', (id, h, if pot' <> 0 then pot' else m)::pl', if pot' <> 0 then pot' else m, full')::IpSidepot(nr'+1, [(id, h, m-pot')], m-pot', full)::[])		
			else
				(print("4a. ");chOldSidepot(iSp', id, h, m, full)::mkNewSidepot(iSp', m, full)::[])
		
		| mkSidepot'(l as iSp'::xs, iSp as IpSidepot(nr', pl', pot', full'), id, h, m, full) =		
		
			if findPlayer(pl', id) = true then 
				(print("1b. ");iSp::mkSidepot'(xs, iSp', id, h, m, full))
			else
				if m = pot' then
					(print("2b. ");IpSidepot(nr', (id, h, m)::pl', pot', full')::xs)
				else if m > pot' then
					(print("3b. ");IpSidepot(nr', (id, h, if pot' <> 0 then pot' else m)::pl', if pot' <> 0 then pot' else m, full')::mkSidepot'(xs, iSp', id, h, m-pot', full))		
				else
					(print("4b. ");chOldSidepot(iSp, id, h, m, full)::mkNewSidepot(iSp, m, full)::updNr(l, nr'))
		
	in
		mkSidepot'(xs, iSp, id, h, m, full)
	end;
	
val a = mkSidepot([emptyIpSidepot], 0, 9999, 1000, false);
val b = mkSidepot(a, 1, 9999, 500, false); 
val c = mkSidepot(b, 2, 9999, 200, false); 
val d = mkSidepot(c, 3, 9999, 1000, false); 
val e = mkSidepot(d, 4, 9999, 1500, false); 
val f = mkSidepot(e, 5, 9999, 1200, false); 
val g = mkSidepot(f, 6, 9999, 1500, false); 
val h = mkSidepot(g, 3, 9999, 500, false); 
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

