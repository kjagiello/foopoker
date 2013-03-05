(* 
	REPRESENTATION CONVENTION: 	Sidepot(nr, [(id, h, m)], pot, full): Represents a side pot where
								nr is the number in count, id is the player's id, h is the player's hand value, 
								m is the player's money put in the pot, pot is equal to m and is the side pot's
								default value and full represents if the sidepot is opened or closed. 
								
	REPRESENTATION INVARIANT: 	id >= 0
								h > 0 
								m > 0
								nr >= 0 
								pot > 0
*)
datatype sidepot = Sidepot of int * (int * int * int) list * int * bool;
(*
	emptySidepot
	TYPE: 		sidepot
	VALUE: 		An empty sidepot. 
*)
val emptySidepot = Sidepot(0, [], 0, false);
(*
	mkSidepot l, id, h, m, full
	TYPE: 		sidepot list * int * int * int * bool -> sidepot list
	PRE:		id > 0, h > 0, m > 0
	POST: 		A new sidepot list updated with (id, h, m) and full. 
	EXAMPLE: 	mkSidepot([Sidepot(0, [], 0, false)], 0, 1, 500, false) = 
				[Sidepot (0, [(0, 1, 500)], 500, false)]: sidepot list
*)
(*VARIANT: length l*)
fun mkSidepot([], _, _, _, _) = []
| mkSidepot(l as iSp::xs, id, h, m, full) =
	let 
		(*
			findPlayer pl, id
			TYPE:		(int*int*int) list * int -> bool
			PRE:		id > 0 
			POST:		True/false depending on if id is in pl or not. 
			EXAMPLE:	findPlayer([(0, 1, 500)], 0) = true: bool
		*)
		(*VARIANT: length pl*)
		fun findPlayer([], _) = false
		| findPlayer(l as (id'', h'', m'')::xs, id') = List.exists(fn (x, y, z) => x = id')(l);
		(*
			updNr l, nr
			TYPE:		sidepot list * int -> sidepot list
			PRE:		(none)
			POST:		A new sidepot list which is updated if there are insidepot numbers bigger than nr.
			EXAMPLE:	updNr([Sidepot (1, [(0, 1, 200), (1, 1, 200), (2, 1, 200)], 200, false)], 0) = 
						[Sidepot (2, [(0, 1, 200), (1, 1, 200), (2, 1, 200)], 200, false)
		*)
		(*VARIANT: length l*)
		fun updNr([], _) = [] 
		| updNr((iSp' as Sidepot(nr', pl', pot', full'))::xs, nr) =
			if nr' < nr then
				iSp' :: updNr(xs, nr)
			else
				Sidepot(nr'+1, pl', pot', full') :: updNr(xs, nr)
		(*
			chOldSidepot s, id, h, m, full
			TYPE:		sidepot * int * int * int * bool -> sidepot
			PRE:		id > 0, h > 0, m > 0
			POST:		A new sidepot updated with with every element being changed to (_, _, m)
						and also element (id, h, m) added. 
			EXAMPLE:	chOldSidepot(Sidepot (1, [(0, 1, 200), (1, 1, 200), (2, 1, 200)], 200, false), 3, 1, 100) =
						Sidepot (1, [(3, 1, 100), (0, 1, 100), (1, 1, 100), (2, 1, 100)], 100, false): sidepot
		*)
		fun chOldSidepot(Sidepot(nr'', pl'', pot'', full''), id', h', m') =
			let
				(*
					chOldSidepot'(l, id, h, m)
					TYPE:		(int * int * int) list * int * int * int -> (int * int * int) list
					PRE:		m > 0 
					POST:		An (int * int * int) list where every element is changed to (_, _, m) 
								and updated with (id, h, m). 
					EXAMPLE:	chOldSidepot([(0, 1, 500), (1, 1, 500)], 2, 1, 200) = 
								[(2, 1, 200), (0, 1, 200), (1, 1, 200)]: (int * int * int) list
				*)
				(*VARIANT: length l*)
				fun chOldSidepot'([], id', h', m') = [(id', h', m')]
				| chOldSidepot'((id'', h'', m'')::xs'', id', h', m') =
					if m'' >= m' then
						(id'', h'', m')::chOldSidepot'(xs'', id', h', m')
					else
						(id'', h'', m'')::chOldSidepot'(xs'', id', h', m')
			in
				Sidepot(nr'', chOldSidepot'(pl'', id', h', m'), m', full'')
			end
		(*
			mkNewSidepot s, m
			TYPE:		sidepot * int -> sidepot
			PRE:		m > 0 
			POST:		A new sidepot where every element being changed to (_, _, m'-m). 
			EXAMPLE:	chOldSidepot(Sidepot (1, [(0, 1, 200), (1, 1, 200), (2, 1, 200)], 200, false), 50) = 
						Sidepot (1, [(0, 1, 50), (1, 1, 50), (2, 1, 50)], 50, false): sidepot
		*)
		fun mkNewSidepot(Sidepot(nr'', pl'', pot'', full''), m') =
			let
				(*
				mkNewSidepot l m
				TYPE: 		(int * int * int) list * int -> (int * int * int) list
				PRE: 		m > 0
				POST: 		A new (int * int * int) list where every element being changed to (_, _, m'-m). 
				EXAMPLE: 	mkNewSidepot([(0, 1, 200), (1, 1, 200), (2, 1, 200)], 50) = 
							Sidepot (1, [(0, 1, 150), (1, 1, 150), (2, 1, 150)], 150, false): (int * int * int) list
				*)
				(*VARIANT: length l*)
				fun mkNewSidepot'([], m''') = []
				| mkNewSidepot'((id'', h'', m'')::xs'', m') =
					(id'', h'', m''-m')::mkNewSidepot'(xs'', m')
			in
				Sidepot(nr''+1, mkNewSidepot'(pl'', m'), pot''-m', false)
			end
		(*
			mkFull s, m
			TYPE:		sidepot list * bool -> sidepot list
			PRE:		(none)
			POST:		A new sidepot list where every sidepot is updated with m. 
			EXAMPLE:	mkFull([Sidepot (0, [(0, 1, 500), (1, 1, 500)], 500, false),
			    		Sidepot (1, [(0, 1, 500)], 500, false)], true) =
						[Sidepot (0, [(0, 1, 500), (1, 1, 500)], 500, true),
				    	Sidepot (1, [(0, 1, 500)], 500, false)]: sidepot list
		*)		
		(*VARIANT: length s*)
		fun mkFull([], _) = []
		| mkFull(Sidepot(nr'', pl'', pot'', full'')::xs, full') = 
			if xs <> [] then
				Sidepot(nr'', pl'', pot'', full')::mkFull(xs, full')
			else
				Sidepot(nr'', pl'', pot'', false)::mkFull(xs, full')
		(*
			mkSidepot' l, id, h, m
			TYPE:		sidepot list * int * int * int -> sidepot list
			PRE:		m > 0
			POST:		A new sidepot list updated with (id, h, m). 
			EXAMPLE:	mkSidepot'([Sidepot(0, [], 0, false)], 0, 1, 500) = 
						[Sidepot (0, [(0, 1, 500)], 500, false)]: sidepot list
		*)
		(*VARIANT: length l*)
		fun mkSidepot'([], iSp' as Sidepot(nr', pl', pot', full'), id, h, m) = 
			if full' = false then
				if findPlayer(pl', id) = true then 
					(print("1a. ");Sidepot(nr', pl', pot', full')::[])
				else
					if m = pot' then
						(print("2a. ");Sidepot(nr', (id, h, m)::pl', pot', full')::[])
					else if m > pot' then
						if pot' <> 0 then
							(print("3a1. ");Sidepot(nr', (id, h, pot')::pl', pot', full')::Sidepot(nr'+1, [(id, h, m-pot')], m-pot', false)::[])		
						else
							(print("3a2. ");Sidepot(nr', (id, h, m)::pl', m, false)::[])
					else
						(print("4a. ");chOldSidepot(iSp', id, h, m)::mkNewSidepot(iSp', m)::[])
			else
				(print("5a. ");iSp'::[])
				
		| mkSidepot'(l as iSp'::xs, iSp as Sidepot(nr', pl', pot', full'), id, h, m) =		
			if full' = false then
				if findPlayer(pl', id) = true then 
					(print("1b. ");Sidepot(nr', pl', pot', full')::mkSidepot'(xs, iSp', id, h, m))
				else
					if m = pot' then
						(print("2b. ");Sidepot(nr', (id, h, m)::pl', pot', full')::xs)
					else if m > pot' then
						(print("3b. ");Sidepot(nr', (id, h, if pot' <> 0 then pot' else m)::pl', if pot' <> 0 then pot' else m, full')::mkSidepot'(xs, iSp', id, h, m-pot'))		
					else
						(print("4b. ");chOldSidepot(iSp, id, h, m)::mkNewSidepot(iSp, m)::updNr(l, nr'))
			else
				(print("5b. ");Sidepot(nr', pl', pot', full')::mkSidepot'(xs, iSp', id, h, m))
		
	in
		if full = true then
			mkFull(mkSidepot'(xs, iSp, id, h, m), full)
		else
			mkSidepot'(xs, iSp, id, h, m)
	end;
(*
	updateHands l m
	TYPE: 		sidepot list * (int * int) list -> sidepot list
	PRE: 		(none)
	POST: 		A new sidepot list with l updated with the elements in m. 
	EXAMPLE: 	updateHands([Sidepot (0, [(0, 1, 500)], 500, false)], [(0, 200)]) =
				[Sidepot (0, [(0, 200, 500)], 500, false)]: sidepot list
*)
(*VARIANT: length l*)
fun updateHands([], _) = []
| updateHands(l, []) = l
| updateHands(x::xs, l') = 
	let 
		(*
			updateHands'(i, l)
			TYPE: 		sidepot * (int * int) list -> sidepot 
			PRE:		(none)
			POST:		A new sidepot with i updated with the elements in l. 
			EXAMPLE: 	updateHands'(Sidepot (0, [(0, 1, 500)], 500, false), [(0, 200)]) =
						Sidepot (0, [(0, 200, 500)], 500, false): sidepot
		*)
		fun updateHands'(Sidepot(nr, pl, pot, full), xs'') = 
			let
				(*
				updateHands'' (pl, h, n)
				TYPE: 		(int * int * int) list * (int * int) list  * (int * int * int) list -> (int * int * int) list
				PRE: 		(none)
				POST: 		A new (int * int * int) list n where pl is updated with h. 
				EXAMPLE: 	updateHands''([(0, 9999, 500), (1, 9999, 500)], [(0, 1), (1, 5)], []) =
							[(0, 1, 500), (1, 5, 500)]: (int * int * int) list
				*)
				(*VARIANT: length pl*)
				fun updateHands''(_, [], _) = []
				| updateHands''([], (id'', h'')::xs''', new) = 
					if xs''' <> [] then
						updateHands''(new, xs''', [])
					else
						new
				| updateHands''((id''', h''', m''')::xs'''', l'' as (id'', h'')::xs''', new) = 
					if id''' = id'' then
						updateHands''(xs'''', l'', (id''', h'', m''')::new)
					else
						updateHands''(xs'''', l'', (id''', h''', m''')::new)
			in
				Sidepot(nr, updateHands''(pl, xs'', []), pot, full)::updateHands(xs, xs'')
			end
	in
		updateHands'(x, l')
	end;
(*
	showDown l 
	TYPE: 		sidepot list -> sidepot list
	PRE:		(none)
	POST:		l as a new sidepot list. 
	EXAMPLE: 	showDown([Sidepot (0, [(2, 1200, 200), (1, 5, 200), (0, 1, 200)], 200, false),
	    		Sidepot (1, [(1, 5, 300), (0, 1, 300)], 300, false),
	    		Sidepot (2, [(0, 1, 500)], 500, false)]) =
				[Sidepot (0, [(0, 1, 600)], 600, true),
				Sidepot (1, [(0, 1, 600)], 600, true),
				Sidepot (2, [(0, 1, 500)], 500, true)]: sidepot list
*)	
(*VARIANT: length l*)
fun showDown([]) = []
| showDown(Sidepot(nr, pl, pot, full)::xs) = 
	let
		(*	
			winners l
			TYPE:		('a * int * 'b) list -> ('a * int * 'b) list
			PRE:		(none)
			POST:		l as a new ('a * int * 'b) list. 
			EXAMPLE:	winners([(0, 1, 500), (3, 1600, 700), (7, 5068, 700)]) = [(0, 1, 500)] 
		*)
		(*VARIANT: length l*)
		fun winners [] = [] 
		| winners ((x as (p, h, m))::xs) =
			let 
				(*
					winners' l, m, h
					TYPE: 		('a * int * 'b) list * ('a * int * 'b) list * int -> ('a * int * 'b) list
					PRE: 		h > 0
					POST:		m as a new ('a * int * 'b) list from l with the least common h. 
					EXAMPLE: 	winners'([(3, 1600, 700), (7, 5068, 700)], [(0, 1, 500), 1]) =
								[(0, 1, 500)] 
				*)
				(*VARIANT: length l*)
				fun winners'([], bestPl, _) = bestPl
				| winners'((x' as (p', h', m'))::xs, bestPl, bestHa) =
					if h' < bestHa then
						winners'(xs, [x'], h')
					else if h' = bestHa then
						winners'(xs, x'::bestPl, bestHa)
					else
						winners'(xs, bestPl, bestHa)
			in
				winners'(xs, [x], h)
			end
		
		(*
			cashFromPlayers l
			TYPE:		('a * 'b * int) list -> int
			PRE:		(none)
			POST:		An int. 
			EXAMPLE:	cashFromPlayers([(0, 1, 500), (3, 1600, 700), (7, 5068, 700)]) = 1900: int
		*)
		(*VARIANT: lenght l*)
		fun cashFromPlayers([]) = 0
		| cashFromPlayers(l as (p, h, m)::xs') = foldr (fn (x,y) => m+y) 0 l
		(*
			mkSidePot nr, w, t
			TYPE:		int * (int * int * 'a) list * int -> sidepot
			PRE:		t >= 0
			POST:		A sidepot from nr, w and t. 
			EXAMPLE:	mkSidepot(0, [(0, 1, 500), (1, 1, 500)], 1500) =
						Sidepot(0, [(0, 1, 750), (1, 1, 750)], 1500, true)
		*)		
		fun mkSidePot(nr, [], _) = emptySidepot 
		| mkSidePot(nr, winners, tot) =
			let
				val players = length winners
				val cashEach = tot div players
				
				(*
					mkSidePot'(l, c, w, t)
					TYPE: 		(int * int * 'a) list * int * (int * int * int) list * int -> sidepot
					PRE: 		c > 0, t > 0
					POST: 		A sidepot where the sidepot's list is w and the sidepot's pot is t. 
					EXAMPLE: 	mkSidepot'([(0, 1, 500), (1, 1, 500)], 200, [], 1400) =
								Sidepot(0, [(0, 1, 700), (1, 1, 700)], 1400, true): sidepot
				*)
				(*VARIANT: length l*)
				fun mkSidePot'([], _, winners, tot) = Sidepot(nr, winners, tot, true)
				| mkSidePot'((p, h, m)::xs, cashEach, winners, tot) =
					mkSidePot'(xs, cashEach, (p, h, cashEach)::winners, tot)
			in
				mkSidePot'(winners, cashEach, [], tot)
			end
		
		val winners = winners(pl)
		val total = cashFromPlayers(pl)
		val mkSidepot = mkSidePot(nr, winners, total)
	in
		mkSidepot::showDown(xs)
	end; 