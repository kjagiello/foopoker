(* 
	REPRESENTATION CONVENTION: 	Sidepot(nr, [(id, h, m)], pot, allin, full): Represents a side pot where
								nr is the number in count, id is the player's id, h is the player's hand value, 
								m is the player's money put in the pot, pot is equal to m and is the side pot's
								default value and full represents if the sidepot is opened or closed. 
								
	REPRESENTATION INVARIANT: 	id >= 0
								h > 0 
								m > 0
								nr >= 0 
								pot > 0
*)
datatype sidepot = Sidepot of int * (int * int * int) list * int * bool * bool;
(*
	emptySidepot
	TYPE: 		sidepot
	VALUE: 		An empty sidepot. 
*)
val emptySidepot = Sidepot(0, [], 0, false, false);
(*
	mkSidepot l, id, h, m, full
	TYPE: 		sidepot list * int * int * int * bool -> sidepot list
	PRE:		id > 0, h > 0, m > 0
	POST: 		A new sidepot list updated with (id, h, m) and full. 
	EXAMPLE: 	mkSidepot([Sidepot(0, [], 0, false)], 0, 1, 500, false) = 
				[Sidepot (0, [(0, 1, 500)], 500, false)]: sidepot list
*)
(*VARIANT: length l*)
fun mkSidepot([], _, _, _, _, _) = []
| mkSidepot(l as iSp::xs, id, h, m, allin, full) =
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
		| updNr((iSp' as Sidepot(nr', pl', pot', allin', full'))::xs, nr) =
			if nr' < nr then
				iSp' :: updNr(xs, nr)
			else
				Sidepot(nr'+1, pl', pot', allin', full') :: updNr(xs, nr)
		(*
			chOldSidepot s, id, h, m, full
			TYPE:		sidepot * int * int * int * bool -> sidepot
			PRE:		id > 0, h > 0, m > 0
			POST:		A new sidepot updated with with every element being changed to (_, _, m)
						and also element (id, h, m) added. 
			EXAMPLE:	chOldSidepot(Sidepot (1, [(0, 1, 200), (1, 1, 200), (2, 1, 200)], 200, false), 3, 1, 100) =
						Sidepot (1, [(3, 1, 100), (0, 1, 100), (1, 1, 100), (2, 1, 100)], 100, false): sidepot
		*)
		fun chOldSidepot(Sidepot(nr'', pl'', pot'', allin'', full''), id', h', m', allin') =
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
				Sidepot(nr'', chOldSidepot'(pl'', id', h', m'), m', allin', full'')
			end
		(*
			mkNewSidepot s, m
			TYPE:		sidepot * int -> sidepot
			PRE:		m > 0 
			POST:		A new sidepot where every element being changed to (_, _, m'-m). 
			EXAMPLE:	chOldSidepot(Sidepot (1, [(0, 1, 200), (1, 1, 200), (2, 1, 200)], 200, false), 50) = 
						Sidepot (1, [(0, 1, 50), (1, 1, 50), (2, 1, 50)], 50, false): sidepot
		*)
		fun mkNewSidepot(Sidepot(nr'', pl'', pot'', allin'', full''), m') =
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
				Sidepot(nr''+1, mkNewSidepot'(pl'', m'), pot''-m', allin'', full'')
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
		fun mkFull([]) = []
		| mkFull(Sidepot(nr'', pl'', pot'', allin'', full'')::xs) = 
			if allin'' = true then
				Sidepot(nr'', pl'', pot'', allin'', true)::mkFull(xs)
			else
				Sidepot(nr'', pl'', pot'', allin'', full'')::mkFull(xs)
				
		fun updPlayerMoney([], id', m') = []
		| updPlayerMoney((id'', h'', m'')::xs'', id', m') =
			if id'' = id' then
				(id'', h'', m')::xs''
			else
				(id'', h'', m'')::updPlayerMoney(xs'', id', m')
				
		fun getPlayerMoney([], id') = 0
		| getPlayerMoney((id'', h'', m'')::xs'', id') =
			if id'' = id' then
				m''
			else
				getPlayerMoney(xs'', id')
		(*
			mkSidepot' l, id, h, m
			TYPE:		sidepot list * int * int * int -> sidepot list
			PRE:		m > 0
			POST:		A new sidepot list updated with (id, h, m). 
			EXAMPLE:	mkSidepot'([Sidepot(0, [], 0, false)], 0, 1, 500) = 
						[Sidepot (0, [(0, 1, 500)], 500, false)]: sidepot list
		*)
		(*VARIANT: length l*)
		
		(*
		
		1. 50 allin, call, call, raise 50, call, call
		
		0, 50, allin, false
		[Sidepot(0, [(0, 9999, 50)], 50, true, false)]
		1, 50, call, false
		[Sidepot(0, [(1, 9999, 50), (0, 9999, 50)], 50, true, false)]
		2, 50, call, false
		[Sidepot(0, [(2, 9999, 50), (1, 9999, 50), (0, 9999, 50)], 50, true, false)]
		3, 100, raise, true
		[Sidepot(0, [(3, 9999, 50), (2, 9999, 50), (1, 9999, 50), (0, 9999, 50)], 50, true, true), 
		Sidepot(1, [(3, 9999, 50)], 50, false, false)]
		1, 50, call, false
		[Sidepot(0, [(3, 9999, 50), (2, 9999, 50), (1, 9999, 50), (0, 9999, 50)], 50, true, true), 
		Sidepot(1, [(1, 9999, 50), (3, 9999, 50)], 50, false, false)]
		2, 50, call, false
		[Sidepot(0, [(3, 9999, 50), (2, 9999, 50), (1, 9999, 50), (0, 9999, 50)], 50, true, true), 
		Sidepot(1, [(2, 9999, 50), (1, 9999, 50), (3, 9999, 50)], 50, false, false)]		
		
		2. 100 all in, 50 all in, raise to 200, raise to 400
		
		0, 100, allin, false
		[Sidepot(0, [(0, 9999, 100)], 100, true, false)]
		1, 50, allin, false
		[Sidepot(0, [(1, 9999, 50), (0, 9999, 50)], 50, true, false), 
		Sidepot(1, [(0, 9999, 50)], 50, true, false)]		
		2, 200, raise, false
		[Sidepot(0, [(2, 9999, 50), (1, 9999, 50), (0, 9999, 50)], 50, true, false), 
		Sidepot(1, [(2, 9999, 50), (0, 9999, 50)], 50, true, false), 
		Sidepot(2, [(2, 9999, 100)], 100, false, false) ]
		3, 400, raise, true
		[Sidepot(0, [(3, 9999, 50), (2, 9999, 50), (1, 9999, 50), (0, 9999, 50)], 50, true, true), 
		Sidepot(1, [(3, 9999, 50), (2, 9999, 50), (0, 9999, 50)], 50, true, true), 
		Sidepot(2, [(3, 9999, 300), (2, 9999, 100)], 300, false, false) ]		
		2, 200, call, true
		[Sidepot(0, [(3, 9999, 50), (2, 9999, 50), (1, 9999, 50), (0, 9999, 50)], 50, true, true), 
		Sidepot(1, [(3, 9999, 50), (2, 9999, 50), (0, 9999, 50)], 50, true, true), 
		Sidepot(2, [(3, 9999, 300), (2, 9999, 300)], 300, false, false) ]
		
		Testfall 1:
		val a = mkSidepot([emptySidepot], 0, 9999, 50, true, false);
		val a = mkSidepot(a, 1, 9999, 50, false, false);
		val a = mkSidepot(a, 2, 9999, 50, false, true);
		val a = mkSidepot(a, 1, 9999, 100, false, false);
		val a = mkSidepot(a, 2, 9999, 200, true, false);
		val a = mkSidepot(a, 1, 9999, 100, false, false);
		
		Testfall 2: 
		val a = mkSidepot([emptySidepot], 0, 9999, 1000, true, false);
		val a = mkSidepot(a, 1, 9999, 500, false, false);
		val a = mkSidepot(a, 2, 9999, 500, true, false);
		val a = mkSidepot(a, 3, 9999, 1000, true, false);
		val a = mkSidepot(a, 4, 9999, 100, true, false);
		val a = mkSidepot(a, 1, 9999, 500, false, true);
		
		Testfall 3:
		print("Pre-Flop:\n");
		val a = mkSidepot([emptySidepot], 0, 9999, 1000, false, false);
		val a = mkSidepot(a, 1, 9999, 500, true, false);
		val a = mkSidepot(a, 2, 9999, 1000, true, false);
		val a = mkSidepot(a, 3, 9999, 1500, false, false);
		val a = mkSidepot(a, 4, 9999, 1500, false, false);
		val a = mkSidepot(a, 5, 9999, 3000, true, false);
		val a = mkSidepot(a, 6, 9999, 100, true, false);
		val a = mkSidepot(a, 0, 9999, 2000, false, false);
		val a = mkSidepot(a, 3, 9999, 1500, false, false);
		val a = mkSidepot(a, 4, 9999, 1500, false, true);
		print("Flop:\n");
		val a = mkSidepot(a, 0, 9999, 1000, false, false);
		val a = mkSidepot(a, 3, 9999, 1000, false, false);
		val a = mkSidepot(a, 4, 9999, 1000, false, true);
		print("Turn:\n");
		val a = mkSidepot(a, 0, 9999, 2000, true, false);
		val a = mkSidepot(a, 3, 9999, 4000, false, false);
		val a = mkSidepot(a, 4, 9999, 4000, false, true);
		
		1. Är sidopotten öppen? Om ja, gå till 2. 
		2. Finns spelaren i potten? Om ja, gå till 3.
		3. Är newMoney > pottpengar? Om ja, gå till 4. 
		4. Är sidopotten allin? Om ja, gå till 5. 
		5. 
		
		
		*)
		
		(*Sista sidopotten*)
		fun mkSidepot'([], iSp' as Sidepot(nr', pl', pot', allin', full'), id, h, m, allin) = 
			let
				val getMoney = getPlayerMoney(pl', id)
				val newMoney = getMoney + m
				val intS = Int.toString
			in
				if full' = false then (*Sidepot opened*)
					if allin' = false then (*Sidepot isn't an allin pot*)
						if getMoney > 0 then (*Player is already in the side pot, update player's money, sidepot money and sidepot allin*)
							(print("1a. ");Sidepot(nr', updPlayerMoney(pl', id, newMoney), newMoney, allin, full')::[])
						else (*Player isn't in the pot. *)	
							if allin = false then (*Player isn't allin. Add player to pot and update sidepot money.*)
								(print("1b. ");Sidepot(nr', (id, h, m)::pl', m, allin', full')::[])
							else (*Player is allin. Check if split pot is needed.*)
								if m >= pot' then (*Money is the same or bigger as pot. Add player and change sidepot money and allin*)
									(print("1c. ");Sidepot(nr', (id, h, m)::pl', m, allin, full')::[])
								else (*Make a split*)
									(print("1d. ");chOldSidepot(iSp', id, h, m, allin)::mkNewSidepot(iSp', m)::[])
		
					else (*Sidepot is an allin pot*)
						if newMoney = pot' then (*Money is the same as pot. Change money.*)
							(print("1e. ");Sidepot(nr', if getMoney > 0 then updPlayerMoney(pl', id, newMoney) else (id, h, m)::pl', pot', allin', full')::[])
						else if newMoney > pot' then (*Money is bigger than pot. Change money and add a new pot.*)
							(print("1f. ");Sidepot(nr', if getMoney > 0 then updPlayerMoney(pl', id, pot') else (id, h, pot')::pl', pot', allin', full')::Sidepot(nr'+1,  [(id, h, m-pot')], newMoney-pot', allin, full')::[])
						else (*Money is smaller than pot. Make a split. *)
							(print("1g. ");chOldSidepot(iSp', id, h, m, allin)::mkNewSidepot(iSp', m)::[])
				else
					(print("1h. ");iSp'::Sidepot(nr'+1, [(id, h, m)], m, allin, false)::[])
			end
				
		| mkSidepot'(l as iSp'::xs, iSp as Sidepot(nr', pl', pot', allin', full'), id, h, m, allin) =	
		let
			val getMoney = getPlayerMoney(pl', id)
			val newMoney = getMoney + m
		in
			if full' = false then (*Sidepot opened*)
				if getMoney > 0 then (*Player is already in the side pot.*)
					if getMoney < pot' then (*Player's money in sidepot is less than sidepot*)
						if newMoney > pot' then (*Player's money is bigger than sidepot. Update player's money and try next sidepot.*)
							(print("2a. ");Sidepot(nr', updPlayerMoney(pl', id, pot'), pot', allin', full')::mkSidepot'(xs, iSp', id, h, m-pot', allin))
						else	(*Player's money is smaller than sidepot. Update player's money. *)
							(print("2b. ");Sidepot(nr', updPlayerMoney(pl', id, newMoney), pot', allin', full')::l)
					else (*Sidepot is full, try next sidepot*)
						(print("2c. ");iSp::mkSidepot'(xs, iSp', id, h, m, allin))
				else (*Player isn't in the side spot*)
					if m = pot' then (*Money is the same as pot. Add player to sidepot, change allin. Terminate.*)
						(print("2d. ");Sidepot(nr', (id, h, m)::pl', pot', allin, full')::l)
					else if m > pot' then (*Money is bigger than pot. Add player and try next sidepot*)
						(print("2e. ");Sidepot(nr', (id, h, pot')::pl', pot', allin, full')::mkSidepot'(xs, iSp', id, h, m-pot', allin))
					else (*Money is less than pot. Make a split.*)
						(print("2f. ");chOldSidepot(iSp, id, h, m, allin)::mkNewSidepot(iSp, m)::updNr(l, nr'))
				
			else	(*Sidepot closed*)
				(print("2g. ");iSp::mkSidepot'(xs, iSp', id, h, m, allin))
		end
	in
		if full = true then
			(print("\n("^Int.toString(id)^", "^Int.toString(h)^", "^Int.toString(m)^")\n");mkFull(mkSidepot'(xs, iSp, id, h, m, allin)))
		else
			(print("\n("^Int.toString(id)^", "^Int.toString(h)^", "^Int.toString(m)^")\n");mkSidepot'(xs, iSp, id, h, m, allin))
	end;
	
(*val a = mkSidepot(a, 2, 9999, 100, false, true);*)
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
		fun updateHands'(Sidepot(nr, pl, pot, allin, full), xs'') = 
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
				Sidepot(nr, updateHands''(pl, xs'', []), pot, allin, full)::updateHands(xs, xs'')
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
| showDown(Sidepot(nr, pl, pot, allin, full)::xs) = 
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
				fun mkSidePot'([], _, winners, tot) = Sidepot(nr, winners, tot, true, true)
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