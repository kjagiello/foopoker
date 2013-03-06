(* 
	REPRESENTATION CONVENTION: 	Player(id, h, m): Represents a poker player where the id is a unique 
								id of the player, h is the player's hand value and m is the money
								of the player. 
								
	REPRESENTATION INVARIANT: 	id >= 0
								h > 0
								m >= 0
*)
datatype player = Player of (int * int * int);

(* 
	REPRESENTATION CONVENTION: 	Besthand(id', h'): id' is the id of the player
								and h' is the best h of the player. 
								
	REPRESENTATION INVARIANT: 	id' >= 0
								h' > 0
*)
datatype besthand = Besthand of (int * int);
(* 
	REPRESENTATION CONVENTION: 	Sidepot(nr, pl, pot, allin, full): Represents a side pot where
								nr is the number in count, pl is a player list, pot is the default
								value of the sidepot, allin is if the sidepot is created by a player 
								who is all in and full represents if the sidepot is opened or closed. 
								
	REPRESENTATION INVARIANT: 	nr >= 0
								pot > 0
*)
datatype sidepot = Sidepot of int * player list * int * bool * bool;
(* 
	REPRESENTATION CONVENTION: 	Sumpot(nr', sum): nr' is the nr of the sidepot, sum
								is the sum of all m of all players in the player list of a sidepot. 
								
	REPRESENTATION INVARIANT: 	nr' >= 0 
								sum >= 0
*)
datatype sumpot = Sumpot of (int * int);

(*
	sh_emptyPlayer
	TYPE: 		player
	VALUE: 		An empty player. 
*)
val sh_emptyPlayer = Player (0, 0, 0);

(*
	sh_emptyBesthand
	TYPE: 		besthand
	VALUE: 		An empty besthand. 
*)
val sh_emptyBesthand = Besthand (0, 0);

(*
	sh_emptyBesthand
	TYPE: 		sidepot
	VALUE: 		An empty sidepot. 
*)
val sh_emptySidepot = Sidepot (0, [], 0, false, false);

(*
	sh_emptySumpot
	TYPE: 		sumpot
	VALUE: 		An empty sumpot. 
*)
val sh_emptySumpot = Sumpot(0, 0);
(*
	sh_mkFull s
	TYPE:		sidepot list -> sidepot list
	PRE:		(none)
	POST:		s as a sidepot list where full is changed to
	 			true if allin = true. 
	EXAMPLE:	sh_mkFull([Sidepot (0, [(0, 1, 500), (1, 1, 500)], 500, true, false),
	    		Sidepot (1, [(0, 1, 500)], 500, false)], true, false) =
				[Sidepot (0, [(0, 1, 500), (1, 1, 500)], 500, true, true),
		    	Sidepot (1, [(0, 1, 500)], 500, true, true)]: sidepot list
*)		
(*VARIANT: length s*)
fun sh_mkFull([]) = []
| sh_mkFull(Sidepot(nr'', pl'', pot'', allin'', full'')::xs) = 
	if allin'' = true then
		Sidepot(nr'', pl'', pot'', allin'', true)::sh_mkFull(xs)
	else
		Sidepot(nr'', pl'', pot'', allin'', full'')::sh_mkFull(xs)
		
(*
	sh_mkSidepot l, id', h', m', allin'
	TYPE: 		sidepot list * int * int * int * bool * 'a -> sidepot list
	PRE:		id' >= 0, h' > 0, m' > 0
	POST: 		l as a sidepot list where (id', h', m') is added to
				pl or a new playerlist in a new sidepot if allin' is true or not.
	EXAMPLE: 	sh_mkSidepot([Sidepot(0, [], 0, false)], 0, 1, 500, false, false) = 
				[Sidepot (0, [Player (0, 1, 500)], 500, false, false)]: sidepot list
*)
(*VARIANT: length l*)
fun sh_mkSidepot([], _, _, _, _) = []
| sh_mkSidepot(l as iSp::xs, id, h, m, allin) =
	let 
		(*
			updNr l, nr'
			TYPE:		sidepot list * int -> sidepot list
			PRE:		nr' >= 0
			POST:		l as a sidepot list where nr is changed to nr+1
						if nr >= nr'.
			EXAMPLE:	updNr([Sidepot (1, [Player (0, 1, 200), Player (1, 1, 200), Player (2, 1, 200)], 200, false)], 0) = 
						[Sidepot (2, [Player (0, 1, 200), Player (1, 1, 200), Player (2, 1, 200)], 200, false, false)]: sidepot list
		*)
		(*VARIANT: length l*)
		fun updNr([], _) = [] 
		| updNr((iSp' as Sidepot(nr', pl', pot', allin', full'))::xs, nr) =
			if nr' < nr then
				iSp' :: updNr(xs, nr)
			else
				Sidepot(nr'+1, pl', pot', allin', full') :: updNr(xs, nr)
		(*
			chOldSidepot s', id', h', m', full'
			TYPE:		sidepot * int * int * int * bool -> sidepot
			PRE:		id' > 0, h' > 0, m' > 0
			POST:		A sidepot of s where m = m' and (id', h', m')::pl. 
			EXAMPLE:	chOldSidepot(Sidepot (1, [Player (0, 1, 200), Player (1, 1, 200), 
						Player (2, 1, 200)], 200, false), 3, 1, 100) =
						Sidepot (1, [Player (3, 1, 100), Player (0, 1, 100), 
						Player (1, 1, 100), Player (2, 1, 100)], 100, false, false): sidepot
		*)
		fun chOldSidepot(Sidepot(nr'', pl'', pot'', allin'', full''), id', h', m', allin') =
			let
				(*
					chOldSidepot'(l, id', h', m')
					TYPE:		player list * int * int * int -> player list
					PRE:		m' > 0 
					POST:		l as a player list where m is changed to m' if 
								m >= m, else m is unchanged. 
					EXAMPLE:	chOldSidepot([Player (0, 1, 500), Player (1, 1, 500)], 2, 1, 200) = 
								[Player (2, 1, 200), Player (0, 1, 200), Player (1, 1, 200)]: player list
				*)
				(*VARIANT: length l*)
				fun chOldSidepot'([], id', h', m') = [Player (id', h', m')]
				| chOldSidepot'(Player (id'', h'', m'')::xs'', id', h', m') =
					if m'' >= m' then
						Player (id'', h'', m')::chOldSidepot'(xs'', id', h', m')
					else
						Player (id'', h'', m'')::chOldSidepot'(xs'', id', h', m')
			in
				Sidepot(nr'', chOldSidepot'(pl'', id', h', m'), m', allin', full'')
			end
		(*
			mkNewSidepot s, m'
			TYPE:		sidepot * int -> sidepot
			PRE:		m' > 0 
			POST:		s as a sidepot where m is changed to m-m' for every player
						in the player list.
			EXAMPLE:	chOldSidepot(Sidepot (1, [Player (0, 1, 200), Player (1, 1, 200), 
						Player (2, 1, 200)], 200, false, false), 50) = 
						Sidepot (1, [Player (0, 1, 50), Player (1, 1, 50), Player (2, 1, 50)], 50, false, false): sidepot
		*)
		fun mkNewSidepot(Sidepot(nr'', pl'', pot'', allin'', full''), m') =
			let
				(*
				mkNewSidepot l m'
				TYPE: 		player list * int -> player list
				PRE: 		m' > 0
				POST: 		l as a player list where m is changed to m-m' for every player
							in the player list.
				EXAMPLE: 	mkNewSidepot([Player (0, 1, 200), Player (1, 1, 200), Player (2, 1, 200)], 50) = 
							Sidepot (1, [Player (0, 1, 150), Player (1, 1, 150), Player (2, 1, 150)], 150, false): player list
				*)
				(*VARIANT: length l*)
				fun mkNewSidepot'([], m''') = []
				| mkNewSidepot'(Player (id'', h'', m'')::xs'', m') =
					Player (id'', h'', m''-m')::mkNewSidepot'(xs'', m')
			in
				Sidepot(nr''+1, mkNewSidepot'(pl'', m'), pot''-m', allin'', full'')
			end
		(*
			updPlayerMoney l id' m'
			TYPE: 		player list * int * int -> player list
			PRE:		m > 0 
			POST:		l as a player list where m is changed to
						m' if id = id'. 
			EXAMPLE: updPlayerMoney([Player (0, 1, 150), Player (1, 1, 150), Player (2, 1, 150)], 0, 200) =
						[Player (0, 1, 200), Player (1, 1, 150), Player (2, 1, 150)]: player list
		*)	
		fun updPlayerMoney([], id', m') = []
		| updPlayerMoney(Player (id'', h'', m'')::xs'', id', m') =
			if id'' = id' then
				Player (id'', h'', m')::xs''
			else
				Player (id'', h'', m'')::updPlayerMoney(xs'', id', m')
		(*
			getPlayerMoney l id'
			TYPE: 		player list * int -> int
			PRE:		id' >= 0
			POST:		m in l if id = id'.
			EXAMPLE:	getPlayerMoney([Player (0, 1, 150), Player (1, 1, 150), Player (2, 1, 150)], 0) =
						150
		*)		
		fun getPlayerMoney([], id') = 0
		| getPlayerMoney(Player (id'', h'', m'')::xs'', id') =
			if id'' = id' then
				m''
			else
				getPlayerMoney(xs'', id')
		(*
			sh_mkSidepot' l, id', h', m', allin'
			TYPE:		sidepot list * int * int * int * int-> sidepot list
			PRE:		m > 0
			POST:		l as a sidepot list where (id', h', m') is added to
						pl or a new playerlist in a new sidepot depending if allin' is true or not.
			EXAMPLE:	sh_mkSidepot'([Sidepot(0, [], 0, false, false)], 0, 1, 500) = 
						[Sidepot (0, [(0, 1, 500)], 500, false, false)]: sidepot list
		*)
		(*VARIANT: length l*)
		
		fun sh_mkSidepot'([], iSp' as Sidepot(nr', pl', pot', allin', full'), id, h, m, allin) = 
			let
				val getMoney = getPlayerMoney(pl', id)
				val newMoney = getMoney + m
			in
				if full' = false then (*Sidepot opened*)
					if allin' = false then (*Sidepot isn't an allin pot*)
						if getMoney > 0 then (*Player is already in the side pot, update player's money, sidepot money and sidepot allin*)
							Sidepot(nr', updPlayerMoney(pl', id, newMoney), newMoney, allin, full')::[]
						else (*Player isn't in the pot. *)	
							if allin = false then (*Player isn't allin. Add player to pot and update sidepot money.*)
								Sidepot(nr', Player (id, h, m)::pl', m, allin', full')::[]
							else (*Player is allin. Check if split pot is needed.*)
								if m >= pot' then (*Money is the same or bigger as pot. Add player and change sidepot money and allin*)
									Sidepot(nr', Player (id, h, m)::pl', m, allin, full')::[]
								else (*Make a split*)
									chOldSidepot(iSp', id, h, m, allin)::mkNewSidepot(iSp', m)::[]
		
					else (*Sidepot is an allin pot*)
						if newMoney = pot' then (*Money is the same as pot. Change money.*)
							Sidepot(nr', if getMoney > 0 then updPlayerMoney(pl', id, newMoney) else Player (id, h, m)::pl', pot', allin', full')::[]
						else if newMoney > pot' then (*Money is bigger than pot. Change money and add a new pot.*)
							Sidepot(nr', if getMoney > 0 then updPlayerMoney(pl', id, pot') else Player (id, h, pot')::pl', pot', allin', full')::Sidepot(nr'+1,  [Player (id, h, m-pot')], newMoney-pot', allin, full')::[]
						else (*Money is smaller than pot. Make a split. *)
							chOldSidepot(iSp', id, h, m, allin)::mkNewSidepot(iSp', m)::[]
				else (*Sidepot is closed. Cons iSp' and make a new sidepot. *)
					iSp'::Sidepot(nr'+1, [Player (id, h, m)], m, allin, false)::[]
			end
				
		| sh_mkSidepot'(l as iSp'::xs, iSp as Sidepot(nr', pl', pot', allin', full'), id, h, m, allin) =	
		let
			val getMoney = getPlayerMoney(pl', id)
			val newMoney = getMoney + m
		in
			if full' = false then (*Sidepot opened*)
				if getMoney > 0 then (*Player is already in the side pot.*)
					if getMoney < pot' then (*Player's money in sidepot is less than sidepot*)
						if newMoney > pot' then (*Player's money is bigger than sidepot. Update player's money and try next sidepot.*)
							Sidepot(nr', updPlayerMoney(pl', id, pot'), pot', allin', full')::sh_mkSidepot'(xs, iSp', id, h, m-pot', allin)
						else	(*Player's money is smaller than sidepot. Update player's money. *)
							Sidepot(nr', updPlayerMoney(pl', id, newMoney), pot', allin', full')::l
					else (*Sidepot is full, try next sidepot*)
						iSp::sh_mkSidepot'(xs, iSp', id, h, m, allin)
				else (*Player isn't in the side spot*)
					if m = pot' then (*Money is the same as pot. Add player to sidepot, change allin. Terminate.*)
						Sidepot(nr', Player (id, h, m)::pl', pot', allin, full')::l
					else if m > pot' then (*Money is bigger than pot. Add player and try next sidepot*)
						Sidepot(nr', Player (id, h, pot')::pl', pot', allin, full')::sh_mkSidepot'(xs, iSp', id, h, m-pot', allin)
					else (*Money is less than pot. Make a split.*)
						chOldSidepot(iSp, id, h, m, allin)::mkNewSidepot(iSp, m)::updNr(l, nr')
				
			else	(*Sidepot closed. Cons iSp and try next sidepot. *)
				iSp::sh_mkSidepot'(xs, iSp', id, h, m, allin)
		end
	in
			sh_mkSidepot'(xs, iSp, id, h, m, allin)
	end;
(*
	sh_unCalled l
	TYPE: 		sidepot list -> player 
	PRE: 		(none)
	POST: 		The last element in the player list of the last sidepot of l. 
	EXAMPLE: 	sh_unCalled([Sidepot (0, [Player (1, 9999, 25), Player (0, 9999, 25)], 25, true, false),
	    		Sidepot (1, [Player (1, 9999, 25)], 25, true, false)]) = Player (1, 9999, 25): player
*)
fun sh_unCalled([]) = sh_emptyPlayer
| sh_unCalled l = 
	let
		val last = List.last l
		(*
			sh_unCalled' last
			TYPE: 		sidepot list -> player
			PRE:		(none)
			POST: 		The last element in the player list.
			EXAMPLE: 	sh_unCalled'(Sidepot (1, [Player (0, 9999, 25), Player (1, 9999, 25)], 25, true, false)) =
						Player (1, 9999, 25)
		*)
		fun sh_unCalled'(Sidepot (_, [], _, _, _)) = sh_emptyPlayer
		| sh_unCalled'(Sidepot (nr, pl as Player (id, h, m)::xs, pot, allin, full)) =
			if length pl = 1 then
				Player (id, h, m)
			else
				sh_emptyPlayer
	in
		sh_unCalled' last
	end;
(*
	sh_sumPots l
	TYPE:		sidepot list -> sumpot list
	PRE:		(none)
	POST:		l as a sumpot list.
	EXAMPLE: 	sh_sumPots([Sidepot (0, [Player (1, 9999, 25), Player (0, 9999, 25)], 25, true, false),
	    	 	Sidepot (1, [Player (1, 9999, 25)], 25, true, false)]) = [Sumpot (0, 50), Sumpot (1, 25)]: sumpot list
*)
fun sh_sumPots([]) = []
| sh_sumPots(l) = 
	let
		(*
			sumFromPlayer p
			TYPE: 		player -> int
			PRE: 		(none)
			POST: 		m in p.
			EXAMPLE: 	sumFromPlayer(Player (0, 1, 50)) = 50: int
		*)
		fun sumFromPlayer(Player (_,_,sum)) = sum
		
		(*
			sh_sumPots' l
			TYPE: 		sidepot list -> sumpot list
			PRE:		(none)
			POST: 		l as a sumpot list.
			EXAMPLE: 	sh_sumPots'([Sidepot (0, [Player (1, 9999, 25), Player (0, 9999, 25)], 25, true, false),
			    	 	Sidepot (1, [Player (1, 9999, 25)], 25, true, false)]) = [Sumpot (0, 50), Sumpot (1, 25)]: sumpot list
		*)
		fun sh_sumPots'([]) = []
		| sh_sumPots'(Sidepot (_, [], _, _, _)::_) = []
		| sh_sumPots'(Sidepot (nr, pl as Player (p, h, m)::xs', pot, allin, full)::xs) =
			let
				val cash = foldr (fn (Player (_,_,pot1), Player (_,_,pot2)) => Player (0, 0, pot1+pot2)) (Player (0, 0, 0)) pl
			in
		 		Sumpot (nr, sumFromPlayer(cash))::sh_sumPots'(xs)
			end
	in
		sh_sumPots'(l)
	end;
(*
	sh_totPot l 
	TYPE: 		sidepot list -> int
	PRE: 		(none)
	POST: 		The sum of all m in the player lists of all the sidepots in l.  
	EXAMPLE: 	sh_totPot([Sidepot (0, [(1, 9999, 25), (0, 9999, 25)], 25, true, false),
	    	 	Sidepot (1, [(1, 9999, 25)], 25, true, false)]) = 75: int
*)
fun sh_totPot l = 
	let
		(*
			sumFromSumpot s
			TYPE: 		sumpot -> int
			PRE: 		(none)
			POST: 		sum in s. 
			EXAMPLE: 	sumFromSumpot(Sumpot (0, 50)) = 50: int
		*)
		fun sumFromSumpot(Sumpot (_,sum)) = sum
		
		val sumPots = sh_sumPots l 
		val cash = foldr (fn (Sumpot (_,pot1), Sumpot (_,pot2)) => Sumpot (0, pot1+pot2)) (Sumpot (0, 0)) sumPots
	in
		sumFromSumpot(cash)
	end;		
(*
	sh_updateHands l b
	TYPE: 		sidepot list * besthand list -> sidepot list
	PRE: 		(none)
	POST: 		l as a sidepot list where every id and m in the player lists
	 			of l are changed to id' and m' in b.
	EXAMPLE: 	sh_updateHands([Sidepot (0, [(0, 1, 500)], 500, false, false)], [(0, 200)]) =
				[Sidepot (0, [(0, 200, 500)], 500, false, false)]: sidepot list
*)
(*VARIANT: length l*)
fun sh_updateHands([], _) = []
| sh_updateHands(l, []) = l
| sh_updateHands(x::xs, l') = 
	let 
		(*
			sh_updateHands'(l, b)
			TYPE: 		sidepot * besthand list -> sidepot 
			PRE:		(none)
			POST:		l as a sidepot where every id and m in the player list
			 			of l are changed to id' and m' in b. 
			EXAMPLE: 	sh_updateHands'(Sidepot (0, [(0, 1, 500)], 500, false, false), [(0, 200)]) =
						Sidepot (0, [(0, 200, 500)], 500, false, false): sidepot
		*)
		fun sh_updateHands'(Sidepot(nr, pl, pot, allin, full), xs'') = 
			let
				(*
				sh_updateHands'' (l, h', n)
				TYPE: 		player list * besthand list * player list -> player list
				PRE: 		(none)
				POST: 		n as a player list where every id and m in l are changed
							to every id' and m in h'. 
				EXAMPLE: 	sh_updateHands''([(0, 9999, 500), (1, 9999, 500)], [(0, 1), (1, 5)], []) =
							[(0, 1, 500), (1, 5, 500)]: player list
				*)
				(*VARIANT: length pl*)
				fun sh_updateHands''(_, [], _) = []
				| sh_updateHands''([], (id'', h'')::xs''', new) = 
					if xs''' <> [] then
						sh_updateHands''(new, xs''', [])
					else
						new
				| sh_updateHands''(Player (id''', h''', m''')::xs'''', l'' as (id'', h'')::xs''', new) = 
					if id''' = id'' then
						sh_updateHands''(xs'''', l'', Player (id''', h'', m''')::new)
					else
						sh_updateHands''(xs'''', l'', Player (id''', h''', m''')::new)
			in
				Sidepot(nr, sh_updateHands''(pl, xs'', []), pot, allin, full)::sh_updateHands(xs, xs'')
			end
	in
		sh_updateHands'(x, l')
	end;

(*
	showDown l 
	TYPE: 		sidepot list -> sidepot list
	PRE:		(none)
	POST:		l as a sidepot list with sidepots where player lists only contain of the player(s) 
				with the minimum h (from now on reffering as w) in a sidepot. w:s m will be the sum of all m in the
				pl divided by w. The pot will be the same as this sum. allin and full will both be true. 
				
	EXAMPLE: 	showDown([Sidepot (0, [Player (2, 1200, 200), Player (1, 5, 200), Player (0, 1, 200)], 200, true, false),
	    		Sidepot (1, [Player (1, 5, 300), Player (0, 1, 300)], 300, true, false),
	    		Sidepot (2, [Player (0, 1, 500)], 500, false, false)]) =
				[Sidepot (0, [Player (0, 1, 600)], 600, true, true),
				Sidepot (1, [Player (0, 1, 600)], 600, true, true),
				Sidepot (2, [Player (0, 1, 500)], 500, true, true)]: sidepot list
*)	
(*VARIANT: length l*)
fun showDown([]) = []
| showDown(Sidepot(nr, pl, pot, allin, full)::xs) = 
	let
		(*	
			winners l
			TYPE:		player list -> player list
			PRE:		(none)
			POST:		l as a player list with the smallest h in common. 
			EXAMPLE:	winners([Player (0, 1, 500), Player (3, 1600, 700), Player (7, 5068, 700)]) = [Player (0, 1, 500)] 
		*)
		(*VARIANT: length l*)
		fun winners [] = [] 
		| winners ((x as Player (p, h, m))::xs) =
			let 
				(*
					winners' l, m', h'
					TYPE: 		player list * player list * int -> player list
					PRE: 		h > 0
					POST:		m' as a new player list from l with the smallest common h'. 
					EXAMPLE: 	winners'([Player (3, 1600, 700), Player (7, 5068, 700)], [Player (0, 1, 500), 1]) =
								[(0, 1, 500)] 
				*)
				(*VARIANT: length l*)
				fun winners'([], bestPl, _) = bestPl
				| winners'((x' as Player (p', h', m'))::xs, bestPl, bestHa) =
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
			TYPE:		player list -> int
			PRE:		(none)
			POST:		The sum of all m in l. 
			EXAMPLE:	cashFromPlayers([Player (0, 1, 500), Player (3, 1600, 700), Player (7, 5068, 700)]) = 1900: int
		*)
		(*VARIANT: length l*)
		fun cashFromPlayers(l) = 
			let
				val cash = foldr (fn (Player (_,_,pot1), Player (_,_,pot2)) => Player (0, 0, pot1+pot2)) (Player (0, 0, 0)) l
				(*
					cashFromPlayers' p
					TYPE: 		player -> int
					PRE: 		(none)
					POST: 		m in p.
					EXAMPLE: 	cashFromPlayers'(Player (0, 1, 50)) = 50: int
				*)
				fun cashFromPlayers'(Player (_,_,sum)) = sum
			in
				cashFromPlayers'(cash)
			end;
		(*
			sh_mkSidepot nr', w', t'
			TYPE:		int * player list * int -> sidepot
			PRE:		t' >= 0
			POST:		A sidepot where nr = nr', where w' is the player list
						except that m in the player list is t' divided by the length of w',
						pot = t', allin and full are both true. 
			EXAMPLE:	sh_mkSidepot(0, [Player (0, 1, 500), Player (1, 1, 500)], 1500) =
						Sidepot(0, [Player (0, 1, 750), Player (1, 1, 750)], 1500, true, true)
		*)		
		fun sh_mkSidepot(nr, [], _) = sh_emptySidepot 
		| sh_mkSidepot(nr, winners, tot) =
			let
				val players = length winners
				val cashEach = tot div players
				
				(*
					sh_mkSidepot'(l, c, w, t)
					TYPE: 		player list * int * player list * int -> sidepot
					PRE: 		c > 0, t > 0
					POST: 		A sidepot where pl = w, pot = tot, allin and full are
								both true. 
					EXAMPLE: 	sh_mkSidepot'([(0, 1, 500), (1, 1, 500)], 200, [], 1400) =
								Sidepot(0, [(0, 1, 700), (1, 1, 700)], 1400, true, true): sidepot
				*)
				(*VARIANT: length l*)
				fun sh_mkSidepot'([], _, winners, tot) = Sidepot(nr, winners, tot, true, true)
				| sh_mkSidepot'(Player (p, h, m)::xs, cashEach, winners, tot) =
					sh_mkSidepot'(xs, cashEach, Player (p, h, cashEach)::winners, tot)
			in
				sh_mkSidepot'(winners, cashEach, [], tot)
			end
		
		val winners = winners(pl)
		val total = cashFromPlayers(pl)
		val sh_mkSidepot = sh_mkSidepot(nr, winners, total)
	in
		sh_mkSidepot::showDown(xs)
	end; 