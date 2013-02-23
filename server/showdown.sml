abstype sidepot = Sidepot of (int * int * int) list * int
	with
	
	(*
		emptyPot
		TYPE: 		sidepot
		VALUE: 		An empty sidepot. 
	*)
	val emptyPot = Sidepot ([], 0);

	(*	
		winners l
		TYPE:		('a * int * 'b) list -> ('a * int * 'b) list
		PRE:		(none)
		POST:		
		EXAMPLE:	winners([(0, 1, 500), (3, 1600, 700), (7, 5068, 700)]) = [(0, 1, 500)] 
	*)
	(*
		INFO:		Collects all winners (from a list of players p contributed to the pot) to a list.
		USED BY: 	winnersMinPot(w), cashToWinners(w, l)
	*)
	fun winners [] = [] 
	| winners ((x as (p, h, m))::xs) =
		let 
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
		end;

	(*
		winnersMinPot l
		TYPE: 		(int * int * int) list -> sidepot
		PRE:		(none)
		POST:		A new Sidepot. 
		EXAMPLE: 	winnersMinPot([(0, 1, 500), (1, 1, 1500)]) = Sidepot ([(0, 1, 500)], 500): sidepot 
	*)
	(*
		INFO:		Get all the winners from winners(l) with minimum pot into a sidepot.
		USED BY: 	cashFromLosers(l, p), rmMinPotWinner(m, l)
	*)
	fun winnersMinPot([]) = []
	| winnersMinPot((x as (p, h, m))::xs) =
		let
			fun winnersMinPot'([], leastPl, _) = leastPl
			| winnersMinPot'((x' as (p', h', m'))::xs, leastPl, least) =
				if m' < least then
					winnersMinPot'(xs, [x'], m')
				else if m' = least then
					winnersMinPot'(xs, x'::leastPl, least)
				else
					winnersMinPot'(xs, leastPl, least)
		in
			winnersMinPot'(xs, [x], m)
		end;
	(*
		cashFromLosers 		l, m
		TYPE:		sidepot * (int * int * int) list -> sidepot
		PRE:		(none)
		POST:		A new Sidepot. 
		EXAMPLE: 	cashFromLosers([(0, 1, 500)], [(0, 1, 500), (3, 1600, 700), (7, 5068, 700)]) =
					Sidepot ([(7, 5068, 200), (3, 1600, 200)], 1000): sidepot
	*)
	(*
		INFO:		Get all the losers into a sidepot.
		USED BY: 	cashToWinners(w, l), mkNewList(l, m)
	*)
	fun cashFromLosers([], _) = emptyPot
	| cashFromLosers((p, h, m)::xs, org) = 
		let
			fun cashFromLosers'([], _, _, losers, totMoney) = Sidepot(losers, totMoney) 
			| cashFromLosers'((p', h', m')::xs', hand, money, losers, totMoney) =
				if hand <> h' then
					if m' >= money then
						cashFromLosers'(xs', hand, money, (p', h', m'-money)::losers, totMoney+money)
					else 
						cashFromLosers'(xs', hand, money, (p', h', 0)::losers, totMoney+m')
				else
					cashFromLosers'(xs', hand, money, losers, totMoney)
		in
			cashFromLosers'(org, h, m, [], 0)
		end;

	(*
		cashToWinners	l, m
		TYPE:		(int * int * int) list * sidepot -> sidepot
		PRE:		(none)
		POST: 		A new sidepot. 
		EXAMPLE: 	cashToWinners([(0, 1, 500), (1, 1, 1500)], 800) = [(0, 1, 900), (1, 1, 1900)]
	*)
	(*
		INFO: 		Give money to winners into a new sidepot. 
		USED BY: 	mkNewList(l, w)
	*)
	fun cashToWinners([], _) = emptyPot 
	| cashToWinners(winners, Sidepot(l, tot)) =
		let
			val players = length winners
			val cashEach = tot div players
			
			fun cashToWinners'([], _, winners, tot) = Sidepot(winners, tot)
			| cashToWinners'((p, h, m)::xs, cashEach, winners, tot) =
				cashToWinners'(xs, cashEach, (p, h, m+cashEach)::winners, tot)
		in
			cashToWinners'(winners, cashEach, [], tot)
		end;

	(*
		mkNewList l, m
		TYPE:		sidepot * sidepot -> (int * int * int) list
		PRE:		(none)
		POST:		A new list with lists in l and m combined. 
		EXAMPLE:	mkNewList(Sidepot([(0, 1, 500), (1, 1, 700)], 800))
	*)
	(*
		INFO: 		Append winners' list and cashFromLosers' list into a new list. 
		USED BY: 	rmMinPotWinner(m, l)
	*)
	fun mkNewList (Sidepot(l, t), Sidepot(l', t')) = l@l';
	
	(*
		rmMinPotWinner l, m
		TYPE: 		(''a * 'b * int) list * (''a * 'c * 'd) list -> (''a * 'b * int) list
		PRE:		(none)
		POST: 		A new (''a * 'b * int) list. 
		EXAMPLE: 	rmMinPotWinner([(0, 1, 900), (1, 1, 1900), (3, 1600, 200), (7, 5068, 200)], [(0, 1, 500)] = [(1, 1, 1900), (3, 1600, 200), (7, 5068, 200)]	 
	*)
	(*
		INFO: 		Remove the winning players with starting minimum pot.
		USED BY: 	showDown(l)
	*)
	fun rmMinPotWinner(org, []) = org
	| rmMinPotWinner(org, w'::ws') =
		let
			fun rmMinPotWinner'([], _, next, org') = rmMinPotWinner(org', next)
			| rmMinPotWinner'((x as (p, h, m))::xs, w' as (p', h', m'), next, org') =
				if p = p' orelse m = 0 then
					rmMinPotWinner'(xs, w', next, org')
				else
					rmMinPotWinner'(xs, w', next, x::org')
		in
			rmMinPotWinner'(org, w', ws', [])
		end;

	(*
		showDown p
		TYPE:		(int * int * int) list -> sidepot list
		PRE:		(none)
		POST:		A sidepot list. 
		EXAMPLE: 	showDown([(0, 1, 500), (5, 1, 500), (1, 1, 1000), (3, 1600, 2500), (7, 5068, 2000)]) =
					[Sidepot ([(1, 1, 1333), (5, 1, 833), (0, 1, 833)], 1000),
					 Sidepot ([(1, 1, 3999)], 2666), Sidepot ([(3, 1600, 834)], 167)]:  sidepot list
	*)
	(*
		INFO: 		Returns a sidepot list with all the sidepots for a specific game. 
	*)
	fun showDown([]) = []
	| showDown p = 
	let
			val winners = winners(p)
			val minPot = winnersMinPot(winners)
			val loserCash = cashFromLosers(minPot, p)
			val mkSidepot = cashToWinners(winners, loserCash)
			val mkNewList = mkNewList(loserCash, mkSidepot)
			val rmMinPotWinner = rmMinPotWinner(mkNewList, minPot)
		in
			mkSidepot::showDown(rmMinPotWinner)						(*Cons Sidepot and repeat process.*)
		end;
		
	(*
		printShowDown l
		TYPE:		sidepot list -> string
		PRE:		(none)
		POST:		l in text as a string. 
		EXAMPLE:	showDown([(0, 1, 500), (1, 1, 700), (3, 1600, 2500), (7, 5068, 2000)]) =
		 			"1 and 0 split a pot of 1000. \n1 won a pot of $2400.\n3 won a pot of $300.\n": string
	*)
	(*
		INFO: 		Returns information of all the players involved in the sidepot. 
	*)

	fun printShowDown([]) = ""
	| printShowDown(Sidepot(players as (p, h, m)::xs, t)::rest) = 
		let 
			val antPlayers = length players
			val intStr = Int.toString

			fun printShowDown'([], t, rest) = "split a pot of $"^intStr(t)^".\n"^printShowDown(rest)
			| printShowDown'(player as (p, h, m)::xs, t, rest) =
				if xs = [] then
					intStr(p)^" "^printShowDown'(xs, t, rest)
				else
					intStr(p)^" and "^printShowDown'(xs, t, rest)
		in
			if antPlayers = 1 then
				intStr(p)^" won a pot of $"^intStr(m)^".\n"^printShowDown(rest)
			else
				printShowDown'(players, t, rest)
		end;

end;