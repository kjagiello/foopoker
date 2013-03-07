(*
	eval_printCard n 
	TYPE:		playcard -> string
	PRE:		(none)
	POST:		Representation of n.
	EXAMPLE:	eval_printCard(Playcard 0wx8001B25) = "Ks": string
*)
(*
	INFO: 		***Cactus Kev's Poker Hand Evaluator***
*)
fun eval_printCard(Playcard(n)) = 
	let
		val op andb = Word32.andb
		val r = Word32.>>(n, Word.fromInt(8))
		val r = Word32.toInt(andb(r, 0wxF))
		val w = Word32.fromInt(0)
	in
		if andb(n, 0wx8000) > w then
			v_rank(r) ^ "c"
		else if andb(n, 0wx4000) > w then
			v_rank(r) ^ "d"
		else if andb(n, 0wx2000) > w then
			v_rank(r) ^ "h"
		else
			v_rank(r) ^ "s"
	end;
(*
	eval_findCard n 
	TYPE:		Word32.word -> Word32.word
	PRE:		(none)
	POST:		n as a Word32.word.
	EXAMPLE: 	eval_findCard(0wxFF) = 0wxCCC: Word32.word
*)
(*
	INFO: 		***Paul D. Senzee's Optimized Hand Evaluator
				for Cactus Kev's Poker Hand Evaluator***
*)
fun eval_findCard(n) =
	let
		val frInt = Word.fromInt
		val op ++ = Word32.+
		val op xorb = Word32.xorb
		val op << = Word32.<<
		val op >> = Word32.>>
		val op andb = Word32.andb
		
		val u = ++(n, 0wxe91aaa35)
		val u = xorb(u, >>(u, frInt(16)))
		val u = ++(u, <<(u, frInt(8)))
		val u = xorb(u, >>(u, frInt(4)))
		
		val b = Word32.toInt(andb(>>(u, frInt(8)), 0wx1ff))
		val a = >>( ++(u, <<(u, frInt(2)) ), frInt(19))
		val r = xorb(a, Word32.fromInt(v_adjust(b)))
	in
		r
	end;
	
(*
	eval_5cards (c1, c2, c3, c4, c5)
	TYPE: 		playcard * playcard * playcard * playcard * playcard -> handvalue
	PRE: 		c1-c5 must have been created by shuffleDeck().
	POST: 		c1-c5 as a handvalue. 
	EXAMPLE: 	eval_5cards((Playcard 0wx10002C29, Playcard 0wx1002817, Playcard 0wx4002A1F, Playcard 0wx200291D,
	    		Playcard 0wx8002B25)) = Handvalue 1: handvalue
*)
(*
	INFO: 		***Paul D. Senzee's Optimized Hand Evaluator
				for Cactus Kev's Poker Hand Evaluator***
*)
fun eval_5cards (Playcard(c1), Playcard(c2), Playcard(c3), Playcard(c4), Playcard(c5)) =
	let
		val orb = Word32.orb
		val frInt = Word.fromInt
		val frInt32 = Word32.fromInt
		val toInt32 = Word32.toInt
		val w = Word32.fromInt(0)
		val andb = Word32.andb
		val op >> = Word32.>>
		
		val q = Word32.toInt(>>(orb(orb(orb(orb(c1, c2), c3), c4), c5), frInt(16)))
		val s = v_unique5(q)
		val p = andb(c1, 0wxFF) * andb(c2, 0wxFF) * andb(c3, 0wxFF) * andb(c4, 0wxFF) * andb(c5, 0wxFF)
	in	
		(*check for v_flushes and Straightv_flushes*)
		if andb(andb(andb(andb(andb(c1, c2), c3), c4), c5), 0wxF000) > w then 
			Handvalue(v_flushes(q))
		(*check for Straights and HighCard hands*)
		else if frInt32(s) > w then 
		 	Handvalue(s)
		(*check for every other hand*)
		else
			Handvalue(v_values(toInt32(eval_findCard(p))))
	end;

(*
	eval_6hand (c1, c2, c3, c4, c5, c6)
	TYPE:		playcard * playcard * playcard * playcard * playcard * playcard -> handvalue
	PRE:		(none)
	POST:		The best handvalue out of 6 cards. 
	EXAMPLE: 	eval_6hand(Card 0wx10002C29, Card 0wx1002817, 
				Playcard 0wx4002A1F, Playcard 0wx200291D, Playcard 0wx408611, Playcard 0wx8002B25) = Handvalue 1: handvalue
*)
fun eval_6hand(c1, c2, c3, c4, c5, c6) =
	let
		fun eval_6hand'(_, _, _, _, _, _, 0, best) = Handvalue(best)
		| eval_6hand'(c1', c2', c3', c4', c5', c6', n, best) =
			let 
				val perm = (c1', c2', c3', c4', c5', c6', n-1)
				val bestfive = handValueToInt(eval_5cards(v_perm6(perm)))
			in
				if bestfive < best then
					eval_6hand'(c1', c2', c3', c4', c5', c6', n-1, bestfive)
				else
					eval_6hand'(c1', c2', c3', c4', c5', c6', n-1, best)
			end
	in
		eval_6hand'(c1, c2, c3, c4, c5, c6, 6, 9999)
	end;
	
(*
	eval_7hand (c1, c2, c3, c4, c5, c6, c7)
	TYPE:		playcard * playcard * playcard * playcard * playcard * playcard * playcard -> handvalue
	PRE:		(none)
	POST:		The best handvalue out of 7 cards.
	EXAMPLE: 	eval_7hand(Playcard 0wx10002C29, Playcard 0wx1002817, Playcard 0wx4002A1F, 
				Playcard 0wx200291D, Playcard 0wx408611, Playcard 0wx1001817, Playcard 0wx8002B25) = Handvalue 1: handvalue
*)
(*
	INFO: 		***Cactus Kev's Poker Hand Evaluator***
*)
fun eval_7hand(c1, c2, c3, c4, c5, c6, c7) =
	let
		fun eval_7hand'(_, _, _, _, _, _, _, 0, best) = Handvalue(best)
		| eval_7hand'(c1', c2', c3', c4', c5', c6', c7', n, best) =
			let 
				val perm = (c1', c2', c3', c4', c5', c6', c7', n-1)
				val bestfive = handValueToInt(eval_5cards(v_perm7(perm)))
			in
				if bestfive < best then
					eval_7hand'(c1', c2', c3', c4', c5', c6', c7', n-1, bestfive)
				else
					eval_7hand'(c1', c2', c3', c4', c5', c6', c7', n-1, best)
			end
	in
		eval_7hand'(c1, c2, c3, c4, c5, c6, c7, 21, 9999) 
	end;

(*
	eval_print7hand (c1, c2, c3, c4, c5, c6, c7)
	TYPE: 		playcard * playcard * playcard * playcard * playcard * playcard * playcard ->
	     		playcard * playcard * playcard * playcard * playcard
	PRE:		(none)
	POST:		The best handvalue 5 cards can get out of 7 cards.
	EXAMPLE: 	eval_print7hand(Card 0wx10002C29, Card 0wx1002817, Card 0wx4002A1F, Card 0wx200291D, Card 0wx408611, 
				Card 0wx1001817, Card 0wx8002B25) = 
				(Card 0wx10002C29, Card 0wx1002817, Card 0wx4002A1F, Card 0wx200291D,
				Card 0wx8002B25): card * card * card * card * card
*)
fun eval_print7hand(c1, c2, c3, c4, c5, c6, c7) =
	let
		fun eval_print7hand'(_, _, _, _, _, _, _, 0, best, (a, b, c, d, e)) = (a, b, c, d, e)
		| eval_print7hand'(c1', c2', c3', c4', c5', c6', c7', n, best, bestHand) =
			let 
				val perm = (c1', c2', c3', c4', c5', c6', c7', n-1)
				val bestfive = handValueToInt(eval_5cards(v_perm7(perm)))
			in
				if bestfive < best then
					eval_print7hand'(c1', c2', c3', c4', c5', c6', c7', n-1, bestfive, v_perm7(perm))
				else
					eval_print7hand'(c1', c2', c3', c4', c5', c6', c7', n-1, best, bestHand)
			end
	in
		eval_print7hand'(c1, c2, c3, c4, c5, c6, c7, 21, 9999, (Playcard 0wx0, Playcard 0wx0, Playcard 0wx0, Playcard 0wx0, Playcard 0wx0))
	end;
	
(*
	eval_cardsToString c1, c2, c3, c4, c5
	TYPE:		playcard * playcard * playcard * playcard * playcard -> string
	PRE:		(none)
	POST:		Representation of c1-c5. 
	EXAMPLE: 	handToString((Card 0wx10002C29, Card 0wx1002817, Card 0wx4002A1F, Card 0wx200291D,
				Card 0wx8002B25)) = "Ah-Th-Qh-Jh-Kh": string
*)
fun eval_cardsToString(a, b, c, d, e) = eval_printCard(a)^"-"^eval_printCard(b)^"-"^eval_printCard(c)^"-"^eval_printCard(d)^"-"^eval_printCard(e);