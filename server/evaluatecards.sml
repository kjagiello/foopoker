(*
	printCard n 
	TYPE:		Word32.word -> card
	PRE:		(none)
	POST:		n as a card. 
	EXAMPLE:	printCard(0wx8001B25) = Card ("K", "s"): card
*)
(*
	INFO: 		Converts a binary card into a card. 
*)
fun printCard n =
	let
		val wAnd = Word32.andb
		val r = Word32.>>(n, Word.fromInt(8))
		val r = Word32.toInt(wAnd(r, 0wxF))
	in
		if wAnd(n, 0wx8000) > Word32.fromInt(0) then
			rank(r) ^ "c"
		else if wAnd(n, 0wx4000) > Word32.fromInt(0) then
			rank(r) ^ "d"
		else if wAnd(n, 0wx2000) > Word32.fromInt(0) then
			rank(r) ^ "h"
		else
			rank(r) ^ "s"
	end;
	
(*
	findCard n 
	TYPE:		Word32.word -> Word32.word
	PRE:		(n)
	POST:		A Word32.word. 
	EXAMPLE: 	findCard(0wxFF) = 0wxCCC: Word32.word
*)
(*
	INFO: 		Finds a binary card in the vector tables. 
*)
fun findCard n =
	let
		val wInt = Word.fromInt
		val wPl = Word32.+
		val wXo = Word32.xorb
		val wLeft = Word32.<<
		val wRight = Word32.>>
		val wAnd = Word32.andb
		
		val u = wPl(n, 0wxe91aaa35)
		val u = wXo(u, wRight(u, wInt(16)))
		val u = wPl(u, wLeft(u, wInt(8)))
		val u = wXo(u, wRight(u, wInt(4)))
		
		val b = Word32.toInt(wAnd(wRight(u, wInt(8)), 0wx1ff))
		val a = wRight( wPl (u, wLeft(u, wInt(2)) ), wInt(19))
		val r = wXo(a, Word32.fromInt(adjust(b)))
	in
		r
	end;
	
(*
	eval5Cards (c1, c2, c3, c4, c5)
	TYPE: 		Word32.word * Word32.word * Word32.word * Word32.word * Word32.word -> int
	PRE: 		(none)
	POST: 		An integer. 
	EXAMPLE: 	eval5Cards()	
*)
(*
	INFO: 		Evaluates a hand and returns its hand value. 
*)
fun eval5Cards (c1, c2, c3, c4, c5) =
	let
		val orb = Word32.orb
		val wInt = Word.fromInt
		val w32Int = Word32.fromInt
		val andb = Word32.andb
		val wRight = Word32.>>
		
		val q = Word32.toInt(wRight(orb(orb(orb(orb(c1, c2), c3), c4), c5), wInt(16)))
		val s = unique5(q)
		val p = andb(c1, 0wxFF) * andb(c2, 0wxFF) * andb(c3, 0wxFF) * andb(c4, 0wxFF) * andb(c5, 0wxFF)
	in	
		(*check for Flushes and StraightFlushes*)
		if andb(andb(andb(andb(andb(c1, c2), c3), c4), c5), 0wxF000) > w32Int(0) then 
			flushes(q)
		(*check for Straights and HighCard hands*)
		else if w32Int(s) > w32Int(0) then 
		 	s
		(*check for every other card*)
		else
			values(Word32.toInt(findCard(p)))
	end;

(*
	eval_6hand (c1, c2, c3, c4, c5, c6)
	TYPE:		Word32.word * Word32.word * Word32.word * Word32.word * Word32.word * Word32.word -> int
	PRE:		(none)
	POST:		An integer.
	EXAMPLE: 	eval_6hand(0wx10002C29, 0wx1002817, 0wx4002A1F, 0wx200291D, 0wx408611, 0wx8002B25) = 1
*)
(*
	INFO: 		Evaluates which 5-card hand is the best hand out of 6 cards. 
*)
fun eval_6hand(c1, c2, c3, c4, c5, c6) =
	let
		fun eval_6hand'(_, _, _, _, _, _, 0, best) = best
		| eval_6hand'(c1', c2', c3', c4', c5', c6', n, best) =
			let 
				val perm = (c1', c2', c3', c4', c5', c6', n-1)
			in
				if eval5Cards(perm6(perm)) < best then
					eval_6hand'(c1', c2', c3', c4', c5', c6', n-1, eval5Cards(perm6(perm)))
				else
					eval_6hand'(c1', c2', c3', c4', c5', c6', n-1, best)
			end
	in
		eval_6hand'(c1, c2, c3, c4, c5, c6, 6, 9999)
	end;
	
(*
	eval_7hand (c1, c2, c3, c4, c5, c6, c7)
	TYPE:		Word32.word * Word32.word * Word32.word * Word32.word * Word32.word * Word32.word * Word32.word -> int
	PRE:		(none)
	POST:		An integer.
	EXAMPLE: 	eval_7hand(0wx10002C29, 0wx1002817, 0wx4002A1F, 0wx200291D, 0wx408611, 0wx1001817, 0wx8002B25) = 1
*)
(*
	INFO: 		Evaluates which 5-card hand is the best hand out of 7 cards. 
*)
fun eval_7hand(c1, c2, c3, c4, c5, c6, c7) =
	let
		fun eval_7hand'(_, _, _, _, _, _, _, 0, best) = best
		| eval_7hand'(c1', c2', c3', c4', c5', c6', c7', n, best) =
			let 
				val perm = (c1', c2', c3', c4', c5', c6', c7', n-1)
			in
				if eval5Cards(perm7(perm)) < best then
					eval_7hand'(c1', c2', c3', c4', c5', c6', c7', n-1, eval5Cards(perm7(perm)))
				else
					eval_7hand'(c1', c2', c3', c4', c5', c6', c7', n-1, best)
			end
	in
		eval_7hand'(c1, c2, c3, c4, c5, c6, c7, 21, 9999) 
	end;

(*
	print_eval_7hand
	TYPE: 		Word32.word * Word32.word * Word32.word * Word32.word * Word32.word *
	   			Word32.word * Word32.word -> card * card * card * card * card
	PRE:		(none)
	POST:		Words as cards. 
	EXAMPLE: 	print_eval_7hand(0wx10002C29, 0wx1002817, 0wx4002A1F, 0wx200291D, 0wx408611, 
				0wx1001817, 0wx8002B25) = 
				(Card ("A", "h"), Card ("T", "h"), Card ("Q", "h"), Card ("J", "h"), Card ("K", "h"))
*)
(*
	INFO: 		Prints out which 5-card hand is the best hand out of 7 cards. 
*)
fun print_eval_7hand(c1, c2, c3, c4, c5, c6, c7) =
	let
		fun print_eval_7hand'(_, _, _, _, _, _, _, 0, best, (a, b, c, d, e)) = (printCard(a), printCard(b), printCard(c), printCard(d), printCard(e))
		| print_eval_7hand'(c1', c2', c3', c4', c5', c6', c7', n, best, bestHand) =
			let 
				val perm = (c1', c2', c3', c4', c5', c6', c7', n-1)
			in
				if eval5Cards(perm7(perm)) < best then
					print_eval_7hand'(c1', c2', c3', c4', c5', c6', c7', n-1, eval5Cards(perm7(perm)), perm7(perm))
				else
					print_eval_7hand'(c1', c2', c3', c4', c5', c6', c7', n-1, best, bestHand)
			end
	in
		print_eval_7hand'(c1, c2, c3, c4, c5, c6, c7, 21, 9999, (0wx0, 0wx0, 0wx0, 0wx0, 0wx0))
	end;