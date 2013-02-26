(*
	printCard n 
	TYPE:		Word32.word -> card
	PRE:		(none)
	POST:		n as a card. 
	EXAMPLE:	printCard(0wx8001B25) = Card ("K", "s"): card
*)
(*
	INFO: 		***Cactus Kev's Poker Hand Evaluator***
				Converts a binary card into a card. 
*)
fun printCard n =
	let
		val op andb = Word32.andb
		val r = Word32.>>(n, Word.fromInt(8))
		val r = Word32.toInt(andb(r, 0wxF))
		val w = Word32.fromInt(0)
	in
		if andb(n, 0wx8000) > w then
			rank(r) ^ "c"
		else if andb(n, 0wx4000) > w then
			rank(r) ^ "d"
		else if andb(n, 0wx2000) > w then
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
	INFO: 		***Paul D. Senzee's Optimized Hand Evaluator
				for Cactus Kev's Poker Hand Evaluator***
				
				Finds a binary card in the vector tables. 
*)
fun findCard n =
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
		val r = xorb(a, Word32.fromInt(adjust(b)))
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
	INFO: 		***Paul D. Senzee's Optimized Hand Evaluator
				for Cactus Kev's Poker Hand Evaluator***
				
				Evaluates a hand and returns its hand value. 
*)
fun eval5Cards (c1, c2, c3, c4, c5) =
	let
		val orb = Word32.orb
		val frInt = Word.fromInt
		val frInt32 = Word32.fromInt
		val toInt32 = Word32.toInt
		val w = Word32.fromInt(0)
		val andb = Word32.andb
		val op >> = Word32.>>
		
		(*
		1. 00010000000000000010110000101001
		2. 00001000000000000010101100100101
		3. 00000000010000000000001010100001
		4. 00000010000000000010100100011101
		5. 00000001000000000010100000010111
		
		1: 00010000000000000010110000101001
		   00001000000000000010101100100101
		   00011000000000000010111100100101
		2. 00011000000000000010111100100101
		   00000000010000000000001010100001
		   00011000010000000010111110100101
		3. 00011000010000000010111110100101
		   00000010000000000010100100011101
		   00011010010000000010111110111101
		4. 00011010010000000010111110111101
		   00000001000000000010100000010111
		   00011011010000000010111110111111
		5. 0001101101000000
		6. 0001101101000000 = 6976
		*)
		
		val q = Word32.toInt(>>(orb(orb(orb(orb(c1, c2), c3), c4), c5), frInt(16)))
		val s = unique5(q)
		val p = andb(c1, 0wxFF) * andb(c2, 0wxFF) * andb(c3, 0wxFF) * andb(c4, 0wxFF) * andb(c5, 0wxFF)
	in	
		(*check for Flushes and StraightFlushes*)
		if andb(andb(andb(andb(andb(c1, c2), c3), c4), c5), 0wxF000) > w then 
			flushes(q)
		(*check for Straights and HighCard hands*)
		else if frInt32(s) > w then 
		 	s
		(*check for every other card*)
		else
			values(toInt32(findCard(p)))
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
	INFO: 		***Cactus Kev's Poker Hand Evaluator***
				Evaluates which 5-card hand is the best hand out of 7 cards. 
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
		fun print_eval_7hand'(_, _, _, _, _, _, _, 0, best, (a, b, c, d, e)) = (a, b, c, d, e)
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

fun handToString(a, b, c, d, e) = printCard(a)^"-"^printCard(b)^"-"^printCard(c)^"-"^printCard(d)^"-"^printCard(e);