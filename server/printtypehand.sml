(*
	dealerRank n
	TYPE:		int -> string
	PRE:		0 <= n <= 12
	POST:		n as a string. 
	EXAMPLE: 	dealerRank(12) = "Ace"
*)
(*
	INFO: 		Returns the value of a card in singular. 
*)
fun dealerRank n =
	let	
		val cards = ["Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King", "Ace"]
		val cardList = Vector.fromList(cards)
	in
		Vector.sub(cardList, n)
	end;
(*
	dealerRanks n
	TYPE:		int -> string
	PRE:		0 <= n <= 12
	POST:		n as a string. 
	EXAMPLE: 	dealerRank(12) = "Aces"
*)
(*
	INFO: 		Returns the value of a card in plural. 
*)	
fun dealerRanks n =
	let	
		val cards = ["Twos", "Threes", "Fours", "Fives", "Sixes", "Sevens", "Eights", "Nines", "Tens", "Jacks", "Queens", "Kings", "Aces"]
		val cardList = Vector.fromList(cards)
	in
		Vector.sub(cardList, n)
	end;
(*
	checkHighCard l
	TYPE: 		int list -> string
	PRE:		(none)
	POST: 		l represented as a string.
	EXAMPLE: 	checkHighCard([0,1,2,3,4]) = "Six"
*)
(*
	INFO:		Check which card in the type HIGH_CARD is the highest.
*)
fun checkHighCard([]) = ""
| checkHighCard(x::rest) = dealerRank(foldr Int.max x rest);

(*
	checkTwoPair l
	TYPE: 		int list -> string
	PRE:		(none)
	POST: 		l represented as a string.
	EXAMPLE: 	checkTwoPair([1,1,2,2,4]) = "Threes and Fours"
*)
(*
	INFO:		Checks the values of TWO_PAIR
*)
fun checkTwoPair([]) = ""		
| checkTwoPair(x::y::z::rest) = 
	let 
		fun checkTwoPair'([], pair1, pair2) = dealerRanks(pair1)^" and "^dealerRanks(pair2)
		| checkTwoPair'(x::rest, pair1, pair2) =
			if x = pair1 then
				checkTwoPair'(rest, x, pair2)
			else if x = pair2 then
				checkTwoPair'(rest, pair1, x)
			else
				checkTwoPair'(rest, pair1, pair2)
	in
		if x = y then
			checkTwoPair'(rest, y, z)
		else
			checkTwoPair'(z::rest, x, y)
	end;

(*
	checkPairThreeFour l
	TYPE: 		int list -> string
	PRE:		(none)
	POST: 		l represented as a string.
	EXAMPLE: 	checkPairThreeFour([1,1,1,2,4]) = "Deuces"
*)
(*
	INFO:		Checks the values of ONE_PAIR/THREE_OF_A_KIND/FOUR_OF_A_KIND
*)	
fun checkPairThreeFour([]) = ""	
| checkPairThreeFour(x::rest) = 
		let 
			fun checkPairThreeFour'([], _) = ""
			| checkPairThreeFour'(x::rest, ptf) =
				if x = ptf then
					dealerRanks(ptf)
				else
					checkPairThreeFour'(rest, x)
		in
			checkPairThreeFour'(rest, x)
		end;
		
(*
	checkStraight l
	TYPE: 		int list -> string
	PRE:		(none)
	POST: 		l represented as a string.
	EXAMPLE: 	checkStraight([1,1,1,2,4]) = "Deuces"
*)
(*
	INFO:		Check the highest and lowest card in STRAIGHT
*)		
fun checkStraight([]) = ""		
| checkStraight(x::rest) = 
		let 
			fun checkStraight'([], min, max) = dealerRank(min)^ " to " ^dealerRank(max)
			| checkStraight'(x::rest, min, max) =
				if x < min then
					checkStraight'(rest, x, max)
				else if x > max then
					checkStraight'(rest, min, x)
				else
					checkStraight'(rest, min, max)
		in
			checkStraight'(rest, x, x)
		end;
(*
	checkFlush w
	TYPE: 		Word32.word -> string
	PRE:		(none)
	POST: 		w represented as a string.
	EXAMPLE: 	checkFlush(0wx8008B25) = "Clubs"
*)
(*
	INFO:		Check the suit of FLUSH
*)		
fun checkFlush(flush) =		
	let
		val op andb = Word32.andb
		val w = Word32.fromInt(0)
	in
		if andb(flush, 0wx8000) > w then
			"Clubs"
		else if andb(flush, 0wx4000) > w then
			"Diamonds"
		else if andb(flush, 0wx2000) > w then
			"Hearts"
		else
			"Spades"
	end;
(*
	checkFull l
	TYPE: 		int list -> string
	PRE:		(none)
	POST: 		l represented as a string.
	EXAMPLE: 	checkFull([1,1,1,2,2]) = "Pairs over Deuces"
*)
(*
	INFO:		Check which values of FULL_HOUSE
*)
fun checkFull([]) = ""		
| checkFull(x::rest) =
		let 
			fun checkFull'([], cards1, cards2) = 
				if length cards1 = 3 then
					dealerRanks(hd(cards1))^ " over " ^dealerRanks(hd(cards2))
				else
					dealerRanks(hd(cards2))^ " over " ^dealerRanks(hd(cards1))
			| checkFull'(x::rest, cards1, cards2) =
				if x = hd(cards1) then
					checkFull'(rest, x::cards1, cards2)
				else
					checkFull'(rest, cards1, x::cards2)
		in
			checkFull'(rest, [x], [])
		end;
(*
	printTypeHand(c1, c2, c3, c4, c5)
	TYPE: 		Word32.word * Word32.word * Word32.word * Word32.word * Word32.word -> string
	PRE:		(none)
	POST: 		c1, c2, c3, c4, c5 as a string. 
	EXAMPLE: 	printTypeHand(0wx8002B25, 0wx8008B25, 0wx8001B25, 0wx1002817, 0wx802713) = "Kings"
*)		
(*
	INFO: 		Supplements the function printHand(n) with the types of the dominant cards. 
*)
fun printTypeHand(c1, c2, c3, c4, c5) =
	let 
		val n = eval5Cards(c1, c2, c3, c4, c5)
		val flush = c1
		
		val op andb = Word32.andb
		val w = Word.fromInt(8)
		val z = 0wxF
		val toInt32 = Word32.toInt
		val op >> = Word32.>>
		
		val c1 = toInt32(andb(>>(c1, w), z))
		val c2 = toInt32(andb(>>(c2, w), z))
		val c3 = toInt32(andb(>>(c3, w), z))
		val c4 = toInt32(andb(>>(c4, w), z))
		val c5 = toInt32(andb(>>(c5, w), z))
		val hand = [c1, c2, c3, c4, c5]
	in
		if n > 6185 then				(*HIGH_CARD*)
			checkHighCard(hand) 
		else if n > 3325 then			(*ONE_PAIR*)
			checkPairThreeFour(hand)
		else if n > 2467 then			(*TWO_PAIR*)
			checkTwoPair(hand)
		else if n > 1609 then			(*THREE_OF_A_KIND*)
			checkPairThreeFour(hand)
		else if n > 1599 then			(*STRAIGHT*)
			checkStraight(hand)
		else if n > 322 then			(*FLUSH*)
			checkFlush(flush)
		else if n > 166 then			(*FULL_HOUSE*)
			checkFull(hand)	
		else if n > 10 then				(*FOUR_OF_A_KIND*)
			checkPairThreeFour(hand)	
		else if n > 1 then				(*STRAIGHT_FLUSH*)
			checkStraight(hand)^" of "^checkFlush(flush)
		else							(*ROYAL*)
			checkFlush(flush)
	end;