(*
	checkHighCard l
	TYPE: 		int list -> cardvalue
	PRE:		(none)
	POST: 		The max integer in l as a cardvalue. 
	EXAMPLE: 	checkHighCard([0,1,2,3,4]) = Six: cardvalue
*)
fun checkHighCard([]) = Joker
| checkHighCard(x::rest) = 
		getCardValue(foldr Int.max x rest);

(*
	checkTwoPair l
	TYPE: 		int list -> cardvalue * cardvalue
	PRE:		(none)
	POST: 		A tupel of two cardvalues where a cardvalue is two of the same integer in l. 
	EXAMPLE: 	checkTwoPair([1,1,2,2,4]) = (Trey, Four): cardvalue * cardvalue
*)
fun checkTwoPair([]) = (Joker, Joker)	
| checkTwoPair(x::y::z::rest) = 
	let 
		fun checkTwoPair'([], pair1, pair2) = (getCardValue(pair1), getCardValue(pair2))
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
	TYPE: 		int list -> cardvalue
	PRE:		(none)
	POST: 		A cardvalue which is the integer in l that 
				occurs more than 1 time. 
	EXAMPLE: 	checkPairThreeFour([1,1,1,2,4]) = Trey: cardvalue
*)
fun checkPairThreeFour([]) = Joker
| checkPairThreeFour(x::rest) = 
		let 
			fun checkPairThreeFour'([], _, list) = checkPairThreeFour(list)
			| checkPairThreeFour'(x::rest, ptf, list) =
				if x = ptf then
					getCardValue(ptf)
				else
					checkPairThreeFour'(rest, ptf, list)
		in
			checkPairThreeFour'(rest, x, rest)
		end;	
(*
	checkStraight l
	TYPE: 		int list -> cardvalue * cardvalue
	PRE:		(none)
	POST: 		A tupel of two card values, where the first is the lowest
				integer in l and the second is the highest integer. 
	EXAMPLE: 	checkStraight([1,2,3,4,5]) = (Trey, Seven): cardvalue * cardvalue
*)
fun checkStraight([]) = (Joker, Joker)	
| checkStraight(x::rest) = 
		let 
			fun checkStraight'([], min, max) = (getCardValue(min), getCardValue(max))
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
	TYPE: 		Word32.word -> cardsuit
	PRE:		(none)
	POST: 		w represented as a cardsuit.
	EXAMPLE: 	checkFlush(0wx8008B25) = CLUB: cardsuit
*)	
fun checkFlush(flush) =		
	let
		val op andb = Word32.andb
		val w = Word32.fromInt(0)
	in
		if andb(flush, 0wx8000) > w then
			CLUB
		else if andb(flush, 0wx4000) > w then
			DIAMOND
		else if andb(flush, 0wx2000) > w then
			HEART
		else
			SPADE
	end;
(*
	checkFull l
	TYPE: 		int list -> cardvalue * cardvalue
	PRE:		(none)
	POST: 		A tupel of two cardvalues where the first cardvalue
				occurs 3 times as an integer in l and the second
				cardvalue occurs 2 times. 
	EXAMPLE: 	checkFull([1,1,1,2,2]) = (Trey, Four): cardvalue * cardvalue
*)
fun checkFull([]) = (Joker, Joker)	
| checkFull(x::rest) =
		let 
			fun checkFull'([], cards1, cards2) = 
				if length cards1 = 3 then
					(getCardValue(hd(cards1)), getCardValue(hd(cards2)))
				else
					(getCardValue(hd(cards2)), getCardValue(hd(cards1)))
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
	TYPE: 		card * card * card * card * card -> string
	PRE:		(none)
	POST: 		c1, c2, c3, c4, c5 as a string. 
	EXAMPLE: 	printTypeHand(Card 0wx8002B25, Card 0wx8008B25, Card 0wx8001B25, Card 0wx1002817, Card 0wx802713) = "Kings": string
*)		
fun printTypeHand(Playcard c1, Playcard c2, Playcard c3, Playcard c4, Playcard c5) =
	let 
		val n = handValueToInt(eval_5cards(Playcard c1, Playcard c2, Playcard c3, Playcard c4, Playcard c5))
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
			dealerCardValue(checkHighCard(hand)) 
		else if n > 3325 then			(*ONE_PAIR*)
			dealerCardValues(checkPairThreeFour(hand))
		else if n > 2467 then			(*TWO_PAIR*)
			dealerTwoPair(checkTwoPair(hand))
		else if n > 1609 then			(*THREE_OF_A_KIND*)
			dealerCardValues(checkPairThreeFour(hand))
		else if n > 1599 then			(*STRAIGHT*)
			dealerStraight(checkStraight(hand))
		else if n > 322 then			(*FLUSH*)
			dealerCardSuit(checkFlush(flush))
		else if n > 166 then			(*FULL_HOUSE*)
			dealerFullHouse(checkFull(hand))	
		else if n > 10 then				(*FOUR_OF_A_KIND*)
			dealerCardValues(checkPairThreeFour(hand))	
		else if n > 1 then				(*STRAIGHT_FLUSH*)
			dealerStraight(checkStraight(hand)) ^ dealerCardSuit(checkFlush(flush))
		else							(*ROYAL*)
			dealerCardSuit(checkFlush(flush))
	end;