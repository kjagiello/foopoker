(*
	REPRESENTATION CONVENTION: 	A play card represented as a 32-bit binary:
		
								+--------+--------+--------+--------+
								|xxxbbbbb|bbbbbbbb|cdhsrrrr|xxpppppp|
								+--------+--------+--------+--------+

								p = prime number of rank (deuce=2,trey=3,four=5,...,ace=41)
								r = rank of card (deuce=0,trey=1,four=2,five=3,...,ace=12)
								cdhs = suit of card (bit turned on based on suit of card)
								b = bit turned on depending on rank of card

								Using such a scheme, here are some bit pattern examples:

								xxxAKQJT 98765432 CDHSrrrr xxPPPPPP
								00001000 00000000 01001011 00100101    King of Diamonds
								00000000 00001000 00010011 00000111    Five of Spades
								00000010 00000000 10001001 00011101    Jack of Clubs
	REPRESENTATION INVARIANT: 	(none)
*)
datatype playcard = Playcard of Word32.word;

(*
	REPRESENTATION CONVENTION: 	A hand represented as 5 playcards which get different
								rank for special combinations all according to
								the rules of poker. 
								
								ROYAL: Royal straight flush
								STRAIGHT_FLUSH: Straight flush
								FOUR_OF_A_KIND: Four of kind
								FULL_HOUSE: Full house
								FLUSH: Flush
								STRAIGHT: Straight
								THREE_OF_A_KIND: Three of a kind
								TWO_PAIR: Two pair
								ONE_PAIR: One pair
								HIGH_CARD: A high card
	REPRESENTATION INVARIANT: 	(none)
*)
datatype hand = ROYAL | STRAIGHT_FLUSH | FOUR_OF_A_KIND | FULL_HOUSE | FLUSH | STRAIGHT | THREE_OF_A_KIND | TWO_PAIR | ONE_PAIR | HIGH_CARD;

(*
	REPRESENTATION CONVENTION: 	The suit of a play card.
								CLUB: Clubs
								DIAMOND: Diamonds
								HEART: Hearts
								SPADE: Spades
	REPRESENTATION INVARIANT: 	(none)
*)
datatype cardsuit = CLUB | DIAMOND | HEART | SPADE;

(*
	REPRESENTATION CONVENTION: 	The values of a play card.
								Deuce: 2 (two)
								Trey: 3 (three)
								Four: 4 (four)
								Five: 5 (five)
								Six: 6 (six)
								Seven: 7 (seven)
								Eight: 8 (eight)
								Nine: 9 (nine)
								Ten: 10 (ten)
								Jack: J (jack)
								Queen: Q (queen)
								King: K (king)
								Ace: A (ace)
								Joker: (joker)
	REPRESENTATION INVARIANT: 	(none)
*)
datatype cardvalue = Deuce | Trey | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace | Joker;

(*
	REPRESENTATION CONVENTION: 	A hand value.
	REPRESENTATION INVARIANT: 	(none)
*)
datatype handvalue = Handvalue of int;
(*
	handValue h 
	TYPE: 		hand -> int
	PRE: 		(none)
	POST: 		h as an integer from 1-10.
	EXAMPLE: 	handValue(STRAIGHT_FLUSH) = 1; 
*)
fun handToInt 	ROYAL = 1
	| handToInt STRAIGHT_FLUSH = 2
	| handToInt	FOUR_OF_A_KIND = 3 
	| handToInt	FULL_HOUSE = 4
	| handToInt	FLUSH = 5
	| handToInt	STRAIGHT = 6 
	| handToInt	THREE_OF_A_KIND = 7 
	| handToInt	TWO_PAIR = 8
	| handToInt	ONE_PAIR = 9 
	| handToInt	HIGH_CARD = 10;
	
(*
	handPrint n 
	TYPE: 		int -> string
	PRE: 		0 <= n <= 10
	POST: 		n represented as a poker hand. 
	EXAMPLE: 	handPrint(1) = "Straight Flush"
*)	
fun handPrint n = 
	let
		val hands = ["", "Royal Straight Flush", "Straight Flush", "Four of a Kind", "Full House", "Flush", "Straight", "Three of a Kind", "Two Pair", "One Pair", "High Card"]
		val handList = Vector.fromList(hands)
	in
		Vector.sub(handList, n)
	end;
	
(*
	handFrHandvalue n
	TYPE:		handvalue -> hand
	PRE:		n > 0 
	POST:		n as a hand. 
	EXAMPLE: 	handFrHandvalue(6186) = HIGH_CARD: hand
*)
fun handFrHandvalue(Handvalue(n)) = 
	if n > 6185 then
		HIGH_CARD
	else if n > 3325 then
		ONE_PAIR
	else if n > 2467 then
		TWO_PAIR
	else if n > 1609 then
		THREE_OF_A_KIND
	else if n > 1599 then
		STRAIGHT
	else if n > 322 then
		FLUSH
	else if n > 166 then
		FULL_HOUSE
	else if n > 10 then
		FOUR_OF_A_KIND
	else if n > 1 then
		STRAIGHT_FLUSH
	else
		ROYAL;
(*
	cardValueToInt c
	TYPE:		cardvalue -> int
	PRE:		(none)
	POST:		c as an integer between 0-13. 
	EXAMPLE: 	cardValueToInt(Ace) = 12: int
*)
fun cardValueToInt 	Deuce = 0
	| cardValueToInt	Trey = 1
	| cardValueToInt	Four = 2 
	| cardValueToInt	Five = 3 
	| cardValueToInt	Six = 4
	| cardValueToInt	Seven = 5 
	| cardValueToInt	Eight = 6 
	| cardValueToInt	Nine = 7
	| cardValueToInt	Ten = 8
	| cardValueToInt Jack = 9
	| cardValueToInt Queen = 10
	| cardValueToInt King = 11
	| cardValueToInt Ace = 12
	| cardValueToInt Joker = 13;	


(*
	getCardValue n
	TYPE:		int -> cardvalue
	PRE:		0 <= n <= 13
	POST:		n as a cardvalue. 
	EXAMPLE: 	getCardValue(12) = Ace: cardvalue
*)
fun getCardValue n =
	let	
		val values = [Deuce, Trey, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace, Joker]
		val valueList = Vector.fromList(values)
	in
		Vector.sub(valueList, n)
	end;

(*
	handValueToInt n
	TYPE: 		handvalue -> int
	PRE: 		(none)
	POST: 		h as an int. 
	EXAMPLE: 	handValueToInt(Handvalue(5)) = 5: int
*)
fun handValueToInt(Handvalue(n)) = n; 
	
(*
	dealerCardValue n
	TYPE:		cardvalue -> string
	PRE:		(none)
	POST:		Representation of n.  
	EXAMPLE: 	dealerCardValue(Ace) = "Ace": string
*)
fun dealerCardValue n =
	case n of 
		Deuce => "Two"
		| Trey => "Three"
		| Four => "Four"
		| Five => "Five"
		| Six => "Six"
		| Seven => "Seven"
		| Eight => "Eight"
		| Nine => "Nine"
		| Ten => "Ten"
		| Jack => "Jack"
		| Queen => "Queen"
		| King => "King"
		| Ace => "Ace"
		| _ => "";
(*
	dealerCardValues n
	TYPE:		cardvalue -> string
	PRE:		(none)
	POST:		Representation of n. 
	EXAMPLE: 	dealerCardValues(Ace) = "Aces": string
*)
fun dealerCardValues n =
	case n of 
		Deuce => "Twos"
		| Trey => "Threes"
		| Four => "Fours"
		| Five => "Fives"
		| Six => "Sixes"
		| Seven => "Sevens"
		| Eight => "Eights"
		| Nine => "Nines"
		| Ten => "Tens"
		| Jack => "Jacks"
		| Queen => "Queens"
		| King => "Kings"
		| Ace => "Aces"
		| _ => "";

(*
	dealerCardSuit n
	TYPE:		cardsuit -> string
	PRE:		(none)
	POST:		Representation of n.  
	EXAMPLE: 	dealerCardSuit(SPADE) = "Spades": string
*)
fun dealerCardSuit n =
	case n of 
		CLUB => "Clubs"
		| DIAMOND => "Diamonds"
		| HEART => "Hearts"
		| SPADE => "Spades";

(*
	dealerStraight n m
	TYPE:		cardvalue * cardvalue -> string
	PRE:		(none)
	POST:		Representation of n and m as STRAIGHT.
	EXAMPLE: 	dealerStraight((Deuce, Six)) = "Two to Six": string
*)
fun dealerStraight (v, s) = 
	dealerCardValue(v) ^ " to " ^ dealerCardValue(s);

(*
	dealerFullHouse n m
	TYPE:		cardvalue * cardvalue -> string
	PRE:		(none)
	POST:		Representation of n and m as FULL_HOUSE.
	EXAMPLE: 	dealerFullHouse((Deuce, Six)) = "Twos over Sixes": string
*)
fun dealerFullHouse (v, s) = 
	dealerCardValues(v) ^ " over " ^ dealerCardValues(s);

(*
	dealerTwoPair n m
	TYPE:		cardvalue * cardvalue -> string
	PRE:		(none)
	POST:		Representation of n and m as TWO_PAIR. 
	EXAMPLE: 	dealerTwoPair((Deuce, Six)) = "Twos and Sixes": string
*)
fun dealerTwoPair (v, s) = 
	dealerCardValues(v) ^ " and " ^ dealerCardValues(s);
	
(* 
	Raise (r, playersMoney, bigBlind)
   	TYPE: 		int * int * int -> int * int
   	PRE: 		r, playersMoney, bigBlind >= 0.
   	POST: 		En tupel med totalsumman totalSum och den minsta
	 			tillåtna satsningen leastAcceptableRaise.
   	EXAMPLE: 	Raise (100, 50, 20) = Exception
	    		Raise (10, 50, 20) = Exception
	    		Raise (30, 50 ,20) = (50, 30)
*)

fun Raise (raiseWith, playersMoney, bigBlind) =
    let
		exception notEnoughMoney;
		exception toSmallRaise;
		val totalSum = raiseWith + bigBlind;
        val leastAcceptableRaise = raiseWith
    in
		if (raiseWith > playersMoney) then
	    	raise notEnoughMoney
		else if (raiseWith < bigBlind) then
	    	raise toSmallRaise
		else 
			(totalSum, leastAcceptableRaise)
    end;

(* 
	Call (call, playerMoney)
   	TYPE: 		int * int -> int
   	PRE: 		call, playerMoney >= 0
   	POST: 		Ger call efter jämförelse med playerMoney.
   	EXAMPLE: 	Call (60, 50) = Exception
	    		Call (40, 50) = 40
*)
fun Call (call, playerMoney) = 
    let
		exception notEnoughMoney;
    in
		if (call > playerMoney) then
	    	raise notEnoughMoney
		else 
			call
    end;

use "vectors.sml";
use "shuffledeck.sml";
use "evaluatecards.sml";
use "printtypehand.sml";
use "showdown.sml";