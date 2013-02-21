(* 
	REPRESENTATION CONVENTION: 	the term Queue([x1,x2,...,xn],[y1,y2,...,ym])
     							represents the queue x1,x2,...,xn,ym,...,y2,y1 
								with head x1 and tail y1
   REPRESENTATION INVARIANT: 	a non-empty queue is never represented
     							by Queue([],[y1,y2,...,ym])
*)
use "shuffle.sml";
abstype 'a queue = Queue of 'a list * 'a list
	with

	(* 
		empty
	   	TYPE:  	'a queue
	   	VALUE: 	the empty queue
	*)
	val empty = Queue([],[])

	(* 
		isEmpty Q
	   	TYPE: 		''a queue -> bool
	   	PRE:  		(none)
	   	POST: 		true if Q is empty, and false otherwise
	*)
	(* TIME COMPLEXITY: Theta(1) always *)
	fun isEmpty Q = (Q = Queue([],[]))

	(* 	
		head Q
	   	TYPE: 		'a queue -> 'a
	   	PRE:  		Q is nonempty
	   	POST: 		the head element of Q
	*)
	(* TIME COMPLEXITY: Theta(1) always *)
	fun head (Queue(h::xs,ys)) = h

	(* 	normalise Q
	   	TYPE: 		'a queue -> 'a queue
	   	PRE:  		(none)
	   	POST: 		if Q is of the form Queue([],[y1,y2,...,ym]),
	         		then Queue([ym,...,y2,y1],[]),
	         		else Q
	*)
	(* TIME COMPLEXITY: Theta(|Q|) at worst *)
	fun normalise (Queue([],ys)) = Queue(rev ys,[])
	  | normalise Q = Q

	(* 
		enqueue (Q, t)
	   	TYPE: 		'a queue * 'a -> 'a queue
	   	PRE:  		(none)
	   	POST: 		the queue Q with t added as new tail element
	*)
	(* TIME COMPLEXITY: Theta(1) always *)
	fun enqueue (Queue(xs,ys), t) = normalise (Queue(xs,t::ys))

	(* 
		dequeue Q
	   	TYPE: 		'a queue -> 'a queue
	   	PRE:  		Q is nonempty
	   	POST: 		Q without its head element
	*)
	(* TIME COMPLEXITY: Theta(1) on average *)
	fun dequeue (Queue(h::xs,ys)) = normalise (Queue(xs,ys))
	
	(*	
		bitDeck n 
		TYPE: 		int -> Word32.word list
		PRE: 		n = 52
		POST: 		A Word32.word list. 
		EXAMPLE: 	bitDeck(52) = [0wx2, 0wx3, 0wx5, 0wx7, 0wxB, 0wxD, 0wx11, 0wx13, 0wx17, 0wx1D, ...]:
		   			Word32.word list
	*)
	(*
		INFO: 		Returns a list of 52 cards which are represented by words. 

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
	*)
	fun getPrime(c) = 
	 	let
			val primes = [2,3,5,7,11,13,17,19,23,29,31,37,41]
			val cardList = Vector.fromList(primes)
		in
			Vector.sub(cardList, c)
		end;
	fun bitDeck 0 = []
	| bitDeck n = 
		let 
			val orb = Word32.orb
			val w32Int = Word32.fromInt
			val wInt = Word.fromInt
			val wLeft = Word32.<<
			val wRight = Word32.>>
			val suit = 0wx8000

			fun bitDeck' (0, _, _) = []
			 | bitDeck' (n', j, suit) = 	
				if j < 12 then				(*Populate a card[0-12] with the form above*)
					orb(orb(orb(w32Int(getPrime(j)), wLeft(w32Int(j), wInt(8))), suit), wLeft(w32Int(1), wInt(16+j)))::bitDeck'(n'-1, j+1, suit)
				else						(*Change suit*)
					orb(orb(orb(w32Int(getPrime(j)), wLeft(w32Int(j), wInt(8))), suit), wLeft(w32Int(1), wInt(16+j)))::bitDeck'(n'-1, 0, wRight(suit, Word.fromInt(1)))	
		in
			bitDeck' (n, 0, suit)
		end;
	
	(*
		makeQDeck n
		TYPE: 		'a list -> 'a queue
		PRE: 		(none)
		POST:		A queue n. 
		EXAMPLE:	makeQDeck([1,2,3]) = Queue([1], [2,3]);
	*)
	(*
		INFO: 		Makes a queue out of a list. For use in the shuffled deck. 
	*)
	fun makeQDeck(x::xs) = Queue([x], xs);
	
	(*
		shuffleDeck n
		TYPE: 		int -> Word32.word queue
		PRE:		n >= 0
		POST:		A Word32.word queue. 
		EXAMPLE:	shuffleDeck(52) = Queue([0wx18002], [0wx28103, 0wx48205, 0wx88307, 
					0wx10840B, 0wx20850D, 0wx408611, 0wx808713, 0wx1008817, ...]): Word32.word queue
	*)
	fun shuffleDeck n = 
		let
			val shuffledeck = shuffle(bitDeck(n))
		in
			makeQDeck(shuffledeck)
		end;
end;