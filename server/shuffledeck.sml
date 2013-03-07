datatype card = Card of Word32.word;(* 
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
		shuffleDeck ()
		TYPE: 		unit -> playcard queue
		PRE:		(none)
		POST:		A card queue with 52 elements. 
		EXAMPLE:	shuffleDeck(52) = Queue([0wx18002], [0wx28103, 0wx48205, 0wx88307, 
					0wx10840B, 0wx20850D, 0wx408611, 0wx808713, 0wx1008817, ...]): card queue
	*)
	fun shuffleDeck () = 
		let
			
			(*
				getPrime c
				TYPE:		int -> int
				PRE:		0 <= c <= 12
				POST:		A primenumber[i] depending on c[i].
				EXAMPLE:	getPrime(5) = 7: int
			*)
			fun getPrime(c) = 
			 	let
					val primes = [2,3,5,7,11,13,17,19,23,29,31,37,41]
					val cardList = Vector.fromList(primes)
				in
					Vector.sub(cardList, c)
				end;

			(*	
				bitDeck () 
				TYPE: 		unit -> playcard list
				PRE: 		(none)
				POST: 		A card list with 52 elements. 
				EXAMPLE: 	bitDeck() = [0wx18002, 0wx28103, 0wx48205, 0wx88307, 0wx10840B, 0wx20850D, 0wx408611,
				    		0wx808713, 0wx1008817, 0wx200891D, ...]: card list
			*)
			fun bitDeck () = 
				let 
					val orb = Word32.orb
					val frInt32 = Word32.fromInt
					val wInt = Word.fromInt
					val op << = Word32.<<
					val op >> = Word32.>>
					val suit = 0wx8000
					
					(*
						bitDeck' n, j, s
						TYPE:		int * int * Word32.word -> playcard list
						PRE:		n > 0, 0 <= j < 12, s = 0wx8000 OR 0wx4000 OR 0wx2000 OR 0wx1000
						POST:		A card list with n elements, where j is the number of values for each suit s. 
						EXAMPLE: 	bitDeck(52, 0, 0wx8000) = [0wx18002, 0wx28103, 0wx48205, 
									0wx88307, 0wx10840B, 0wx20850D, 0wx408611, 0wx808713, 0wx1008817, 0
									wx200891D, ...]: card list
					*)
					fun bitDeck'(0,_,_) = []
					| bitDeck' (n', j, suit) = 	
						if j < 12 then				(*Populate a card[0-12] with the form above*)
							Playcard (orb(orb(orb(frInt32(getPrime(j)), <<(frInt32(j), wInt(8))), suit), <<(frInt32(1), wInt(16+j))))::bitDeck'(n'-1, j+1, suit)
						else						(*Change suit*)
							Playcard (orb(orb(orb(frInt32(getPrime(j)), <<(frInt32(j), wInt(8))), suit), <<(frInt32(1), wInt(16+j)))) ::bitDeck'(n'-1, 0, >>(suit, Word.fromInt(1)))	
				in
					bitDeck' (52, 0, suit)
				end;
			(*
				makeQDeck n
				TYPE: 		'a list -> 'a queue
				PRE: 		(none)
				POST:		n as a queue. 
				EXAMPLE:	makeQDeck([1,2,3]) = Queue([1], [2,3]);
			*)
			fun makeQDeck(x::xs) = Queue([x], xs);
			
			val shuffledeck = shuffle(bitDeck())
		in
			makeQDeck(shuffledeck)
		end;
		
		
end;