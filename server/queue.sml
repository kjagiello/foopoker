(* 
	REPRESENTATION CONVENTION: 	the term Queue([x1,x2,...,xn],[y1,y2,...,ym])
     							represents the queue x1,x2,...,xn,ym,...,y2,y1 
								with head x1 and tail y1
   REPRESENTATION INVARIANT: 	a non-empty queue is never represented
     							by Queue([],[y1,y2,...,ym])
*)
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
		shuffleDeck n
		TYPE: 		'a list -> 'a queue
		PRE: 		(none)
		POST:		A queue n. 
		EXAMPLE:	shuffleDeck([1,2,3]) = Queue([1], [2,3]);
	*)
	(*
		INFO: 		Makes a queue out of a list. For use in the shuffled deck. 
	*)
	fun shuffleDeck(x::xs) = Queue([x], xs);
end