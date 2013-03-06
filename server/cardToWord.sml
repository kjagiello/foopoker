(*
xxxAKQJT 98765432 CDHSrrrr xxPPPPPP
00000000 00000000 00000000 00000000
2  3  4  5  6   7   8   9   T   J   Q   K   A
2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41
0, 1, 2, 3,  4,  5,  6,  7,  8,  9, 10, 11, 12
100000010111
*)
fun whichCard v = 
	case v of 
		  "2" => "0000000000000001"
		| "3" => "0000000000000010"
		| "4" => "0000000000000100"
		| "5" => "0000000000001000"
		| "6" => "0000000000010000"
		| "7" => "0000000000100000"
		| "8" => "0000000001000000"
		| "9" => "0000000010000000"
		| "T" => "0000000100000000"
		| "J" => "0000001000000000"
		| "Q" => "0000010000000000"
		| "K" => "0000100000000000"
		| "A" => "0001000000000000"
		| _   => "0000000000000000";

fun whichSuit c = 
	case c of
		  "c" => "1000"
		| "d" => "0100"
		| "h" => "0010"
		| "s" => "0001"
		| _   => "0000";

fun whichScore v = 
	case v of
		  "2" => "000000000010"
		| "3" => "000100000011"
		| "4" => "001000000101"
		| "5" => "001100000111"
		| "6" => "010000001011"
		| "7" => "010100001101"
		| "8" => "011000010001"
		| "9" => "011100010011"
		| "T" => "100000010111"
		| "J" => "100100011101"
		| "Q" => "101000011111"
		| "K" => "101100100101"
		| "A" => "110000101001"
		| _   => "000000000000";
		
fun c2bin(c) = 
	let 
		val v = String.substring(c, 0, 1)
		val s = String.substring(c, 1, 1)
	in
		whichCard(v)^whichSuit(s)^whichScore(v)
	end;
	

fun orbList l =
    foldr Word32.orb (Word32.fromInt 0) l;

fun bin2word x =
    let
        fun bin2word' ("", _) = []
          | bin2word' (s, i) =
            let
                val b = String.sub (s, 0)
                val v = case b of
                    #"0" => 0wx0
                  | #"1" => 0wx1
                  | _ => raise Subscript    
            in
                Word32.<< (v, Word.fromInt (i))::bin2word' (String.substring (s, 1, size s - 1), i - 1)
            end
    in
        orbList (bin2word' (x, size x - 1))
    end;

(*
	cardToWord c
	TYPE: 		card -> Word32.word
	PRE:		(none)
	POST: 		c as Word32.word.
	EXAMPLE: 	cardToWord(Card("A", "h")) = 0wx10002C29: Word32.word
*)
fun cardToWord(c) = 
	bin2word(c2bin(c));
	
	
	(*
		findPlayer pl, id
		TYPE:		(int*int*int) list * int -> bool
		PRE:		id > 0 
		POST:		True/false depending on if id is in pl or not. 
		EXAMPLE:	findPlayer([(0, 1, 500)], 0) = true: bool
	*)
	(*VARIANT: length pl*)
	fun findPlayer([], _) = false
	| findPlayer(l as (id'', h'', m'')::xs, id') = List.exists(fn (x, y, z) => x = id')(l);