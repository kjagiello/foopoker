PolyML.SaveState.loadState "../isaplib/heaps/all.polyml-heap";


val ord = o_ord;
val chr = o_chr;
val explode = o_explode;
val implode = o_implode;

use "../utils/sha1-sig.sml";
use "../utils/sha1.sml";

use "../utils/json.sml";


fun readAll (h, acc) = 
    let
        val s = TextIO.inputLine h
    in
        case s
            of SOME (s)   => readAll (h, acc ^ s)
             | NONE       => acc
    end


fun renderToFile(qt, name) =
	    let
	        val h = TextIO.openOut name
	    in
	        TextIO.outputSubstr(h, Substring.full(qt));
	        TextIO.flushOut(h)
	    end;

(*
	chopJsonInt x
	TYPE: 		T -> int
	PRE: 		(none)
	POST:		x as an int. 
	EXAMPLE:	chopJsonInt[JSON.Int 2] = 2: int
*)
fun chopJsonInt (JSON.Int x) = x;

(*
	chopJsonString x
	TYPE: 		T -> string
	PRE: 		(none)
	POST:		x as a string. 
	EXAMPLE:	chopJsonString[JSON.String "hej"] = "hej": string
*)
fun chopJsonString (JSON.String x) = x; 

(*
	findPlayer p
	TYPE: 			string -> bool
	PRE: 			(none)
	POST: 			True/false. 
	SIDE-EFFECTS: 	-
	EXAMPLE: 		findPlayer("Joel") = True: bool		
*)
fun findPlayer(pl) = 
	let 
		val player = pl
		val x = TextIO.openIn "medlemsdatabas.txt"
		val db = readAll (x, "")
		val db = JSONEncoder.parse(db)
		val JSON.List users = JSON.get db "users"

		fun findPlayer'([]) = (TextIO.closeIn x;false)
		| findPlayer'(x'::xs') = 
			if JSON.toString(JSON.get x' "Name") = player then
				(TextIO.closeIn x;true)
			else
				findPlayer'(xs')
		
	in
		findPlayer'(users)
	end;

(*
	addPlayer pl, pw
	TYPE: 			string * string -> unit
	PRE: 			(none)
	POST: 			()
	SIDE-EFFECTS: 	-
	EXAMPLE: 		addPlayer("Joel", "12345") = ()
*)

fun addPlayer(pl, pw) = 
	let 
		val player = pl
		val password = SHA1.hash pw
		val money = 1000
		
		val x = TextIO.openIn "medlemsdatabas.txt"
		val db = readAll (x, "")
		val db = JSONEncoder.parse(db)
		val JSON.List users = JSON.get db "users"
		
		val pid = chopJsonInt(JSON.get db "primaryid")
		val newid = pid + 1
		val db = JSON.update ("primaryid", (JSON.Int newid)) db
		
		val	y = JSON.empty
		val y = JSON.add ("ID", (JSON.Int pid)) y
		val y = JSON.add ("Name", (JSON.String player)) y
		val y = JSON.add ("Pwd", (JSON.String password)) y
		val y = JSON.add ("Cash", (JSON.Int money)) y
		val users = JSON.List (y::users)
		
		val db = JSON.update ("users", users) db
		val db = JSON.encode db
		
	in
		renderToFile(db, "medlemsdatabas.txt")
	end;
(*
	regPlayer pl, pw
	TYPE: 			string * string -> unit
	PRE: 			(none)
	POST: 			()
	SIDE-EFFECTS: 	-
	EXAMPLE: 		regPlayer("Joel", "12345") = ()
*)
fun regPlayer(pl, pw) = 
	let 
		exception usernameExists
	in
		if findPlayer(pl) = false then
			addPlayer(pl, pw)
		else
			 raise usernameExists
	end;
	
(*
	loginPlayer pl, pw
	TYPE: 			string * string -> bool
	PRE: 			(none)
	POST: 			()
	SIDE-EFFECTS: 	-
	EXAMPLE: 		regPlayer("Joel", "12345") = true
*)
fun loginPlayer(pl, pw) =
	let 
		exception userNotExists
		exception wrongPwd
		val player = pl
		val password = SHA1.hash pw
		val x = TextIO.openIn "medlemsdatabas.txt"
		val db = readAll (x, "")
		val db = JSONEncoder.parse(db)
		val JSON.List users = JSON.get db "users"

		fun loginPlayer'([]) = (TextIO.closeIn x;raise userNotExists)
		| loginPlayer'(x'::xs') = 
			if JSON.toString(JSON.get x' "Name") = player then
				if JSON.toString(JSON.get x' "Pwd") = password then
					(TextIO.closeIn x;true)
				else
					(TextIO.closeIn x;raise wrongPwd)
			else
				loginPlayer'(xs')
			
	in
		loginPlayer'(users)
	end;


(*
	updateMoney pl, m
	TYPE: 			string * int -> unit
	PRE: 			(none)
	POST: 			()
	SIDE-EFFECTS: 	-
	EXAMPLE: 		updateMoney("Joel", 1500) = ()
*)
fun updateMoney(pl, m) = 
	let 
		val player = pl
		val money = m
		val x = TextIO.openIn "medlemsdatabas.txt"
		val db = readAll (x, "")
		val db = JSONEncoder.parse(db)
		val JSON.List users = JSON.get db "users"

		fun updateMoney'([], db') = ()
		| updateMoney'(x::xs, db') = 
			if JSON.toString(JSON.get x "Name") = player then
				let
					val users' = JSON.List ((JSON.update("Cash", (JSON.Int money)) x)::db'@xs)
					val db = JSON.update ("users", users') db
					val db = JSON.encode db
				in
					renderToFile(db, "medlemsdatabas.txt")
				end
			else
				updateMoney'(xs, x::db')
			
	in
		updateMoney'(users, [])
	end;

(*
	dropTable()
	TYPE: 		unit -> unit
	PRE: 		(none)
	POST: 		()
	ExAMPLE: 	dropTable() = (): unit
*)
fun dropTable() = 
	let
		val x = TextIO.openIn "medlemsdatabas.txt"
		val db = readAll (x, "")
		val freshTable = "{\"primaryid\":1,\"users\":[]}\n"
	in
		renderToFile(freshTable, "medlemsdatabas.txt")
	end;
	
(*
	deletePlayer(pl)
	TYPE: 		string -> unit
	PRE: 		(none)
	POST: 		()
	ExAMPLE: 	deletePlayer("Joel") = (): unit
*)
fun deletePlayer(pl) = 
	let 
		val player = pl
		val x = TextIO.openIn "medlemsdatabas.txt"
		val db = readAll (x, "")
		val db = JSONEncoder.parse(db)
		val JSON.List users = JSON.get db "users"

		fun deletePlayer'([], db') = 
			let 
				val db' = JSON.List db'
				val db = JSON.update ("users", db') db
				val db = JSON.encode db
			in
				renderToFile(db, "medlemsdatabas.txt")
			end
				
		| deletePlayer'(x'::xs', db') = 
			if JSON.toString(JSON.get x' "Name") = player then
				deletePlayer'(xs', db')
			else
				deletePlayer'(xs', x'::db')
		
	in
		deletePlayer'(users, [])
	end;


(*
	jsonToList()
	TYPE: 		unit -> (string * int) list
	PRE: 		(none)
	POST: 		A (string * int) list.
	EXAMPLE: 	jsonToList() = [("Joel", 1000), ("Krille", 1000)]: (string * int) list
*)
fun jsonToList() = 
	let 
		val x = TextIO.openIn "medlemsdatabas.txt"
		val db = readAll (x, "")
		val db = JSONEncoder.parse(db)
		val JSON.List users = JSON.get db "users"
		
		fun jsonToList'([], _) = (TextIO.closeIn x;[])
		| jsonToList'(x'::xs', n) =
			let
				val jName = chopJsonString(JSON.get x' "Name")
				val jCash = chopJsonInt(JSON.get x' "Cash")
			in
				(jName, jCash)::jsonToList'(xs', n+1)
			end
	in 
		jsonToList'(users, 1)
	end; 

(*
	topList n 
	TYPE:		int -> (string * int) list
	PRE:		n > 0 
	POST:		A (string * int) list.		
	EXAMPLE: 	topList(10) = [("Joel", "1500"), "Krille", "1000", "Jocke", 1000]
*)
fun topList(0) = []
| topList(n) = 
	let
		(*Bubblesort from:
		http://www.utdallas.edu/~krw053000/progs/bubblesort.sml*)
		(* Returns the first x elements of a list *)
		fun firstx(A, 0) = nil |
			firstx(nil, x) = nil |
			firstx(h::t, 1) = [h] |
			firstx(h::t, x) = if x > length(h::t) then h::t
				else h::firstx(t, x - 1);

		(*  Returns the last element of a list *)
		fun last([]) = nil |
			last([h]) = [h] |
			last(h::t) = last(t);

		(* Sorts once *)
		fun compareloop(L, 1) = nil |
			compareloop(nil, X) = nil |
			compareloop([h], X) = [h] |
			compareloop((p, m)::(p', m')::t, X) = if m<m' then (p', m')::(compareloop((p, m)::t, X-1))
				else (p, m)::(compareloop((p', m')::t, X-1));

		(*  Performs compareloop X times  *)

		fun sort(A, 0, 0) = nil |
			sort([h], X, Y) = [h] |
			sort(nil, X, Y) = nil |
			sort(h::t, X, 0) = sort(compareloop(h::t, length(h::t) + 1), X, 1) |
			sort(h::t, X, Y) = if X =Y then compareloop(h::t, length(h::t)+1)@last(t)
				else sort( compareloop( firstx(h::t, length(t)), length(h::t)), X, Y+1)@last(t);
		(* Bubblesorts occur*)
		fun bubblesort(nil) = nil |
			bubblesort([h]) = [h] |
			bubblesort(L) = sort(L, length(L)+1, 0);
		
		(* Grab the database*)
		val db = bubblesort(jsonToList())
		val lDb = length db
	in
		if n > lDb then
			db
		else
			List.take(db, n)		

	end;

	val y = JSON.empty;
	val y = JSON.add ("test1", (JSON.String "foo")) y;
	val y = JSON.add ("test2", (JSON.Bool true)) y;