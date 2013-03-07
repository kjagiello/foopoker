open SmlTests;

(*
    vectorToList v
    TYPE: Word8Vector.vector -> Word8Vector.elem list
    PRE: (none)
    POST: v as list
    SIDE-EFFECTS: (none)
*)
fun vectorToList v =
    Word8Vector.foldr (fn (a, l) => a::l) [] v;

(* Test cases for vectorToList. *)
let
    val l1 = map Word8.fromInt [1, 2, 3, 4, 5]
    val l2 = vectorToList (Word8Vector.fromList l1)
in
    test("vectorToList equals with input list converted to vector", assert_equals(l1, l2))
end;

(*
    nvectorToList v
    TYPE: Vector.vector -> Vector.elem list
    PRE: (none)
    POST: v as list
    SIDE-EFFECTS: (none)
*)
fun nvectorToList v =
    Vector.foldr (fn (a, l) => a::l) [] v;

(* Test cases for nvectorToList. *)
let
    val l1 = [1, 2, 3, 4, 5]
    val l2 = nvectorToList (Vector.fromList l1)
in
    test("nvectorToList equals with input list converted to vector", assert_equals(l1, l2))
end;

(*
    orbList l
    TYPE: Word8.word list -> Word8.word
    PRE: (none)
    POST: Binary OR together all elements in l.
    SIDE-EFFECTS: (none)
*)
fun orbList l =
    foldr Word32.orb (Word32.fromInt 0) l;

(* Test cases for orbList. *)
let
    val x1 = orbList [0wx1, 0wx0]
    val x2 = 0wx1

    val x3 = orbList [0wx1, 0wx2]
    val x4 = orbList [0wx3]
in
    test("orbList test 1", assert_equals(x1, x2));
    test("orbList test 2", assert_equals(x3, x4))
end;

(*
    bin2word s
    TYPE: string -> Word8.word
    PRE: (none)
    POST: Number from the binary represented number s. 
    SIDE-EFFECTS: (none)
*)
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

(* Test cases for bin2word. *)
let
    val x1 = bin2word "01101011"
    val x2 = 0wx6B

    val x3 = bin2word ""
    val x4 = 0wx0
in
    test("bin2word test 1", assert_equals(x1, x2));
    test("bin2word test 2", assert_equals(x3, x4))
end;

(*
    implodeStrings s l
    TYPE: string -> string list -> string
    PRE: (none)
    POST: l with s inbetween elements.
    SIDE-EFFECTS: (none)
*)
fun implodeStrings s l = 
    foldr (fn (x, "") => x | (x, y) => x ^ s ^ y) "" l;

(* Test cases for implodeStrings. *)
let
    val x1 = implodeStrings "," ["a", "b", "c", "d"]
    val x2 = "a,b,c,d"

    val x3 = implodeStrings "," []
    val x4 = ""
in
    test("implodeStrings test 1", assert_equals(x1, x2));
    test("implodeStrings test 2", assert_equals(x3, x4))
end;

(*
    validateArguments f args
    TYPE: string -> string -> bool
    PRE: (none)
    POST: true if arguments in args are conforming
        to format f, false otherwise.
    SIDE-EFFECTS: (none)
*)
fun validateArguments f args =
    let
        exception InvalidArgumentFormat of char

        fun validateArguments' (_, []) = true
          | validateArguments' (f::format, a::args) = 
            if (case f of
                #"s" => true
              | #"d" => isSome (Int.fromString a)
              | _ => raise InvalidArgumentFormat f)
            then
                validateArguments' (format, args)
            else
                false

        val args = String.tokens (fn x => x = #" ") args
        val f = String.explode f
    in
        if length f <> length args then
            false
        else
            validateArguments' (f, args)
    end;

(* Test cases for implodeStrings. *)
let
    val x1 = implodeStrings "," ["a", "b", "c", "d"]
    val x2 = "a,b,c,d"

    val x3 = implodeStrings "," []
    val x4 = ""
in
    test("implodeStrings test 1", assert_equals(x1, x2));
    test("implodeStrings test 2", assert_equals(x3, x4))
end;
