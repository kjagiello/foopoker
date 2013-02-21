fun vectorToList v =
    Word8Vector.foldr (fn (a, l) => a::l) [] v

fun nvectorToList v =
    Vector.foldr (fn (a, l) => a::l) [] v

fun orbList l =
    foldr Word32.orb (Word32.fromInt 0) l

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
    end

fun implodeStrings s l = 
    foldr (fn (x, "") => x | (x, y) => x ^ s ^ y) "" l

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
    end
