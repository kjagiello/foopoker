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
