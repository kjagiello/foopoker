(* Encoding and decoding Base64. *)

(* The original version is by Anthony Shipman, and was found
   at http://web.access.net.au/felixadv/smlbook.html. (Looks cool!)
   This file was modified on 7 Jan 2003 by Tom Murphy to conform
   to the sml-lib style.

   Copyright (c) 2001 Anthony L Shipman

Permission is granted to anyone to use this version of the software
for any purpose, including commercial applications, and to alter it and
redistribute it freely, subject to the following restrictions:

    1. Redistributions in source code must retain the above copyright
    notice, this list of conditions, and the following disclaimer.

    2. The origin of this software must not be misrepresented; you must
    not claim that you wrote the original software. If you use this
    software in a product, an acknowledgment in the product documentation
    would be appreciated but is not required.

    3. If any files are modified, you must cause the modified files to
    carry prominent notices stating that you changed the files and the
    date of any change.

Disclaimer

    THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
    OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT,
    INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
    IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*)

structure Base64 : BASE64 =
struct

    structure SS = Substring

    (* Called SS.all or SS.full, depending on basis version.
       This always works. *)
    fun ss_all s = SS.extract(s, 0, NONE)

    (*  This will return NONE if there is some error.
    *)
    fun decode str : string option =
    let
        (*  This will throw an exception if there aren't 0 or 4 chars left
            in ss.
        *)
        fun loop ss rslt =
            if SS.isEmpty ss
            then
                SOME(implode(rev rslt))
            else
                let
                    val four = SS.slice(ss, 0, SOME 4)
                    val rest = SS.slice(ss, 4, NONE)
                in
                    loop rest (push four rslt)
                end

        (*  Convert a substring of 4 chars to 3 chars and push them onto rslt.
        *)
        and push (group: substring) (rslt: char list) =
        let
            fun get ss numeq bits =
                case SS.getc ss of
                  NONE => (numeq, bits)
                | SOME (c, rest) =>
                let
                    val v = cvt c
                    val n' = if c = #"=" then numeq+1 else numeq
                    val b' = Word.orb(v, Word.<<(bits, 0w6))
                in
                    get rest n' b'
                end

            (* XXX could benefit from a table *)
            and cvt ch =
                if ch >= #"A" andalso ch <= #"Z"
                then
                    Word.fromInt((ord ch) - 65)
                else
                if ch >= #"a" andalso ch <= #"z"
                then
                    Word.fromInt((ord ch) - 71)
                else
                if ch >= #"0" andalso ch <= #"9"
                then
                    Word.fromInt((ord ch) + 4)
                else
                if ch = #"+"
                then
                    0w62
                else
                if ch = #"/"
                then
                    0w63
                else
                    0w0

            and split bytes 0  r = r
              | split bytes nc r =
            let
                (* get left-most byte *)
                val left = Word.>>(bytes, 0w24)
                val rest = Word.<<(bytes, 0w8)
                val c = chr(Word.toInt left)
            in
                split rest (nc-1) (c::r)
            end

            val (numeq, bytes) = get group 0 0w0
        in
            split (Word.<<(bytes, 0w8)) (3 - numeq) rslt
        end

    in
        loop (ss_all str) []
    end
    handle _ => NONE


    fun encode the_str : string =
    let
        (*  Grab groups of 3 chars. 
            n is the number of chars to add to a group.
        *)

        fun loop ss 0 acc rslt =
            (* end of a group *)
            loop ss 3 0w0 (push acc 4 rslt)
          | loop ss n acc rslt =
            (* add to a group, n=1,2,3 *)
            case SS.getc ss of
              NONE => 
                let
                    (* no more chars, pad the group *)
                    val final =
                        if n = 3    (* group is empty *)
                        then rslt
                        else
                            let 
                                (* flush the group, n=1,2 *)
                                val acc' = padz acc n
                            in
                                pade n (push acc' (4-n) rslt)
                            end
                in
                    implode(rev final)
                end
            | SOME (c, rest) =>
            let
                val b = Word.fromInt(ord c)
                val acc' = Word.orb(Word.<<(acc, 0w8), b)
            in
                loop rest (n-1) acc' rslt
            end

        (*  Pad the acc with 0 bytes. *)
        and padz acc 0 = acc
          | padz acc m = padz (Word.<<(acc, 0w8)) (m-1)

        (*  Pad with = chars at the end of the string. *)
        and pade 0 rslt = rslt
          | pade n rslt = pade (n-1) (#"="::rslt)

        (*  Push n characters from acc onto rslt.
            The input is padded with 0 to get 24 bits.
        *)
        and push acc 0 rslt = rslt
          | push acc n rslt =
        let
            val b = Word.>>(acc, 0w18)  (* top 6 bits *)
            val acc' = Word.andb(Word.<<(acc, 0w6), 0wxffffff)
            val c = cvt(Word.toInt b)
        in
            push acc' (n-1) (c::rslt)
        end

        (* XXX again, tabling... *)
        and cvt b =
            if b <= 25
            then
                chr(65 + b)
            else
            if b <= 51
            then
                chr(71 + b)
            else
            if b <= 61
            then
                chr(b - 4)
            else
            if b = 62
            then #"+"
            else #"/"
    in
        if the_str = ""
        then the_str (* spec appears to be undefined on this case *)
        else loop (ss_all the_str) 3 0w0 []
    end

end
