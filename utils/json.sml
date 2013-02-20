signature JSON =
sig

   exception notobj_exn of unit

   structure Tab : NAME_TAB
   structure Name : SSTR_NAMES

   datatype T =
      Bool of bool
     | Int of int
     | List of T list
     | Null
     | Object of T Tab.T
     | Real of real
     | String of string
     | Pair of (string * T)

   val empty : T
   val update : string * T -> T -> T
   val add : string * T -> T -> T
   val lookup : T -> string -> T option
   val get : T -> string -> T
   val delete : string -> T -> T
   val encode : T -> string

   val toString : T -> string
   val hasKeys : T * string list -> bool
end

structure JSON : JSON
= struct

    structure Name = SStrName;
    structure Tab = Name.NTab;

    (*datatype name = Root | Name of string;*)
    datatype T = Object of (T Tab.T)
              | List of (T list)
              | String of string
              | Int of int
              | Bool of bool
              | Real of real
              | Pair of (string * T)
              | Null;

    val empty = Object Tab.empty;
    

    exception notobj_exn of unit;
    
    (** escapes ", \ and \n **)
    fun escape s =
      let
        fun helper #"\"" = [#"\\", #"\""]
          | helper #"\\" = [#"\\", #"\\"]
          | helper #"\n" = [#"\\", #"n"]
          | helper x = [x]
      in
        String.implode
          (foldr op@ [] (map helper (String.explode s)))
      end
    
    (*replaces ~ to - *)
    fun convert_minus s = String.implode
            (map (fn (#"~") => #"-" | x => x) (String.explode s))
    
    
    (*table functions *)
    fun add (name:string, value) (Object tab) =
        let
            val (_, t) = Tab.add (Name.mk name, value) tab;
        in
            Object t
        end
      | add _ _ = raise notobj_exn ()
    
    fun update (n:string, v) (Object tab) =
            (Object (Tab.update ((Name.mk n), v) tab))
      | update _ _ = raise notobj_exn ()
    
    fun delete (n:string) (Object tab) =
            (Object (Tab.delete (Name.mk n) tab))
      | delete _ _ = raise notobj_exn ()
      
    fun get (Object tab) (n:string) =
            Tab.get tab (Name.mk n)
      | get _ _ = raise notobj_exn ()
      
    fun lookup (Object tab) (n:string) =
            Tab.lookup tab (Name.mk n)
      | lookup _ _ = raise notobj_exn ()
    
    

    fun enc_name (name) = "\"" ^ (Name.string_of_name name) ^ "\":"
    and enc_value (String value) = "\"" ^ escape value ^ "\""
      | enc_value (Int value) = convert_minus (Int.toString value)
      | enc_value (Bool value) = Bool.toString value
      | enc_value (Real value) = Real.toString value
      | enc_value (List value) = enc_list value
      | enc_value (Null) = "null"
      | enc_value (obj) = (encode obj)
    and enc_list [] = "[]"
      | enc_list l =
        let
            val e = foldr (fn(a,b) => (enc_value a) ^ "," ^ b) "" l
        in
            "[" ^ String.substring(e, 0, (String.size e)-1) ^ "]"
        end
    and enc_1 (name, value) x = (enc_name name) ^ (enc_value value) ^ "," ^ x
    and encode (Object tab) =
        let
            val e = Tab.fold enc_1 tab "";
        in
            "{" ^ String.substring(e, 0, (String.size e)-1) ^ "}"
        end
    
    fun toString (String value) =
      value

    fun hasKeys (_, []) = true
      | hasKeys (tab, key::keys) =
        if isSome (lookup tab key) then
          hasKeys (tab, keys)
        else
          false
end;

signature JSON_CALLBACKS =
sig
   type json_data

   val json_object   : json_data list -> json_data
   val json_pair     : string * json_data -> json_data
   val json_array    : json_data list -> json_data
   val json_value    : json_data -> json_data
   val json_string   : string -> json_data
   val json_int      : int -> json_data
   val json_real     : real -> json_data
   val json_bool     : bool -> json_data
   val json_null     : unit -> json_data

   val error_handle  : string * int * string -> json_data
end

functor JSONParser (Callbacks : JSON_CALLBACKS) =
struct
   type json_data = Callbacks.json_data

   exception JSONParseError of string * int

   val inputData = ref ""
   val inputPosition = ref 0

   fun isDigit () = Char.isDigit (String.sub (!inputData,0))

   fun ws () = while (String.isPrefix " " (!inputData) orelse
                      String.isPrefix "\n" (!inputData) orelse
                      String.isPrefix "\t" (!inputData) orelse
                      String.isPrefix "\r" (!inputData))
               do (inputData := String.extract (!inputData, 1, NONE))

   fun peek () = String.sub (!inputData,0)
   fun take () = 
      String.sub (!inputData,0) before 
         inputData := String.extract (!inputData, 1, NONE)

   fun matches s = (ws(); String.isPrefix s (!inputData))
   fun consume s =
      if matches s then 
         (inputData := String.extract (!inputData, size s, NONE);
          inputPosition := !inputPosition + size s)
                   else 
         raise JSONParseError ("Expected '"^s^"'", !inputPosition)

   fun parseObject () =
      if not (matches "{") then 
         raise JSONParseError ("Expected '{'", !inputPosition)
      else 
         (consume "{"; ws ();
          if matches "}" then Callbacks.json_object [] before consume "}"
          else 
            (Callbacks.json_object (parseMembers ()) 
               before (ws (); consume "}")))

   and parseMembers () =
      parsePair () :: 
         (if matches "," then (consume ","; parseMembers ()) else [])

   and parsePair () =
      Callbacks.json_pair (parseString (),
         (ws(); consume ":"; parseValue ()))

   and parseArray () =
      if not (matches "[") then 
         raise JSONParseError ("Expected '['", !inputPosition)
      else 
        (consume "[";
         if matches "]" then
            Callbacks.json_array [] before consume "]" 
         else
            Callbacks.json_array (parseElements ()) before (ws (); consume "]"))

   and parseElements () =
      parseValue () ::
         (if matches "," then (consume ","; parseElements ()) else [])

   and parseValue () =
      Callbacks.json_value (
         if matches "\"" then Callbacks.json_string (parseString ()) else
         if matches "-" orelse isDigit () then parseNumber () else
         if matches "true" then Callbacks.json_bool true else
         if matches "false" then Callbacks.json_bool false else
         if matches "null" then Callbacks.json_null () else
         if matches "[" then parseArray () else
         if matches "{" then parseObject () else
         raise JSONParseError ("Expected value", !inputPosition))

   and parseString () =
        (ws () ;
         consume ("\"") ;
         parseChars () before consume "\"")

   and parseChars () = 
   let
      fun pickChars s =
         if peek () = #"\"" (* " *) then s else
            pickChars (s ^ String.str (take ()))
   in
      pickChars ""
   end

   and parseNumber () =
   let
      val i = parseInt ()
   in
      if peek () = #"e" orelse peek () = #"E" then 
         Callbacks.json_int (valOf (Int.fromString (i^parseExp())))
      else if peek () = #"." then
         let
            val f = parseFrac()

            val f' = if peek() = #"e" orelse peek() = #"E" then
                        i ^ f ^ parseExp ()
                     else i ^ f
         in
            Callbacks.json_real (valOf (Real.fromString f'))
         end
      else Callbacks.json_int (valOf (Int.fromString i))
   end

   and parseInt () =
   let
      val f =
         if peek () = #"0" then
            raise JSONParseError ("Invalid number", !inputPosition)
         else if peek () = #"-" then (take (); "~")
         else String.str (take ())
   in
      f ^ parseDigits ()
   end

   and parseDigits () = 
   let
      val r = ref ""
   in
      (while Char.isDigit (peek ()) do
         r := !r ^ String.str (take ());
       !r)
   end

   and parseFrac () =
      (consume "." ;
         "." ^ parseDigits ())

   and parseExp () =
   let
      val _ = 
         if peek () = #"e" orelse
            peek () = #"E" then take ()
         else 
            raise JSONParseError ("Invalid number", !inputPosition)

      val f = if peek () = #"-" then (take (); "~")
               else if peek () = #"+" then (take (); "")
               else ""
   in
      "e" ^ f ^ parseDigits ()
   end

   fun parse s = 
      (inputData := s ;
       inputPosition := 0 ;
       parseObject ()) handle JSONParseError (m,p) => 
         Callbacks.error_handle (m,p,!inputData)
end

structure JSONEncoderCallbacks =
struct
   type json_data = JSON.T

   fun json_object l = 
      let
         val x = ref JSON.empty
      in
         map (fn JSON.Pair(k, v) => x := JSON.update(k, v) (!x)) l;
         !x
      end

   fun json_pair (k,v) = JSON.Pair(k, v)
   fun json_array l = JSON.List(l)
   fun json_value x = x
   fun json_string s = JSON.String(s)
   fun json_int x = JSON.Int(x)
   fun json_real x = JSON.Real(x)
   fun json_bool x = JSON.Bool(x)
   fun json_null () = JSON.Null;

   fun error_handle (msg,pos,data) =
      raise Fail ("Error: " ^ msg ^ " near " ^ Int.toString pos)
end


structure JSONEncoder = JSONParser (JSONEncoderCallbacks);
(*
val x = JSON.empty
|> JSON.add ("type", JSON.Int 2)
|> JSON.add ("f", (JSON.String "getElementById"))
|> JSON.add ("arg1", (JSON.String "foo"))
|> JSON.add ("arg2", (JSON.Bool true))
|> JSON.update ("arg1", JSON.Null);

val y = JSON.empty;
val y = JSON.add ("test1", (JSON.String "foo")) y;
val y = JSON.add ("test2", (JSON.Bool true)) y;

val x = JSON.add ("obj", y) x;

TextIO.print (JSON.encode x);
val test = JSON.encode x
*)