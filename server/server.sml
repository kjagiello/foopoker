(* just in case cleanup before building the heap *)
PolyML.fullGC();

PolyML.SaveState.loadState "../isaplib/heaps/all.polyml-heap";

val ord = o_ord;
val chr = o_chr;
val explode = o_explode;
val implode = o_implode;

use "../utils/base64-sig.sml";
use "../utils/base64.sml";

use "../utils/sha1-sig.sml";
use "../utils/sha1.sml";

use "../utils/json.sml";
use "../utils/utils.sml";

fun mapi f l =
    let fun mm _ nil = nil
          | mm n (h :: t) = f (h, n) :: mm (n + 1) t
    in
        mm 0 l
    end

fun vectorToInt (v) =
    let
        val l = rev (vectorToList v)
        val ls = length l

        fun convert (x, i) =
            let
                val x = LargeWord.fromInt (Word8.toInt x)
            in
                LargeWord.<< (x, (Word.fromInt i) * (Word.fromInt 8))
            end

        val l = mapi convert l
    in
        List.foldr (LargeWord.xorb) (LargeWord.fromInt 0) l
    end

fun parseHeaders(d) =
    let
        exception Incomplete

        fun parseHeaders'(h, []) = raise Incomplete
          | parseHeaders'(h, first::rest) = 
            let
                val tokens = String.fields (fn c => c = #":") first
            in
                case tokens of
                  (* we've got a header here *)
                    [key, value] => 
                        (HashArray.update(h, key, String.substring(value, 1, size value - 1));
                        parseHeaders'(h, rest))

                  (* end of request *)
                  | [""] => ()

                  (* keep looking for the end of the request *)
                  | _ => parseHeaders'(h, rest)
            end

        val h = HashArray.hash 32
        val d = implode (List.filter (fn c => not(c = #"\r")) (explode d))
    in
        parseHeaders'(h, String.fields (fn c => c = #"\n") d);
        h
    end

signature WEBSOCKET_PACKET = 
sig
    type packet
    
    val fromVector      : Word8Vector.vector -> Word8.word list * Word8.word list * packet
    val toVector        : (
            bool                        (* FIN *)
        *   bool                        (* RSV1 *)
        *   bool                        (* RSV2 *)
        *   bool                        (* RSV3 *)
        *   int                         (* OPCOD *)
        *   bool                        (* MASK *)
        *   int                         (* Payload length *)
        *   Word8Vector.vector          (* Masking-key *)
        *   Word8Vector.vector          (* Payload Data *)
    ) -> Word8Vector.vector
    val getPayload      : packet -> Word8Vector.vector
    val getOpcode       : packet -> int
    val isRSVSet        : packet -> bool
    val isFinal         : packet -> bool
end

structure WebsocketPacket :> WEBSOCKET_PACKET =
struct
    val op xorb = Word8.xorb
    val op andb = Word8.andb
    val op >> = Word8.>>

    (*------------------------ Wire format -------------------------+
    +---------------------------------------------------------------+
    |0                   1                   2                   3  |
    |0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1|
    +-+-+-+-+-------+-+-------------+-------------------------------+
    |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
    |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
    |N|V|V|V|       |S|             |   (if payload len==126/127)   |
    | |1|2|3|       |K|             |                               |
    +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
    |     Extended payload length continued, if payload len == 127  |
    + - - - - - - - - - - - - - - - +-------------------------------+
    |                               |Masking-key, if MASK set to 1  |
    +-------------------------------+-------------------------------+
    | Masking-key (continued)       |          Payload Data         |
    +-------------------------------- - - - - - - - - - - - - - - - +
    :                     Payload Data continued ...                :
    + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
    |                     Payload Data continued ...                |
    +--------------------------------------------------------------*)

    type packet = (
            bool                        (* FIN *)
        *   bool                        (* RSV1 *)
        *   bool                        (* RSV2 *)
        *   bool                        (* RSV3 *)
        *   int                         (* OPCOD *)
        *   bool                        (* MASK *)
        *   int                         (* Payload length *)
        *   Word8Vector.vector          (* Masking-key *)
        *   Word8Vector.vector          (* Payload Data *)
    )

    fun fromVector v =
        let
            fun word8ToBool w =
                w = (Word8.fromInt 1)

            fun getBit (w, b) =
                Word8.andb (Word8.>> (w, Word.fromInt b), Word8.fromInt 1) (* w >> b & 1 *)

            fun extract (v, i, j) =
                let
                    val v = Word8VectorSlice.full v
                    val v = Word8VectorSlice.subslice (v, i, SOME j)
                    val v = Word8VectorSlice.vector v
                in
                    v
                end

            fun getBytes (l, n) =
                (List.drop (l, n), Word8Vector.fromList (List.take (l, n)))

            fun getByte (h::t) =
                (t, h)

            val data = vectorToList v
            val orgdata = data

            (* First octet: FIN, RSV1-3, opcode *)
            val (data, octet) = getByte data
                
            val final = word8ToBool (getBit (octet, 7))
            val rsv1 = word8ToBool (getBit (octet, 6))
            val rsv2 = word8ToBool (getBit (octet, 5))
            val rsv3 = word8ToBool (getBit (octet, 4))
            val opcode = Word8.toInt (Word8.andb (octet, Word8.fromInt 15)) (* x & 0b00001111 *)

            (* Second octet *)
            val (data, octet) = getByte data

            val mask = word8ToBool (getBit (octet, 7))
            val payloadlen = Word8.toInt (Word8.andb (octet, Word8.fromInt 127)) (* x & 0b01111111 *)

            val (data, payloadlen) = case payloadlen of
                126     => getBytes (data, 2)
              | 127     => getBytes (data, 8)
              | _       => (data, Word8Vector.fromList [Word8.fromInt payloadlen])

            val payloadlen = LargeWord.toInt (vectorToInt payloadlen)

            val (data, maskkey) = getBytes (data, 4) 
            val (data, payload) = getBytes (data, payloadlen) 

            fun unmask (i, x) =
                let
                    val j = i mod 4
                    val mk = Word8Vector.sub (maskkey, j)
                in
                    Word8.xorb (x, mk)
                end

            val payload = Word8Vector.mapi unmask payload

        in

            (data, List.take (orgdata, Word8Vector.length v - length data), (final, rsv1, rsv2, rsv3, opcode, mask, payloadlen, maskkey, payload))
        end

    fun toVector (final, rsv1, rsv2, rsv3, opcode, mask, payloadlen, maskkey, payload) =
        let
            fun orbList l =
                foldr Word8.orb (Word8.fromInt 0) l

            fun lrb (x, n) =
                Word8.<< (Word8.fromInt x, Word.fromInt n)

            fun getBit (w, b) =
                Word8.andb (Word8.>> (w, Word.fromInt b), Word8.fromInt 1)

            fun vectorToList v =
                Word8Vector.foldr (fn (a, l) => a::l) [] v

            fun arrayToList v =
                Word8Array.foldr (fn (a, l) => a::l) [] v

            fun boolToInt b = if b then 1 else 0

            val octet1 = orbList [
                lrb (boolToInt final, 7),
                lrb (boolToInt rsv1, 6),
                lrb (boolToInt rsv2, 5),
                lrb (boolToInt rsv3, 4),
                Word8.andb (Word8.fromInt opcode, Word8.fromInt 15)
            ]

            val (payloadlenf) = (if payloadlen >= 65536 then
                                    127
                                else if payloadlen >= 127 then
                                    126
                                else
                                    payloadlen)

            val octet2 = orbList [
                lrb (boolToInt mask, 7),
                Word8.fromInt payloadlenf
            ]

            val pllen = Word8Array.tabulate ((case payloadlenf of 126 => 2 | 127 => 8 | _ => 0), fn _ => Word8.fromInt 0)
        in
            case payloadlenf of
                126 => PackWord16Big.update (pllen, 0, LargeWord.fromInt payloadlen)
              | 127 => (PackWord32Big.update (pllen, 1, LargeWord.fromInt payloadlen); PackWord32Big.update (pllen, 0, LargeWord.>> (LargeWord.fromInt payloadlen, Word.fromInt 32)))
              | _ => ();

            Word8Vector.fromList ((octet1::octet2::[])@(vectorToList (Word8Array.vector pllen))@(vectorToList payload))
        end

    fun getPayload p =
        #9 p

    fun getOpcode p =
        #5 p

    fun isRSVSet p =
        #2 p orelse #3 p orelse #4 p

    fun isFinal p =
        #1 p
end

signature WEBSOCKET_SERVER = 
sig
    type server
    type connectionState
    type connection
    type event
    
    val create              : int * (connection -> unit) * (connection -> unit) * (connection * int * Word8Vector.vector -> unit) -> server
    val run                 : server -> unit
    val shutdown            : server -> unit
    val readSockets         : server * Socket.sock_desc list -> unit
    val readData            : server * connection -> unit
    val sameConnection      : connection * connection -> bool
    val acceptConnection    : server -> unit
    val parseData           : server * connection -> unit
    val parseHandshake      : server * connection -> unit
    val handlePing          : server * connection * WebsocketPacket.packet -> unit
    val handlePong          : server * connection * WebsocketPacket.packet -> unit
    val handleDisconnect    : server * connection * WebsocketPacket.packet -> unit
    val handleContinuation  : server * connection * WebsocketPacket.packet -> unit
    val handlePacket        : server * connection * WebsocketPacket.packet -> unit
    val closeConnection     : server * connection * int * string -> unit
    val send                : connection * int * Word8Vector.vector -> unit
    val sendEx              : server * (connection -> bool) * int * Word8Vector.vector -> unit
end

structure WebsocketServer :> WEBSOCKET_SERVER = 
struct
    datatype connectionState =
          Closed
        | Handshake
        | Estabilished
    
    type connection = {
        socket: (INetSock.inet, Socket.active Socket.stream) Socket.sock,
        address: INetSock.inet Socket.sock_addr,
        buffer: Word8.word list ref,
        fbuffer: Word8VectorSlice.slice list ref,
        state: connectionState ref,
        fragmentOpcode: int ref
    }

    type server = {
        listenSocket: (INetSock.inet, Socket.passive Socket.stream) Socket.sock,
        connections: connection list ref,
        connectHandler: (connection -> unit),
        disconnectHandler: (connection -> unit),
        messageHandler: (connection * int * Word8Vector.vector -> unit)
    }

    datatype event = Connect of connection

    fun create (port, ch, dh, mh) =
        let
            val s = INetSock.TCP.socket ()
            val a = INetSock.any port
        in
            Socket.Ctl.setREUSEADDR (s, true);
            Socket.bind (s, a);
            Socket.listen (s, 128);

            {listenSocket=s, connections=ref [], connectHandler=ch, disconnectHandler=dh, messageHandler=mh}
        end

    fun sameConnection ({socket=s1, ...}, {socket=s2, ...}) =
        Socket.sameDesc (Socket.sockDesc s1, Socket.sockDesc s2)

    fun send ({socket=socket, ...}, opcode, v) =
        let
            val packet = WebsocketPacket.toVector (true, false, false, false, opcode, false, Word8Vector.length v, Word8Vector.fromList [], v)
        in
            Socket.sendVec (socket, Word8VectorSlice.full packet);
            ()
        end

    fun sendEx (s as {connections=ref connections, ...}, f, opcode, v) = 
        let
            val connections = List.filter f connections
        in
            map (fn c => send (c, opcode, v)) connections; ()
        end

    fun acceptConnection {listenSocket=ls, connections=cs, ...} = 
        let
            val (sock, addr) = Socket.accept ls
            val c = {socket=sock, address=addr, buffer=ref [], fbuffer=ref [], state=ref Handshake, fragmentOpcode=ref 0}
        in
            print "[server]\tNew connection has been estabilished.\n";
            cs := c::(!cs);
            ()
        end

    fun closeConnection ({connections=connections, disconnectHandler=dh, ...}, c as {socket=socket, state=state, ...}, code, reason) =
        let
            val codea = Word8Array.tabulate (2, fn _ => Word8.fromInt 0)
            val _ = PackWord16Big.update (codea, 0, LargeWord.fromInt code)
            val codev = Word8Array.vector codea
            val reason = Byte.stringToBytes reason
            val payload = Word8VectorSlice.concat [Word8VectorSlice.full codev, Word8VectorSlice.full reason]
            val payloadlen = Word8Vector.length payload
        in
            Socket.sendVec (socket, Word8VectorSlice.full (WebsocketPacket.toVector (true, false, false, false, 8, false, payloadlen, Word8Vector.fromList [], payload)));
            state := Closed;
            connections := List.filter (fn {socket=x, ...} => not (Socket.sameDesc (Socket.sockDesc x, Socket.sockDesc socket))) (!connections);
            Socket.close socket;

            print "[server]\tClosing connection.\n";

            (* call the disconnect handler *)
            dh c
        end

    fun parseHandshake (s as {connectHandler=ch, ...}, c as {buffer=buffer, socket=socket, state=state, ...}) = 
        let
            val v = Word8Vector.fromList (!buffer)
            val h = parseHeaders (Byte.bytesToString v)
            val k = HashArray.sub(h, "Sec-WebSocket-Key")
        in
            if isSome k then
                let
                    val _ = print "[client]\tClient wants to shake a hand with us.\n"

                    val k = (valOf k) ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
                    val k = SHA1.hash k
                    val k = Base64.encode k

                    val r = 
                          "HTTP/1.1 101 Switching Protocols\r\n"
                        ^ "Upgrade: websocket\r\n"
                        ^ "Connection: Upgrade\r\n"
                        ^ "Sec-WebSocket-Accept: " ^ k ^ "\r\n"
                        ^ "Sec-WebSocket-Version: 13\r\n"
                        ^ "\r\n"
                in
                    Socket.sendVec (socket, Word8VectorSlice.full (Byte.stringToBytes r));
                    buffer := [];
                    state := Estabilished;
                    print "[client]\tDone! Connection estabilished.\n";

                    (* call the connect handler *)
                    ch (c)
                end
            else
                ()
        end

    fun handlePing (s as {connections=connections, ...}, c as {buffer=buffer, socket=socket, ...}, p) = 
        let
            val pl = WebsocketPacket.getPayload p
            val plr = Byte.stringToBytes ""
        in 
            if Word8Vector.length pl > 125 then
                closeConnection (s, c, 1002, "1002/Protocol Error")
            else if not (WebsocketPacket.isFinal p) then
                closeConnection (s, c, 1002, "1002/Protocol Error")
            else
                (Socket.sendVec (socket, Word8VectorSlice.full (WebsocketPacket.toVector (true, false, false, false, 10, false, Word8Vector.length pl, Word8Vector.fromList [], pl)));
                ())
        end

    fun handlePong (s as {connections=connections, ...}, c as {buffer=buffer, socket=socket, ...}, p) = 
        if not (WebsocketPacket.isFinal p) then
            closeConnection (s, c, 1002, "1002/Protocol Error")
        else
            ()

    fun handleDisconnect (s as {connections=connections, ...}, c as {buffer=buffer, socket=socket, fragmentOpcode=fragmentOpcode, ...}, p) = 
        let
            val pl = WebsocketPacket.getPayload p
            val pll = Word8Vector.length pl
        in
            if pll = 1 orelse pll >= 126 then
                closeConnection (s, c, 1002, "1002/Protocol Error")
            else if pll >= 2 then
                let
                    val codeVec = Word8VectorSlice.vector (Word8VectorSlice.slice (pl, 0, SOME 2));
                    val code = LargeWord.toInt (vectorToInt codeVec)
                in
                    if  (code >= 0 andalso code <= 999)
                        orelse (code >= 2000 andalso code <= 2999)
                        orelse code = 1004 orelse code = 1005
                        orelse code = 1006 orelse code = 1015
                        orelse code = 1012 orelse code = 1013
                        orelse code = 1014 orelse code = 1016
                        orelse code = 1100 
                    then
                        (* Reserved codes *)
                        closeConnection (s, c, 1002, "1002/Protocol Error")
                    else
                        closeConnection (s, c, 1000, "Goodbye")
                end
            else
                closeConnection (s, c, 1000, "Goodbye")
        end

    fun handlePacket (s as {connections=connections, messageHandler=mh, ...}, c as {buffer=buffer, fbuffer=fbuffer, socket=socket, fragmentOpcode=fragmentOpcode, ...}, p) = 
        let
            val pl = WebsocketPacket.getPayload p
            val opcode = WebsocketPacket.getOpcode p
        in
            if WebsocketPacket.isFinal p then
                if null (!fbuffer) then
                    mh (c, opcode, pl)
                else
                    closeConnection (s, c, 1002, "1002/Protocol Error")
            else
                (fragmentOpcode := opcode;
                fbuffer := (!fbuffer) @ [(Word8VectorSlice.full pl)])
        end


    fun handleContinuation (s as {connections=connections, messageHandler=mh, ...}, c as {buffer=buffer, fbuffer=fbuffer, socket=socket, fragmentOpcode=ref fragmentOpcode, ...}, p) = 
        let
            val pl = WebsocketPacket.getPayload p
        in
            if null (!fbuffer) then
                closeConnection (s, c, 1002, "1002/Protocol Error")
            else if WebsocketPacket.isFinal p then
                let
                    val _ = fbuffer := (Word8VectorSlice.full pl)::(!fbuffer)
                    val pl = Word8VectorSlice.concat (rev (!fbuffer))
                in
                    mh (c, fragmentOpcode, pl);
                    fbuffer := []
                end
            else
                fbuffer := (Word8VectorSlice.full pl)::(!fbuffer)
        end 

    fun parseData (s as {connections=connections, ...}, c as {buffer=buffer, fbuffer=fbuffer, socket=socket, state=ref state, ...}) = 
        (if length (!buffer) >= 2 andalso state = Estabilished then
                    let
                        val (b, d, p) = WebsocketPacket.fromVector (Word8Vector.fromList (!buffer))
        
                        val _ = buffer := b
                        val pl = WebsocketPacket.getPayload p
                        val opcode = WebsocketPacket.getOpcode p
                        val isFinal = WebsocketPacket.isFinal p
                    in
                        if WebsocketPacket.isRSVSet p then
                            closeConnection (s, c, 1002, "1002/Protocol Error")
                        else
                            (case opcode of
                                0 => handleContinuation (s, c, p)
                              | 1 => handlePacket (s, c, p)
                              | 2 => handlePacket (s, c, p)
                              | 8 => handleDisconnect (s, c, p)
                              | 9 => handlePing (s, c, p)
                              | 10 => handlePong (s, c, p)
                              | _ => closeConnection (s, c, 1002, "1002/Protocol Error");

                            parseData (s, c))
                    end
                else
                    (* we need at least 2 bytes *)
                    ()) handle Subscript => ()

    fun readData (s as {connections=connections, disconnectHandler=dh, ...}, c as {state=state, socket=socket, buffer=buffer, ...}) =
        let
            val data = Socket.recvVec (socket, 1024)
        in
            if Word8Vector.length data = 0 then
                (* disconnect *)
                let in
                    state := Closed;
                    connections := List.filter (fn {socket=x, ...} => not (Socket.sameDesc (Socket.sockDesc x, Socket.sockDesc socket))) (!connections);
                    dh c
                end
            else
                let in
                    buffer := (!buffer) @ (vectorToList data);
                    case !state of
                        Handshake => parseHandshake (s, c)
                      | Estabilished => parseData (s, c)
                      | _ => ()
                end
        end

    fun readSockets (_, []) = ()
      | readSockets (s as {listenSocket=ls, connections=cs, ...}, sock::rest) =
        let
            val c = List.find (fn {socket=x, ...} => Socket.sameDesc (sock, Socket.sockDesc x)) (!cs)
        in
            case c of
                NONE => acceptConnection s
              | SOME c => readData (s, c)
        end

    fun run (s as {listenSocket=ls, connections=cs, ...}) =
        let 
            val lsDesc = Socket.sockDesc ls
            val csDesc = map (fn {socket=s, ...} => Socket.sockDesc s) (!cs)

            val {rds, ...} = Socket.select {rds=lsDesc::csDesc, wrs=[], exs=[], timeOut=NONE}
        in
            case rds of
                [] => run s
              | _ => (readSockets (s, rds); run s)
        end

    fun shutdown {listenSocket=ls, ...} =
        Socket.close ls
end

signature WebsocketHandler = 
sig
    type game
    type tableEvent
    type playerState
    type parsedMessage

    val players             : game ref list ref
    val boards              : game ref list ref
    val playerIndex         : game ref vector ref
    val boardIndex          : game ref vector ref

    val getFreeId           : game ref vector -> int option
    val getFreePlayerId     : unit -> int option
    val getFreeBoardId      : unit -> int option

    val parseMessage        : Word8Vector.vector -> parsedMessage

    val filterPlayers       : (game -> bool) -> game ref list
    val filterINull         : int * game -> bool
    val filterNull          : game -> bool
    val filterAll           : game -> bool
    val filterPlayer        : game -> game -> bool
    val filterOthers        : game -> game -> bool
    val filterConnection    : WebsocketServer.connection -> game -> bool

    val send                : string -> (game -> bool) -> JSON.T -> unit
    val sendResponse        : string -> (game -> bool) -> JSON.T -> unit
    val sendClientResponse  : string -> WebsocketServer.connection -> JSON.T -> unit

    val getPlayer           : WebsocketServer.connection -> game ref option
    val getPlayerName       : game ref -> string

    val createBoard         : string * int * (int * int) * (int * int) -> unit
    val spectateTable       : game ref * int -> game ref
    val joinTable           : game ref * game ref * int -> int option
    val printBoards         : unit -> unit
    val getChair            : game ref * int -> game ref option

    val createPlayer        : WebsocketServer.connection * string -> game ref option
    val handleConnect       : WebsocketServer.connection -> unit
    val handleDisconnect    : WebsocketServer.connection -> unit
    val handleMessage       : WebsocketServer.connection * int * Word8Vector.vector -> unit
    val handleTableEvent    : game ref * tableEvent -> unit
    val handleEvent         : game ref * parsedMessage -> unit
    val handleCommand       : game ref * string * string -> unit
end

structure MLHoldemServer :> WebsocketHandler = 
struct
    exception InvalidMessage
    exception InvalidCommand

    datatype playerState = 
        Normal
      | Turn of int (* time when the turn started *)
      | Folded

    datatype game = Board of {
                        id: int,
                        name: string,
                        smallBlind: int,
                        bigBlind: int,
                        minBuyIn: int,
                        maxBuyIn: int,
                        chairs: game ref vector ref
                    }
                  | Player of {
                        id: int,
                        name: string ref,
                        board: game ref,
                        connection: WebsocketServer.connection
                    }
                  | Null

    datatype tableEvent = 
        PlayerJoined of game ref
      | PlayerLeaving of game ref
      | PlayerFold of game ref
      | PlayerRaise of game ref * int
      | PlayerCall of game

    type parsedMessage = string * string * JSON.T

    val clients = ref [];
    val players = ref [];
    val boards = ref [];
    val playerIndex = ref (Vector.tabulate(1024, fn _ => ref Null));
    val boardIndex = ref (Vector.tabulate(128, fn _ => ref Null));

    fun filterNull Null = true
      | filterNull _ = false

    fun filterINull (_, x) = 
        filterNull x

    fun filterAll x = 
        true

    fun filterRefList l f =
        List.filter (fn (ref x) => f x) l

    fun filterPlayers f =
        filterRefList (!players) f 

    fun filterPlayer (Player {connection=c1, ...}) (Player {connection=c2, ...}) =
        WebsocketServer.sameConnection (c1, c2)

    fun filterConnection c1 (Player {connection=c2, ...}) =
        WebsocketServer.sameConnection (c1, c2)

    fun filterOthers p x =
        not (filterPlayer p x)

    fun getFreeId v =
        let
            val id = Vector.findi (fn (_, ref x) => filterNull x) v
        in
            case id of
                SOME (i, _) => SOME i
              | NONE => NONE
        end

    fun getFreePlayerId () =
        getFreeId (!playerIndex)

    fun getFreeBoardId () =
        getFreeId (!boardIndex)

    fun getPlayer c =
        let
            val players = filterPlayers (filterConnection c)
        in
            if null players then
                NONE
            else
                SOME (hd players)
        end

    fun getPlayerName (ref (Player {name=ref name, ...})) =
        name

    fun send e f d =
        let
            val players = filterPlayers f
            val d = d
                 |> JSON.update ("event", JSON.String e)
                 |> JSON.update ("data", d)
            val d = Byte.stringToBytes (JSON.encode d)
        in
            List.app (fn (ref (Player {connection=connection, ...})) => WebsocketServer.send (connection, 1, d)) players
        end

    fun sendResponse r f d =
        let
            val d = JSON.update ("ref", JSON.String r) d
        in
            send "" f d
        end

    fun sendClientResponse r c d =
        let
            val d = JSON.update ("ref", JSON.String r) d
                 |> JSON.update ("data", d)
                 |> JSON.encode
                 |> Byte.stringToBytes
        in
            WebsocketServer.send (c, 1, d)
        end
    
    fun createPlayer (connection, name) =
        let
            val exists = List.exists (fn (ref (Player {name=ref n, ...})) => n = name) (!players)
            val id = getFreePlayerId ()
        in
            if exists orelse id = NONE then
                NONE
            else
                let 
                    val player = (ref (Player {id=valOf id, name=ref name, board=ref Null, connection=connection}))
                in
                    players := player::(!players);
                    playerIndex := Vector.update (!playerIndex, valOf id, player);
                    SOME player
                end
        end

    fun createBoard (name, s, (sb, bb), (minb, maxb)) =
        let
            val id = getFreeBoardId ()
            val board = ref (Board {
                id=valOf id,
                name=name,
                smallBlind=sb,
                bigBlind=bb,
                minBuyIn=minb,
                maxBuyIn=maxb,
                chairs=ref (Vector.tabulate(s, fn _ => ref Null))
            })
        in
            boards := board::(!boards);
            boardIndex := Vector.update (!boardIndex, valOf id, board)
        end

    fun printBoards () =
        ()

    fun handleConnect (c) =
        let in
            clients := c::(!clients)
        end

    fun handleDisconnect (c) =
        let 
            val player = getPlayer c
        in
            case player of 
                SOME (ref (player as (Player {id=id, ...}))) => 
                    let in
                        playerIndex := Vector.update (!playerIndex, id, ref Null);
                        players := filterPlayers (filterOthers player)
                    end
              | _ => ();

            clients := List.filter (fn x => not(WebsocketServer.sameConnection (c, x))) (!clients)
        end

    fun parseMessage (m) =
        let
            val _ = PolyML.print (Byte.bytesToString m)
            val p = JSONEncoder.parse (Byte.bytesToString m) handle _ => raise InvalidMessage
            val e = JSON.lookup p "event"
            val d = JSON.lookup p "data"
            val r = JSON.lookup p "ref"
            val r = if isSome r then JSON.toString (JSON.get p "ref") else ""
        in
            if isSome e andalso isSome d then
                case (valOf e) of
                    JSON.String e => (e, r, valOf d)
                  | _ => raise InvalidMessage
            else
                raise InvalidMessage
        end

    fun serverMessage (player, message) =
        let
            val d = JSON.empty
                 |> JSON.add ("message", JSON.String (message))
        in
            send "server_message" (filterPlayer (!player)) d
        end

    fun spectateTable (player as (ref (Player {board=board, ...})), id) =
        let
            val b = Vector.sub (!boardIndex, id)
        in
            board := (!b);
            board
        end

    fun handleTableEvent (board as (ref (Board {chairs=ref chairs, ...})), PlayerJoined (player)) =
        let
            val chairs = nvectorToList chairs
            val freeChairs = filterRefList chairs filterNull
            val playersCount = (length chairs) - (length freeChairs)
        in
            if playersCount >= 2 then
                serverMessage (player, "TIME TO PLAAAAY!!")
            else
                serverMessage (player, "We need more people!!")
        end
      | handleTableEvent (_, _) =
        print "Not implemented"

    fun getChair (ref (Board {chairs=chairs, ...}), id) =
        (SOME (Vector.sub (!chairs, id))) handle Subscript => NONE

    fun joinTable (player, board as (ref (Board {chairs=chairs, ...})), id) =
        let
            val (ref s) = Vector.sub (!chairs, id)
        in
            case s of
                Null => 
                    let in
                        chairs := Vector.update (!chairs, id, player);
                        handleTableEvent (board, PlayerJoined player);
                        SOME id
                    end
              | _ => NONE
        end

    fun handleCommand (player, command, arguments) =
        let in
            case command of
                "help" => serverMessage (player, "tjenare!")
              | "players" => serverMessage (player, "Players on-line:\n" ^ (implodeStrings "\n" (map getPlayerName (!players))))
              | "rooms" => 
                let 
                    val boards = map (fn (ref (Board {name=name, id=id, ...})) => "+ " ^ name ^ " (ID: " ^ Int.toString (id) ^ ")") (!boards);
                    val message = implodeStrings "\n" boards
                in
                    serverMessage (player, "Rooms:\n" ^ message)
                end
              | "join" => 
                let
                    val id = Int.fromString arguments
                in
                    case id of
                        SOME (id) => 
                            let 
                                val (ref board) = spectateTable (player, id)
                            in
                                case board of
                                    Board {name=name, ...} => serverMessage (player, "You have joined \"" ^ name ^ "\" table.")
                                  | _ => serverMessage (player, "Wrong ID.")
                            end
                      | NONE => ()
                end
              | "sit" =>
                if validateArguments "d" arguments then
                    let
                        val id = Int.fromString arguments
                        val ref (Player {board=board, ...}) = player
                        val chair = getChair (board, valOf id)
                    in
                        case board of
                            ref Null => serverMessage (player, "Join a room first!")
                          | _ => 
                            let in
                                case (valOf chair) of
                                    ref Null =>
                                        let in
                                            joinTable (player, board, valOf id);
                                            serverMessage (player, "Time to play!")
                                        end
                                  | _ => serverMessage (player, "That chair is taken. Sorry brah!") 
                            end
                    end
                else
                    serverMessage (player, "/sit [sit id]")
              | _ => raise InvalidCommand
        end

    fun handleEvent (player as (ref (Player {name=ref username, ...})), (e, r, d)) =
        case e of 
            "chat" => 
                let 
                    val d = JSON.add ("username", JSON.String username) d
                in
                    if not (JSON.hasKeys (d, ["message"])) then
                        raise InvalidMessage
                    else 
                        let 
                            val message = JSON.toString (JSON.get d "message")
                        in
                            if size message = 0 orelse size message > 128 then
                                raise InvalidMessage
                            else
                                send "chat" filterAll d
                        end
                end
          | "command" =>
                let in
                    if JSON.hasKeys (d, ["name", "arguments"]) then
                        handleCommand (player, JSON.toString (JSON.get d "name"),
                            JSON.toString (JSON.get d "arguments"))
                            handle InvalidCommand => ()
                    else
                        raise InvalidMessage
                end


    fun handleMessage (c, opcode, m) =
        (let
            val others = List.filter (fn x => not(WebsocketServer.sameConnection (c, x))) (!clients) 
            val pm as (e, r, d) = parseMessage m
            val _ = PolyML.print (JSON.encode d)
            val player = getPlayer c
        in
            case player of
                SOME player => handleEvent (player, pm)
              | _ => ();

            (* not logged in *)
            case e of
                "login" => 
                    let 
                        val username = JSON.toString (JSON.get d "username")
                        val _ = if size username = 0 then raise InvalidMessage else ()
                        val player = createPlayer (c, username)
                        val response = JSON.empty
                                    |> JSON.add ("status", JSON.String (if isSome player then "OK" else "error"))
                        
                        val d1 = JSON.empty
                             |> JSON.add ("message", JSON.String (username ^ " has connected."))
                        val d2 = JSON.empty
                             |> JSON.add ("message", JSON.String ("Welcome, " ^ username ^ "!"))
                    in
                        sendClientResponse r c response;
                        
                        case player of
                            SOME (ref player) =>
                                let in
                                    send "server_message" (filterOthers player) d1;
                                    send "server_message" (filterPlayer player) d2
                                end
                          | _ => ()
                    end
              | _ => ();
            ()
        end) handle InvalidMessage => (print "invalid message\n");
end;

MLHoldemServer.createBoard ("Test Bord 1", 8, (5, 10), (100, 1000));
MLHoldemServer.createBoard ("Test Bord 2", 8, (5, 10), (100, 1000));
MLHoldemServer.createBoard ("Test Bord 3", 8, (5, 10), (100, 1000));
MLHoldemServer.createBoard ("Test Bord 4", 8, (5, 10), (100, 1000));
MLHoldemServer.printBoards ();
val s = WebsocketServer.create (9001, MLHoldemServer.handleConnect, MLHoldemServer.handleDisconnect, MLHoldemServer.handleMessage);
PolyML.exception_trace(fn () => WebsocketServer.run s);
(*WebsocketServer.run s handle Interrupt => WebsocketServer.shutdown s;*)
