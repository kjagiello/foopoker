(* just in case cleanup before building the heap *)
PolyML.fullGC();

PolyML.SaveState.loadState "../isaplib/heaps/all.polyml-heap";

use "poker.sml";

val ord = o_ord;
val chr = o_chr;
val explode = o_explode;
val implode = o_implode;

(* 3rd party libraries *)
use "../utils/base64-sig.sml";
use "../utils/base64.sml";
use "../utils/sha1-sig.sml";
use "../utils/sha1.sml";
use "../utils/json.sml";

use "../utils/utils.sml";
use "databas.sml";

(*
    getBit (w, b)
    TYPE: Word8.word * int -> bool
    PRE: (none)
    POST: Gets bit b of w (counting from the LSB, Big Endian).
    SIDE-EFFECTS: (none)
*)
fun getBit (w, b) =
    Word8.andb (Word8.>> (w, Word.fromInt b), Word8.fromInt 1) (* w >> b & 1 *)

(*
    mapi f l
    TYPE: ('a * int -> 'b) -> 'a list -> 'b list
    PRE: (none)
    POST: New list by mapping the function f from left to right over l.
    SIDE-EFFECTS: (none)
    EXAMPLE: 
        mapi (fn (x, i) => i) [5, 10, 15] = [0, 1, 2]
*)
fun mapi f l =
    let fun mm _ nil = nil
          | mm n (h :: t) = f (h, n) :: mm (n + 1) t
    in
        mm 0 l
    end

(*
    vectorToInt v
    TYPE: Word8Vector.vector -> LargeWord.word
    PRE: (none)
    POST: Vector v seen as binary integer (in big-endian format).
    SIDE-EFFECTS: (none)
    EXAMPLE: 
        vectorToInt (Word8Vector.fromList (map Word8.fromInt [1, 2, 3])) = 0wx10203

*)
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

(*
    parseHeaders d
    TYPE: string -> string HashArray.hash
    PRE: (none)
    POST: HashArray containing all HTTP headers in d
    SIDE-EFFECTS: raises Incomplete exception if there is no trailing space
    EXAMPLE: 
        parseHeaders("GET /Basis/vector.html HTTP/1.1\nHost:
www.standardml.org\nConnection: keep-alive\nCache-Control: no-cache\nPragma: no-
cache\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
\nUser-Agent: Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.17 (KHTML, like
Gecko) Chrome/24.0.1312.56 Safari/537.17\nReferer: https://www.google.se
/\nAccept-Encoding: gzip,deflate,sdch\nAccept-Language: en,pl;q=0.8,sv;q=0.6
\nAccept-Charset: ISO-8859-2,utf-8;q=0.7,*;q=0.3\n\n") =
           Hash
            {hash = ref fn, used = ref 9, entries =
             ref
                (fromList[Some ("Host", "www.standardml.org"),
                      Some ("Accept-Charset", "ISO-8859-2,utf-8;q=0.7,*;q=0.3"),
                      None, None, None,
                      Some
                       ("User-Agent",
                        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.56 Safari/537.17"),
                      ..., ...])}
*)
fun parseHeaders d =
    let
        exception Incomplete

        (*
            parseHeaders' (h, l)
            TYPE: strnig HashArray.hash * string list -> unit
            PRE: type
            POST: (none)
            SIDE-EFFECTS: Updates h with correct headers from l. 
                Raises Incomplete if the headers in l does not end with
                an empty line.
        *)
        fun parseHeaders' (h, []) = raise Incomplete
          | parseHeaders' (h, first::rest) = 
            let
                val tokens = String.fields (fn c => c = #":") first
            in
                case tokens of
                  (* we've got a header here *)
                    [key, value] => 
                        (HashArray.update (h, key, String.substring(value, 1, size value - 1));
                        parseHeaders' (h, rest))

                  (* end of request *)
                  | [""] => ()

                  (* keep looking for the end of the request *)
                  | _ => parseHeaders' (h, rest)
            end

        val h = HashArray.hash 32
        val d = implode (List.filter (fn c => not (c = #"\r")) (explode d))
    in
        parseHeaders'(h, String.fields (fn c => c = #"\n") d);
        h
    end

signature WEBSOCKET_PACKET = 
sig
    (*  REPRESENTATION CONVENTION: packet follows WebSocket protocol
        specification (RFC 6455) and is therefore representing
        almost all parts (extensions are not supported) of any WebSocket
        packet. For more information about meaning of parts of the packet,
        please read Section 5.2 of RFC 6455.
        REPRESENTATION INVARIANT: (none)
    *)
    type packet
    
    (*
        fromVector v
        TYPE: Word8Vector.vector -> Word8.word list * Word8.word list * packet
        PRE: (none)
        POST: (original data from v, original data from v without parsed packet
            data, parsed packet)
        SIDE-EFFECTS: (none)
    *)
    val fromVector      : Word8Vector.vector -> Word8.word list * Word8.word list * packet
    
    (*
        toVector p
        TYPE: bool * bool * bool * bool * int * bool * int * 
            Word8Vector.vector * Word8Vector.vector -> Word8Vector.vector
        PRE: (none)
        POST: p as vector following WebSocket protocal specification.
        SIDE-EFFECTS: (none)
    *)
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
    
    (*
        getPayload p
        TYPE: packet -> Word8Vector.vector
        PRE: (none)
        POST: The payload of the p.
        SIDE-EFFECTS: (none)
    *)
    val getPayload      : packet -> Word8Vector.vector
    
    (*
        getOpcode p
        TYPE: packet -> int
        PRE: (none)
        POST: The opcode of p.
        SIDE-EFFECTS: (none)
    *)
    val getOpcode       : packet -> int
    
    (*
        isRSVSet
        TYPE: packet -> bool
        PRE: (none)
        POST: true if any RSV flag is set, false otherwise
        SIDE-EFFECTS: (none)
    *)
    val isRSVSet        : packet -> bool
    
    (*
        isFinal
        TYPE: packet -> bool
        PRE: (none)
        POST: true if the packet is the final part of a message, 
            false otherwise.
        SIDE-EFFECTS: (none)
    *)
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
            (*
                word8ToBool w
                TYPE: Word8.word -> bool
                PRE: (none)
                POST: true if w = 1, false otherwise
                SIDE-EFFECTS: (none)
            *)
            fun word8ToBool w =
                w = (Word8.fromInt 1)

            (*
                extract (v, s, l)
                TYPE: Word8VectorSlice.vector * int * int -> Word8VectorSlice.vector
                PRE: (none)
                POST: Vector containg a part of v starting from s with length l.
                SIDE-EFFECTS: (none)
            *)
            fun extract (v, i, j) =
                let
                    val v = Word8VectorSlice.full v
                    val v = Word8VectorSlice.subslice (v, i, SOME j)
                    val v = Word8VectorSlice.vector v
                in
                    v
                end

            (*
                getBytes (l, n)
                TYPE: Word8Vector.elem list * int -> Word8Vector.elem list * Word8Vector.vector
                PRE: (none)
                POST: (l without first n elements, vector with first n elements
                    of l)
                SIDE-EFFECTS: (none)
            *)
            fun getBytes (l, n) =
                (List.drop (l, n), Word8Vector.fromList (List.take (l, n)))

            (*
                getByte l
                TYPE: Word8Vector.elem list -> Word8Vector.elem list * Word8Vector.vector
                PRE: length l > 0
                POST: (l without first element, first element in l)
                SIDE-EFFECTS: (none)
            *)
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

            (*
                unmask k (i, n)
                TYPE: Word8Vector.vector -> int * Word8.word -> Word8.word
                PRE: (none)
                POST: Masked n at index i with masking key k, following
                    WebSocket protocol specification (Section 5.3 in RFC6455). 
                SIDE-EFFECTS: (none)
            *)
            fun unmask maskkey (i, x) =
                let
                    val j = i mod 4
                    val mk = Word8Vector.sub (maskkey, j)
                in
                    Word8.xorb (x, mk)
                end

            val payload = Word8Vector.mapi (unmask maskkey) payload

        in
            (data, List.take (orgdata, Word8Vector.length v - length data), (final, rsv1, rsv2, rsv3, opcode, mask, payloadlen, maskkey, payload))
        end

    fun toVector (final, rsv1, rsv2, rsv3, opcode, mask, payloadlen, maskkey, payload) =
        let
            (*
                orbList l
                TYPE: Word8.word list -> Word8.word
                PRE: (none)
                POST: Binary OR together all elements in l.
                SIDE-EFFECTS: (none)
            *)
            fun orbList l =
                foldr Word8.orb (Word8.fromInt 0) l

            (*
                lrb (x, n)
                TYPE: int * int -> Word8.word
                PRE: (none)
                POST: Binary left shift x with n positions.
                SIDE-EFFECTS: (none)
            *)
            fun lrb (x, n) =
                Word8.<< (Word8.fromInt x, Word.fromInt n)

            (*
                vectorToList v
                TYPE: Word8Vector.vector -> Word8Vector.elem list
                PRE: (none)
                POST: v as list
                SIDE-EFFECTS: (none)
            *)
            fun vectorToList v =
                Word8Vector.foldr (fn (a, l) => a::l) [] v

            (*
                arrayToList a
                TYPE: Word8Array.array -> Word8Array.elem list
                PRE: (none)
                POST: a as list
                SIDE-EFFECTS: (none)
            *)
            fun arrayToList v =
                Word8Array.foldr (fn (a, l) => a::l) [] v

            (*
                boolToInt x
                TYPE: bool -> int
                PRE: (none)
                POST: 1 if b = true, 0 otherwise
                SIDE-EFFECTS: (none)
            *)
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
    (*  REPRESENTATION CONVENTION: socket consists of 
            - listenSocket: handles all incomming connections
            - connections: holds all the active websocket connections
            - connectHandler: a callback function that is called every time
                a new connection has been estabilished
            - disconnectHandler: a callback function that is called every time
                some connection has been closed
            - messageHander: a callback function that is called every time
                a new and *complete* message has been received
            - tickHandler: a callback function that gets called every 1000ms
        REPRESENTATION INVARIANT: (none)
    *)
    type server

    (*  REPRESENTATION CONVENTION: A connectionState has three representations
            describing the state of a WebSocket connection:
            - Closed: the connection has been closed
            - Handshake: the client has connected and sent a HTTP handshake
            - Estabilished: the connection has been fully estabilished
        REPRESENTATION INVARIANT: (none)
    *)
    type connectionState

    (*  REPRESENTATION CONVENTION: connection consists of:
            - socket: a TCP socket for the client connection
            - address: client's address
            - buffer: buffers all incomming client data
            - fbuffer: buffers all fragmented client packets
            - state: holds the state of client connection
            - fragmentOpcode: the opcode of the last fragmented packet
        REPRESENTATION INVARIANT: (none)
    *)
    type connection
    
    (*
        create (p, ch, dh, mh, th)
        TYPE: int * (connection -> unit) * (connection -> unit) * (connection * int * Word8Vector.vector -> unit) * (unit -> unit) -> server
        PRE: 1024 <= p =< 65535
        POST: initialized server with listen socket on port p and
            connectHandler, disconnectHandler, messageHandler and tickHandler
            from ch, dh, mh and th respectively.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val create              : int * (connection -> unit) * (connection -> unit) * (connection * int * Word8Vector.vector -> unit) * (unit -> unit) -> server
    
    (*
        run s
        TYPE: server -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: It runs server s and blocks in the server loop until
            any abort signal is received.
        EXAMPLE: 
    *)
    val run                 : server -> unit
    
    (*
        shutdown s
        TYPE: server -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Deinitializes the server s, closes all the sockets and
            performs all necessary cleanups.
        EXAMPLE: 
    *)
    val shutdown            : server -> unit
    
    (*
        readSockets (s, scks)
        TYPE: server * Socket.sock_desc list -> unit
        PRE: scks must contain only sockets that are ready to read from
        POST: (none)
        SIDE-EFFECTS: If any of the sockets in scks is the listen socket,
            it means that a new connection is awaiting for accepting, so
            it gets accepted and estabilished between the client and server s.
            For all other sockets in scks (i.e. clients), we read data from them
            inte respective buffers.
        EXAMPLE: 
    *)
    val readSockets         : server * Socket.sock_desc list -> unit
    
    (*
        readData (s, c)
        TYPE: server * connection -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: It reads data from the socket s and tries to parse it as
            soon we have enough data in the buffer. Every time we have 
            a complete message, messageHandler of s gets called. 
        EXAMPLE: 
    *)
    val readData            : server * connection -> unit
    
    (*
        sameConnection (c1, c2)
        TYPE: connection * connection -> bool
        PRE: (none)
        POST: true if c1 and c2 are the same, false otherwise.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val sameConnection      : connection * connection -> bool
    
    (*
        acceptConnection s
        TYPE: server -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: It accepts a connection that is awaiting on
            the listen socket of s.
        EXAMPLE: 
    *)
    val acceptConnection    : server -> unit
    
    (*
        parseData (s, c)
        TYPE: server * connection -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Tries to parse the data buffer in c and if it succedes,
            further actions are performed depending on the opcode of the packet.
        EXAMPLE: 
    *)
    val parseData           : server * connection -> unit
    
    (*
        parseHandshake (s, c)
        TYPE: server * connection -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Parses the WebSocket handshake of c and generates
            a response, then sends it to the client and finally accepts
            the connection. An accepted connection is signaled to
            the connectHandler in s.
        EXAMPLE: 
    *)
    val parseHandshake      : server * connection -> unit
    
    (*
        handlePing (s, c, p)
        TYPE: server * connection * WebsocketPacket.packet -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Handles a WebSocket packet with Ping opcode 
            and takes actions conforming with the WebSocket protocol
            specification (e.g. closes the connection in s if the packet is
            invalid).
        EXAMPLE: 
    *)
    val handlePing          : server * connection * WebsocketPacket.packet -> unit
    
    (*
        handlePong (s, c, p)
        TYPE: server * connection * WebsocketPacket.packet -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Handles a WebSocket packet p with Pong opcode 
            from the client c and takes actions conforming with 
            the WebSocket protocol specification (e.g. closes the connection
            in s if the packet is invalid).
        EXAMPLE: 
    *)
    val handlePong          : server * connection * WebsocketPacket.packet -> unit
    
    (*
        handleDisconnect (s, c, p)
        TYPE: server * connection * WebsocketPacket.packet -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Handles a WebSocket packet p with Disconnect opcode 
            and takes actions conforming with the WebSocket protocol
            specification, i.e. it sends back an acknowledgment and 
            properly closes the connection in s.
        EXAMPLE: 
    *)
    val handleDisconnect    : server * connection * WebsocketPacket.packet -> unit
    
    (*
        handleContinuation (s, c, p)
        TYPE: server * connection * WebsocketPacket.packet -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Handles a WebSocket packet p with Continuation opcode 
            and takes actions conforming with the WebSocket protocol
            specification, e.g. it buffers fragmented message in c or 
            closes connection in s if the packet is invalid.
        EXAMPLE: 
    *)
    val handleContinuation  : server * connection * WebsocketPacket.packet -> unit
    
    (*
        handlePacket (s, c, p)
        TYPE: server * connection * WebsocketPacket.packet -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Handles incomming message packet p from c. If the message 
            is fragmented, we handle it separately. If the message is complete,
            we call messageHandler in s.
        EXAMPLE: 
    *)
    val handlePacket        : server * connection * WebsocketPacket.packet -> unit
    
    (*
        closeConnection (s, c, i, r)
        TYPE: server * connection * int * string -> unit
        PRE: Code c must conform with Section 7.4 of WebSocket specification
            protocol.
        POST: (none)
        SIDE-EFFECTS: Closes the connection c in server s with code c and 
            reason r.
        EXAMPLE: 
    *)
    val closeConnection     : server * connection * int * string -> unit
    
    (*
        send (c, o, p)
        TYPE: connection * int * Word8Vector.vector -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Sends a WebSocket packet with opcode o and payload p 
            to client c.
        EXAMPLE: 
    *)
    val send                : connection * int * Word8Vector.vector -> unit
    
    (*
        sendEx (s, f, o, p)
        TYPE: server * (connection -> bool) * int * Word8Vector.vector -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Sends a WebSocket packet with opcode o and payload p 
            to all the connections in s that pass filter function f.
        EXAMPLE: 
    *)
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
        messageHandler: (connection * int * Word8Vector.vector -> unit),
        tickHandler: unit -> unit
    }

    fun create (port, ch, dh, mh, th) =
        let
            val s = INetSock.TCP.socket ()
            val a = INetSock.any port
        in
            Socket.Ctl.setREUSEADDR (s, true);
            Socket.bind (s, a);
            Socket.listen (s, 128);

            {listenSocket=s, connections=ref [], connectHandler=ch, disconnectHandler=dh, messageHandler=mh, tickHandler=th}
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

    fun run (s as {listenSocket=ls, connections=cs, tickHandler=th, ...}) =
        let 
            val lsDesc = Socket.sockDesc ls
            val csDesc = map (fn {socket=s, ...} => Socket.sockDesc s) (!cs)

            val {rds, ...} = Socket.select {rds=lsDesc::csDesc, wrs=[], exs=[], timeOut=SOME (Time.fromSeconds 1)}
        in
            th ();

            case rds of
                [] => run s
              | _ => (readSockets (s, rds); run s)
        end

    fun shutdown {listenSocket=ls, ...} =
        Socket.close ls
end

signature WebsocketHandler = 
sig
    (*  REPRESENTATION CONVENTION: 
        game has three different representations:
            - Player, that represents a single, connected and logged in client.
              Each Player holds following data:
                * id: an unique, server-side identificator of the player
                * name: name of the player
                * board: the Board the player is currently spectating
                * connection: client's connection to the server
                * state: the state of a playing (sitting at the table) player
                * cards: player's cards in the current game
                * money: player's money
                * stake: player's stake in the current game
            - Board, that represents a single room/board/table
              (all these are the same thing). Each Board holds following data:
                * id: an unique, server-side identificator of the board
                * name: name of the board
                * smallBlind: the small blind at the table
                * bigBlind: the big blind at the table
                * minBuyIn: the minimal buy-in at the table
                * maxBuyIn: the maximal buy-in at the table
                * sidePotList: a generated at the fly list of the sidepots
                    fir the current game
                * chairs: all the seats at the table
                * state: the state of the game at the table
                * deck: deck with cards for the current game
                * cards: cards at the table for the current game
                * spectators: players currently subscribed to the board
                * pot: current money at the table (TODO: remove (deprecated))
                * betTimer: a handle for the bet timer
                * startTimer: a handle for the start timer (used to
                    delay the start of the game)
            - Null, that represents nothing.
        REPRESENTATION INVARIANT: 
            id > 0
            size name > 0
            money > 0
            0 < smallBlind < bigBlind
            0 < minBuyIn < maxBuyIn
    *)
    type game

    (*  REPRESENTATION CONVENTION:
        time has two different representations:
            - Timer(id, f, exipreTime), there id is an unique timer id,
                f is the function that will get called when actual time
                has passed expireTime.
            - NullTimer represents an empty timer that does nothing.
        REPRESENTATION INVARIANT:
            (none)
    *)
    type timer

    (*  REPRESENTATION CONVENTION:
        betType has two different representations that represents
        the type of a bet:
            - BetNormal: Normal bet, which means that it should be
                the max bet for current round.
            - BetSmallBlind: Small blind.
            - BetBigBlind: Big blind.
        REPRESENTATION INVARIANT:
            (none)
    *)
    type betType

    (*  REPRESENTATION CONVENTION:
        tableState has several different representations:
            - TableIdle: this state tells us that nothing currently happens
                at the table.
            - TablePreFlop: Pre flop (a poker term).
            - TableFlop: Flop (a poker term).
            - TableTurn: Turn (a poker term).
            - TableRiver: River (a poker term).
            - TableShowdown: Showdown (a poker term).
            - TableBet (nextState, betType, startPos, curPost, maxBet):
                This state represents a poker bet that has to be made.
                * nextState: the next state that should be set after the
                    last bet has been made
                * betType: the type of the bet
                * startPos: the position (the id of the seat at the table), 
                    where the betting round has started. Each time somebody does
                    any action, that requires that other seats needs to bet
                    (i.e. a raise), this position is updated to the curPos.
                * curPos: the current position (the id of the seat at the table)
                * maxBet: the current max bet that has been made in either 
                    TablePreFlop, TableFlop, TableTurn, TableRiver
                    or TableShowdown.
        REPRESENTATION INVARIANT:
            TableBet (nextState, betType, startPos, curPost, maxBet):
                maxBet > 0
    *)
    type tableState

    (*  REPRESENTATION CONVENTION:
        playerState has several different representations telling us
        the in-game state of the player:
            - PlayerIdle: the player sits at the table, but is not playing
            - PlayerInGame: the player participates in the current game
            - PlayerFolded: the player has folded in the current game
            - PlayerAllIn: the player went all in in the current game
        REPRESENTATION INVARIANT:
            (none)
    *)
    type playerState

    (*  REPRESENTATION CONVENTION:
        tableEvent has several different representations used for signaling
        different events happening at a table:
            - PlayerJoined (player, seatId): player p has joined the table
            (player holds info about what table it is) at the seat with
            id seatId.
            - PlayerLeaving (player, seatId): player p is leaving the table
            and at the moment is sitting at the seat with id seatId.
            - PlayerFold p: player p has folded
            - PlayerRaise (p, i): player p has raised with i
            - PlayerCall p: player p has called tha last bet
            - StateChanged s: the state has been changed to s
        REPRESENTATION INVARIANT:
            (none)
    *)
    type tableEvent

    (*  REPRESENTATION CONVENTION:
        parsedMessage represents a parsed message from the client and consists
        of the event name, the reference id and the actual body of the message.
        REPRESENTATION INVARIANT:
            (none)
    *)
    type parsedMessage

    
    (*
        players
        TYPE: game ref list ref
K    *)
    val players             : game ref list ref
    
    (*
        boards
        TYPE: game ref list ref
    *)
    val boards              : game ref list ref
    
    (*
        playerIndex
        TYPE: game ref vector ref
    *)
    val playerIndex         : game ref vector ref
    
    (*
        boardIndex
        TYPE: game ref vector ref
    *)
    val boardIndex          : game ref vector ref
    
    (*
        timers
        TYPE: timer list ref 
    *)
    val timers              : timer list ref 

    
    (*
        delay f e
        TYPE: (unit -> unit) -> int -> timer
        PRE: (none)
        POST: Timer with f as function to call after expire time e has passed
        SIDE-EFFECTS: created Timer gets inserted into timers
        EXAMPLE: delay (fn _ => print "hello world!\n") 1000 =
            It gets called after about 1000ms (depending on the frequency of
            the tickHandler). 
    *)
    val delay               : (unit -> unit) -> int -> timer
    
    (*
        processTimers ()
        TYPE: unit -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: It processes all the timers in 'timers' and check if any of
            them has expired. If so, it calls all those expired timers and
            removes them from 'timers'.
        EXAMPLE: 
    *)
    val processTimers       : unit -> unit
    
    (*
        cancelTimer t
        TYPE: timer -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: It removes timer t from 'timers'.
        EXAMPLE: 
    *)
    val cancelTimer         : timer -> unit

    
    (*
        samePlayer (p1, p2)
        TYPE: game ref * game ref -> bool
        PRE: p1 = p2 = Player
        POST: p1 = p2
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    (* WARNING: Deprecated *)
    val samePlayer          : game ref * game ref -> bool
    
    (*
        samePlayerC p1 p2 
        TYPE: game ref -> game ref -> bool
        PRE: (none)
        POST: p1 = p2
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    (* TODO: this curried version should replace the old, regular one *)
    val samePlayerC         : game ref -> game ref -> bool
    
    (*
        getFreeId v
        TYPE: game ref vector -> int option
        PRE: (none)
        POST: SOME id for the first empty index in v, NONE if none found
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getFreeId           : game ref vector -> int option
    
    (*
        getFreePlayerId ()
        TYPE: unit -> int option
        PRE: (none)
        POST: SOME id for the first empty index in `players`, NONE if none found
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getFreePlayerId     : unit -> int option
    
    (*
        getFreeBoardId
        TYPE: unit -> int option
        PRE: (none)
        POST: SOME id for the first empty index in `boards`, NONE if none found
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getFreeBoardId      : unit -> int option
    
    (*
        parseMessage v
        TYPE: Word8Vector.vector -> parsedMessage
        PRE: (none)
        POST: parsedMessage from v
        SIDE-EFFECTS: it raises InvalidMessage exception if the message
            in v has incorrect format.
        EXAMPLE: 
    *)
    val parseMessage        : Word8Vector.vector -> parsedMessage
    
    (*
        filterServerPlayers f
        TYPE: (game -> bool) -> game ref list
        PRE: (none)
        POST: Elements in `players` that pass filter f.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val filterServerPlayers : (game -> bool) -> game ref list
    
    (*
        filterBoardPlayers b f
        TYPE: game ref -> (game -> bool) -> game ref list
        PRE: b = Board
        POST: Spectators in b that pass filter f.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val filterBoardPlayers  : game ref -> (game -> bool) -> game ref list
    
    (*
        filterBoardChairs b f
        TYPE: game ref -> (game -> bool) -> game ref list
        PRE: b = Board
        POST: Players at the table in b that pass filter f.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val filterBoardChairs   : game ref -> (game -> bool) -> game ref list

    (*
        filterNull g
        TYPE: game -> bool
        PRE: (none)
        POST: true if g is Null, false otherwise
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val filterNull          : game -> bool
    
    (*
        filterNotNull g
        TYPE: game -> bool
        PRE: (none)
        POST: false if g is Null, true otherwise
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val filterNotNull       : game -> bool

    (*
        filterInGame p
        TYPE: game -> bool
        PRE: (none)
        POST: true if p is currently in in-game (sitting at a table
            and playing), false otherwise
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val filterInGame   : game -> bool
    
    (*
        filterAll _
        TYPE: game -> bool
        PRE: (none)
        POST: true
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val filterAll           : game -> bool
    
    (*
        filterPlayer p1 p2
        TYPE: game -> game -> bool
        PRE: (none)
        POST: true if p1 = p2, false otherwise
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val filterPlayer        : game -> game -> bool
    
    (*
        filterOthers p1 p2
        TYPE: game -> game -> bool
        PRE: (none)
        POST: true if p1 <> p2, false otherwise
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val filterOthers        : game -> game -> bool
    
    (*
        filterConnection c p
        TYPE: WebsocketServer.connection -> game -> bool
        PRE: (none)
        POST: true if connection of p = c, false otherwise 
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    (* INFO: Used for looking for a player by connection handle. *)
    val filterConnection    : WebsocketServer.connection -> game -> bool
    
    (*
        send l e f d
        TYPE: game ref list -> string -> (game -> bool) -> JSON.T -> unit
        PRE: Every element in l shoud be Player.
        POST: (none)
        SIDE-EFFECTS: It sends message d with event e to all elements in l
            that pass filter f.
        EXAMPLE: 
    *)
    val send                : game ref list -> string -> (game -> bool) -> JSON.T -> unit
    
    (*
        sendToAll e f d
        TYPE: string -> (game -> bool) -> JSON.T -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: It sends message d with event e to all players in 
            `players` that pass filter f.
        EXAMPLE: 
    *)
    val sendToAll           : string -> (game -> bool) -> JSON.T -> unit
    
    (*
        sendToBoard b e f d
        TYPE: game ref -> string -> (game -> bool) -> JSON.T -> unit
        PRE: b = Board
        POST: (none)
        SIDE-EFFECTS: It sends message d with event f to all players spectating
            board b and pass filter function f.
        EXAMPLE: 
    *)
    val sendToBoard         : game ref -> string -> (game -> bool) -> JSON.T -> unit
    
    (*
        sendClientResponse r c d
        TYPE: string -> WebsocketServer.connection -> JSON.T -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Sends response d with reference id r to client c.
        EXAMPLE: 
    *)
    val sendClientResponse  : string -> WebsocketServer.connection -> JSON.T -> unit
    
    (*
        sendResponse r p d
        TYPE: string -> game ref -> JSON.T -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Sends response d with reference id r to player p.
        EXAMPLE: 
    *)
    val sendResponse        : string -> game ref -> JSON.T -> unit

    
    (*
        setPlayerState (p, s)
        TYPE: game ref * playerState -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Sets state of palyer p to s.
        EXAMPLE: 
    *)
    val setPlayerState      : game ref * playerState -> unit
    
    (*
        getPlayer c
        TYPE: WebsocketServer.connection -> game ref option
        PRE: (none)
        POST: SOME player in `players` there player's connection handle = c, 
            NONE if none found.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getPlayer           : WebsocketServer.connection -> game ref option
    
    (*
        getPlayerId p
        TYPE: game ref -> int
        PRE: p = Player
        POST: id of player p
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getPlayerId         : game ref -> int
    
    (*
        getPlayerById id
        TYPE: int -> game ref
        PRE: (none)
        POST: Player in `players` with id `id`.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getPlayerById       : int -> game ref
    
    (*
        getPlayerName p
        TYPE: game ref -> string
        PRE: p = Player
        POST: The name of player p.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getPlayerName       : game ref -> string
    
    (*
        getMoney p
        TYPE: game ref -> int
        PRE: (none)
        POST: The money of player p.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getMoney            : game ref -> int
    
    (*
        changeMoney (p, a)
        TYPE: game ref * int -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Adds a to player p's money.
        EXAMPLE: 
    *)
    val changeMoney         : game ref * int -> unit
    
    (*
        serializePlayer p d
        TYPE: game ref -> JSON.T -> JSON.T
        PRE: p = Player
        POST: Serialized player p with extra data d.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val serializePlayer     : game ref -> JSON.T -> JSON.T
    
    (*
        syncBoard p
        TYPE: game ref -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Sends the state of the board to player p.
        EXAMPLE: 
    *)
    val syncBoard           : game ref -> unit
    
    (*
        syncPlayer p
        TYPE: game ref -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Sends the state of player p to all the other players
            at the board he/she is at.
        EXAMPLE: 
    *)
    val syncPlayer          : game ref -> unit
    
    (*
        isInRoom p
        TYPE: game ref -> bool
        PRE: p = Player
        POST: true if p is spectating any board, false otherwise
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val isInRoom            : game ref -> bool
    
    (*
        getChairIndexByPlayer b p 
        TYPE: game ref -> game ref -> int option
        PRE: p = Player, b = Board
        POST: SOME id of the seat the player p is in at table b,
            NONE if player is not sitting anywhere at table b.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getChairIndexByPlayer : game ref -> game ref -> int option
    
    (*
        setMoney (p, a)
        TYPE: game ref * int -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Sets p's money to a, sends it to p.
        EXAMPLE: 
    *)
    val setMoney            : game ref * int -> unit
    
    (*
        changeStake (p, a)
        TYPE: game ref * int -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Adds a to p's stake.
        EXAMPLE: 
    *)
    val changeStake         : game ref * int -> unit
    
    (*
        setStake (p, a)
        TYPE: game ref * int -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Sets p's stake to a.
        EXAMPLE: 
    *)
    val setStake            : game ref * int -> unit
    
    (*
        getStake p
        TYPE: game ref -> int
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Stake of player p.
        EXAMPLE: 
    *)
    val getStake            : game ref -> int
    
    (*
        getName p
        TYPE: game ref -> string
        PRE: p = Player
        POST: Name of player p.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getName             : game ref -> string
    
    
    (*
        createBoard (n, s, (sb, bb), (maxB, minB))
        TYPE: string * int * (int * int) * (int * int) -> unit
        PRE: (none)
        POST: Creates a Board with name n, size s, small blind sb, big blind bb,
            max buy-in maxB and min buy-in minB.
        SIDE-EFFECTS: Inserts created Board into `boards`.
        EXAMPLE: 
    *)
    val createBoard         : string * int * (int * int) * (int * int) -> unit
    
    (*
        spectateTable (p, id)
        TYPE: game ref * int -> game ref
        PRE: A Board with id `id` must exist in `boards`, p = Player.
        POST: A Board player started spectating.
        SIDE-EFFECTS: Player p starts spectaing a board with id `id`.
        EXAMPLE: 
    *)
    val spectateTable       : game ref * int -> game ref
    
    (*
        unspectateTable (p, f)
        TYPE: game ref * bool -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Player p gets removed from the spectators of 
            currently spectated board, but only if he/she is not in-game 
            (at the table and playing). If f = true, the player will be forced
            to unspectate the table, even if currently playing.
        EXAMPLE: 
    *)
    val unspectateTable     : game ref * bool -> unit
    
    (*
        joinTable (p, b, id)
        TYPE: game ref * game ref * int -> int option
        PRE: b = Board, p = Player
        POST: SOME id there id is the id of the seat if existing,
            NONE if none seat with id `id` was found.
        SIDE-EFFECTS: p joins b at the seat with id `id`.
        EXAMPLE: 
    *)
    val joinTable           : game ref * game ref * int -> int option
    
    (*
        leaveTable (p, f)
        TYPE: game ref * bool -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Player p gets removed from the seat of 
            currently spectated board, but only if he/she is not in-game 
            (at the table and playing). If f = true, the player will be forced
            to leave the table, even if currently playing.
        EXAMPLE: 
    *)
    val leaveTable          : game ref * bool -> unit
    
    (*
        getChair (b, i)
        TYPE: game ref * int -> game ref option
        PRE: b = Board
        POST: SOME Player sitting in a chair with id i at table b,
            NONE if the seat wasn't found.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getChair            : game ref * int -> game ref option
    
    (*
        getTakenChairsCount b
        TYPE: game ref -> int
        PRE: b = Board
        POST: Count of currently taken seats.
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getTakenChairsCount : game ref -> int
    
    (*
        getInGamePlayersCount b
        TYPE: game ref -> int
        PRE: b = Board
        POST: Count of currently in-game players (at the table, not idle,
            not folded).
        SIDE-EFFECTS: (none)
        EXAMPLE: 
    *)
    val getInGamePlayersCount : game ref -> int
    
    (*
        takeCard b
        TYPE: game ref -> Word32.word
        PRE: b = Board
        POST: A card from the deck of b.
        SIDE-EFFECTS: One card is removed from the deck of b.
        EXAMPLE: 
    *)
    val takeCard            : game ref -> Word32.word
    
    (*
        cardOnTable (b, c)
        TYPE: game ref * Word32.word -> unit
        PRE: b = Board
        POST: (none)
        SIDE-EFFECTS: Card c is put on the table b.
        EXAMPLE: 
    *)
    val cardOnTable         : game ref * Word32.word -> unit
    
    (*
        cardToPlayer (p, c)
        TYPE: game ref * Word32.word -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Player p gets card c.
        EXAMPLE: 
    *)
    val cardToPlayer        : game ref * Word32.word -> unit
    
    (*
        updatePots b
        TYPE: game ref -> unit
        PRE: b = Boards
        POST: (none)
        SIDE-EFFECTS: List of all pots at table b is sent to all
            the spectators of b.
        EXAMPLE: 
    *)
    val updatePots          : game ref -> unit
    
    (*
        createPlayer (c, n)
        TYPE: WebsocketServer.connection * string -> game ref option
        PRE: (none)
        POST: SOME player if an empty spot in `players` was found,
            NONE otherwise.
        SIDE-EFFECTS: If an empty spot in `players` was found, the player
            gets inserted at that spot.
        EXAMPLE: 
    *)
    val createPlayer        : WebsocketServer.connection * string -> game ref option
    
    (*
        handleConnect c
        TYPE: WebsocketServer.connection -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Connection c gets inserted into `clients`.
        EXAMPLE: 
    *)
    val handleConnect       : WebsocketServer.connection -> unit
    
    (*
        handleDisconnect c
        TYPE: WebsocketServer.connection -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Connection c gets removed from `clients`. We perform
            even various cleanups, such as checkin if player was logged in
            and if so, removing him/her from current game, saving money
            and so on.
        EXAMPLE: 
    *)
    val handleDisconnect    : WebsocketServer.connection -> unit
    
    (*
        handleMessage (c, o, v)
        TYPE: WebsocketServer.connection * int * Word8Vector.vector -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Handles message v from client c depending on opcode o.
        EXAMPLE: 
    *)
    val handleMessage       : WebsocketServer.connection * int * Word8Vector.vector -> unit
    
    (*
        handleTableEvent (b, e)
        TYPE: game ref * tableEvent -> unit
        PRE: b = Board
        POST: (none)
        SIDE-EFFECTS: Processes event e for table b.
        EXAMPLE: 
    *)
    val handleTableEvent    : game ref * tableEvent -> unit
    
    (*
        handleEvent (p, m)
        TYPE: game ref * parsedMessage -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Processes message m for player p.
        EXAMPLE: 
    *)
    val handleEvent         : game ref * parsedMessage -> unit
    
    (*
        handleCommand (p, c, a)
        TYPE: game ref * string * string -> unit
        PRE: p = Player
        POST: (none)
        SIDE-EFFECTS: Processes command c with arguments a from player p.
        EXAMPLE: 
    *)
    val handleCommand       : game ref * string * string -> unit
    
    (*
        tick
        TYPE: unit -> unit
        PRE: (none)
        POST: (none)
        SIDE-EFFECTS: Processes timers.
        EXAMPLE: 
    *)
    val tick                : unit -> unit
end

structure MLHoldemServer :> WebsocketHandler = 
struct
    exception InvalidMessage
    exception InvalidCommand

    datatype timer =
        Timer of int * (unit -> unit) * Time.time
      | NullTimer

    datatype betType =
        BetNormal
      | BetSmallBlind
      | BetBigBlind

    datatype tableState =
        TableIdle
      | TablePreFlop
      | TableFlop
      | TableTurn
      | TableRiver
      | TableShowdown
      | TableBet of tableState * betType * int * int * int (* next state after bet, betType, starting position, current position, max bet *)

    datatype playerState =
        PlayerIdle 
      | PlayerInGame
      | PlayerFolded
      | PlayerAllIn

    datatype game = 
        Board of {
            id: int,
            name: string,
            smallBlind: int,
            bigBlind: int,
            minBuyIn: int,
            maxBuyIn: int,
			sidePotList: sidepot list ref,
            chairs: game ref vector ref,
            state: tableState ref,
            deck: Word32.word queue ref,
            cards: Word32.word list ref,
            spectators: game ref list ref,
            pot: int ref,
            betTimer: timer ref,
            startTimer: timer ref
        }
      | Player of {
            id: int,
            name: string ref,
            board: game ref ref,
            connection: WebsocketServer.connection,
            state: playerState ref,
            cards: Word32.word list ref,
            money: int ref,
            stake: int ref
        }
      | Null

    datatype tableEvent = 
        PlayerJoined of game ref * int
      | PlayerLeaving of game ref * int
      | PlayerFold of game ref
      | PlayerRaise of game ref * int
      | PlayerCall of game ref
      | StateChanged of tableState

    type parsedMessage = string * string * JSON.T

    val maxTimerId = ref 0;
    val clients = ref [];
    val players = ref [];
    val boards = ref [];
    val timers = ref [];
    val playerIndex = ref (Vector.tabulate(1024, fn _ => ref Null));
    val boardIndex = ref (Vector.tabulate(128, fn _ => ref Null));

    fun delay f e =
        let 
            val timer = Timer (!maxTimerId, f, Time.now () + (Time.fromSeconds e))
        in
            timers := timer::(!timers);
            maxTimerId := (!maxTimerId) + 1;
            timer
        end

    fun processTimers () =
        let
            val now = Time.now ()
            val expiredTimers = filter (fn (Timer (_, _, ex)) => now >= ex) (!timers)
        in
            List.app (fn (tt as Timer (_, f, _)) => (print "processing timer\n"; PolyML.print tt; f ())) expiredTimers;
            timers := filter (fn (t as Timer (_, _, ex)) => now < ex) (!timers)
        end

    fun cancelTimer (Timer (id, _, _)) =
        timers := List.filter (fn Timer (x, _, _) => id <> x) (!timers)
      | cancelTimer _ = ()

    fun setPlayerState (ref (Player {state=state, ...}), s) =
        state := s

    fun getPlayerById id =
        Vector.sub (!playerIndex, id)

    fun samePlayer (ref (Player {connection=c1, ...}), ref (Player {connection=c2, ...})) =
        WebsocketServer.sameConnection (c1, c2)
      | samePlayer (_, _) = false

    fun samePlayerC p1 p2 =
        samePlayer (p1, p2)

    fun filterNull Null = true
      | filterNull _ = false

    fun filterInGame (Player {state=ref PlayerInGame, ...}) = true
      | filterInGame (Player {state=ref PlayerAllIn, ...}) = true
      | filterInGame _ = false

    fun filterNotNull x = not (filterNull x)

    fun filterAll x = 
        true

    fun filterRefList l f =
        List.filter (fn (ref x) => f x) l

    fun filterServerPlayers f =
        filterRefList (!players) f 

    fun filterBoardPlayers (ref (Board {spectators=spectators, ...})) f =
        filterRefList (!spectators) f
      | filterBoardPlayers (_) _ = []

    fun filterBoardChairs (ref (Board {chairs=chairs, ...})) f =
        filterRefList (nvectorToList (!chairs)) f

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

    fun getChairIndexByPlayer (ref (Board {chairs=chairs, ...})) p =
        let
            val id = Vector.findi (fn (_, ref x) => not (filterNull x) andalso filterPlayer x (!p)) (!chairs)
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
            val players = filterServerPlayers (filterConnection c)
        in
            if null players then
                NONE
            else
                SOME (hd players)
        end

    fun getPlayerId (ref (Player {id=id, ...})) =
        id

    fun getPlayerName (ref (Player {name=ref name, ...})) =
        name


    fun send l e f d =
        let
            val players = filterRefList l f
            val d = JSON.empty
                 |> JSON.update ("event", JSON.String e)
                 |> JSON.update ("data", d)
            val d = Byte.stringToBytes (JSON.encode d)
        in
            List.app (fn (ref (Player {connection=connection, ...})) => WebsocketServer.send (connection, 1, d)) players
        end

    fun sendToAll e f d =
        send (filterServerPlayers filterAll) e f d 

    fun sendToBoard b e f d =
        let in
            send (filterBoardPlayers b filterAll) e f d 
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

    fun sendResponse r (ref (Player {connection=c, ...})) d =
        sendClientResponse r c d
    
    fun createPlayer (connection, name) =
        let
            val exists = List.exists (fn (ref (Player {name=ref n, ...})) => n = name) (!players)
            val id = getFreePlayerId ()
        in
            if exists orelse id = NONE then
                NONE
            else
                let 
                    val player = (ref (Player {
                        id=valOf id, 
                        name=ref name, 
                        board=ref (ref Null), 
                        connection=connection, 
                        state=ref PlayerIdle,
                        cards=ref [],
                        money=ref 0,
                        stake=ref 0
                    }))
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
				sidePotList=ref [],
                chairs=ref (Vector.tabulate(s, fn _ => ref Null)),
                state=ref TableIdle,
                deck=ref empty,
                cards=ref [],
                spectators=ref [],
                pot=ref 0,
                betTimer=ref NullTimer,
                startTimer=ref NullTimer
            })
        in
            boards := board::(!boards);
            boardIndex := Vector.update (!boardIndex, valOf id, board)
        end

    fun getMoney (ref (Player {money=ref money, ...})) =
        money
      | getMoney (ref (Board {pot=ref pot, ...})) =
        pot

    fun changeMoney (player as (ref (Player _)), amount) =
        let 
            val money = getMoney (player) + amount
        in
            setMoney (player, money)
        end
      | changeMoney (board as (ref (Board _)), amount) =
        let 
            val pot = getMoney (board) + amount
        in
            setMoney (board, pot)
        end

    and setMoney (player as ref (Player {money=money, ...}), amount) =
        let 
            val _ = money := amount;
            val d = JSON.empty
                 |> JSON.add ("money", JSON.Int (!money))
            val playerStr = getPlayerName(player)
        in            
            (db_updateMoney(playerStr, amount);sendToAll "update_money" (filterPlayer (!player)) d)
        end
      | setMoney (board as ref (Board {pot=pot, ...}), amount) =
        let
            val _ = pot := amount; 
            val d = JSON.empty
                 |> JSON.add ("pot", JSON.Int (!pot))
        in
            sendToBoard board "update_pot" filterAll d
        end
    
    fun getStake (player as ref (Player {stake=ref stake, ...})) =
        stake

    fun getName (player as ref (Player {name=ref name, ...})) =
        name

    fun setStake (player as ref (Player {stake=stake, board=ref board, ...}), amount) =
        let
            val chairIndex = valOf (getChairIndexByPlayer board player)
            
            val d  = JSON.empty
                  |> JSON.add ("seat", JSON.Int chairIndex)
                  |> JSON.add ("stake", JSON.Int amount)
        in
            sendToBoard board "update_stake" filterAll d;
            stake := amount
        end

    fun changeStake (player as ref (Player {stake=stake, ...}), amount) =
        setStake (player, (!stake) + amount)

    fun handleConnect (c) =
        let in
            clients := c::(!clients)
        end

    fun parseMessage (m) =
        let
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
                 |> JSON.add ("username", JSON.String "Server")
                 |> JSON.add ("chatType", JSON.String "server")
        in
            sendToAll "server_message" (filterPlayer (!player)) d
        end

    fun tableMessage (board as ref (Board _), message) =
        let
            val d = JSON.empty
                 |> JSON.add ("message", JSON.String (message))
                 |> JSON.add ("username", JSON.String "Table")
                 |> JSON.add ("chatType", JSON.String "table")
        in
            sendToBoard board "server_message" filterAll d
        end
      | tableMessage (player as ref (Player _), message) =
        let
            val d = JSON.empty
                 |> JSON.add ("message", JSON.String (message))
                 |> JSON.add ("username", JSON.String "Table")
                 |> JSON.add ("chatType", JSON.String "table")
        in
            send [player] "server_message" filterAll d
        end

    fun serializePlayer (player as ref (Player {cards=ref cards, ...})) d =
        let 
            fun hasFolded (ref (Player {state=ref PlayerFolded, ...})) = true
              | hasFolded _ = false

            fun isAllIn (ref (Player {state=ref PlayerAllIn, ...})) = true
              | isAllIn _ = false

            val cards = map (fn x => JSON.String (eval_printCard x)) cards
            val cards = map (fn _ => JSON.String "reverse") cards
            val du = JSON.empty
                  |> JSON.add ("id", JSON.Int (getPlayerId player))
                  |> JSON.add ("username", JSON.String (getPlayerName player))
                  |> JSON.add ("avatar", JSON.String "http://2.bp.blogspot.com/-RatTLFiu6J4/T5l_v59jbVI/AAAAAAAAQ2A/kelVxm_vcLI/s400/blank_avatar_220.png")
                  |> JSON.add ("stake", JSON.Int (getStake player))
                  |> JSON.add ("allin", JSON.Bool (isAllIn player))
                  |> JSON.add ("cards", JSON.List cards)
                  |> JSON.add ("folded", JSON.Bool (hasFolded player))
        in
            JSON.add ("user", du) d
        end

    fun syncBoard (player as (ref (Player {board=ref board, ...}))) =
        let
            fun sendSerializedPlayer u (i, ref Null) = ()
              | sendSerializedPlayer u (i, p) =
                let
                    val d = JSON.empty
                         |> JSON.add ("seat", JSON.Int i)
                         |> serializePlayer p
                in
                    send [u] "user_join" filterAll d
                end
        in
            case board of
                (ref (Board {chairs=chairs, ...})) =>
                        Vector.appi (sendSerializedPlayer player) (!chairs)
              | _ => ()
        end

    fun syncPlayer (player as (ref (Player {board=ref board, ...}))) =
        let 
            fun sendSerializedPlayer board p =
                let
                    val d = JSON.empty
                         |> JSON.add ("seat", JSON.Int (valOf (getChairIndexByPlayer board p)))
                         |> serializePlayer p
                in
                    sendToBoard board "user_join" filterAll d
                end
        in
            sendSerializedPlayer board player
        end

    fun isInRoom (ref (Player {board=ref (ref (Board _)), ...})) = true
      | isInRoom _ = false

    fun unspectateTable (player as (ref (Player {board=board, ...})), force) =
        let in
            case board of
                ref (ref (Board {spectators=spectators, ...})) =>
                    if not (filterInGame (!player)) orelse force then
                    let in
                        spectators := filterBoardPlayers (!board) (filterOthers (!player));
                        board := ref Null
                    end
                    else
                        ()
              | _ => ()
        end

    fun spectateTable (player as (ref (Player {board=board, ...})), id) =
        let
            val b = Vector.sub (!boardIndex, id)
        in
            unspectateTable (player, false);
            board := ref (!b);

            case board of
                ref (ref (Board {spectators=spectators, ...})) =>
                let 
                    val alreadySpectating = exists (samePlayerC player) (!spectators)
                in
                    if alreadySpectating then
                        ()
                    else
                        spectators := player::(!spectators)
                end
              | _ => ();

            (!board)
        end

    fun getTakenChairsCount (ref (Board {chairs=ref chairs, ...})) =
        let
            val chairs = nvectorToList chairs
            val freeChairs = filterRefList chairs filterNull
        in
            (length chairs) - (length freeChairs)
        end

    fun getInGamePlayersCount (ref (Board {chairs=ref chairs, ...})) =
        let
            val chairs = nvectorToList chairs
        in
            (length (filterRefList chairs filterInGame))
        end 

    fun takeCard (ref (Board {cards=cards, deck=deck, ...})) =
        let 
            val card = (head (!deck))
        in
            deck := dequeue (!deck);
            card
        end

    fun cardOnTable (board as ref (Board {cards=cards, ...}), card) =
        let 
            val d1 = JSON.empty
                  |> JSON.add ("id", JSON.String (eval_printCard card))
        in
            cards := card::(!cards);

            sendToBoard board "new_card" filterAll d1
        end

    fun cardToPlayer (player as ref (Player {cards=cards, board=ref board, ...}), card) =
        let 
            val chairIndex = valOf (getChairIndexByPlayer board player)
            
            val d2 = JSON.empty
                  |> JSON.add ("seat", JSON.Int chairIndex)
                  |> JSON.add ("id", JSON.String "reverse")

            val d1 = JSON.empty
                  |> JSON.add ("seat", JSON.Int chairIndex)
                  |> JSON.add ("id", JSON.String (eval_printCard card))
        in
            cards := card::(!cards);

            sendToBoard board "new_player_card" (filterOthers (!player)) d2;
            send [player] "new_player_card" filterAll d1
        end

    fun updatePots (board as ref (Board {sidePotList=ref sidePotList, ...})) =
        let 
            val pots = sh_sumPots (sidePotList)
            val pots = map (fn (nr, sum) => JSON.empty |> JSON.add ("potId", JSON.Int nr) |> JSON.add ("amount", JSON.Int sum)) pots
            val d = JSON.empty
                 |> JSON.add ("pots", JSON.List pots)
        in
            sendToBoard board "update_pots" filterAll d
        end

    fun getChair (ref (Board {chairs=chairs, ...}), id) =
        Vector.sub (!chairs, id) handle Subscript => ref Null


    fun leaveTable (player as (ref (Player {board=board as ref (ref (Board {chairs=chairs, ...})), ...})), force) =
        let
            val index = getChairIndexByPlayer (!board) player
        in
            case index of
                SOME index => 
                    let in
                        if not (filterInGame (!player)) orelse force then
                        let in
                            handleTableEvent (!board, PlayerLeaving (player, index));
                            chairs := Vector.update (!chairs, index, ref Null)
                        end
                        else
                            ()
                    end
              | _ => ()
        end
      | leaveTable (_, _) = ()

    and handleTableEvent (board as (ref (Board {chairs=chairs, state=ref state, startTimer=startTimer, ...})), PlayerJoined (player, chairId)) =
        let
            val playersCount = getTakenChairsCount board
            val ref (Player {state=playerState, ...}) = player

            val d1 = JSON.empty
                  |> JSON.add ("seat", JSON.Int chairId)
                  |> serializePlayer (player)
        in
            playerState := PlayerIdle;
            sendToBoard board "user_join" filterAll d1;
            setStake (player, 0);

            case state of
                TableIdle =>
                let in
                    (* Enough players at the table? *)
                    if playersCount >= 2 then
                        let in
                            tableMessage (board, "Starting in 15 seconds.");
                            cancelTimer (!startTimer);
                            startTimer := delay (fn _ => setTableState (board, TablePreFlop)) 15;
                            ()
                        end
                    else
                        (* nope :( you are forever alone *)
                        ()
                end
              | _ => ()
        end
      
      | handleTableEvent (board as (ref (Board {chairs=chairs, bigBlind=bigBlind, smallBlind=smallBlind, sidePotList=sidePotList, deck=deck, cards=cards, ...})), StateChanged (TablePreFlop)) = 
            let 
                val tmpChairs = nvectorToList (!chairs)
                val players = filterRefList tmpChairs filterNotNull

                (* kick people who can't afford playing *)
                val _ = List.app (fn p => if (getMoney p >= bigBlind) then () else leaveTable (p, false)) players

                val chairs = nvectorToList (!chairs)
                val players = filterRefList chairs filterNotNull

                val playersCount = getTakenChairsCount board
            in
                (* Enough players at the table? *)
                if playersCount >= 2 then
                    let 
                        val newDeck = shuffleDeck 51
						val startSidepot = [sh_emptySidepot]

                        (*
                            resetPlayer p
                            TYPE: game ref -> unit
                            PRE: p = Player
                            POST: (none)
                            SIDE-EFFECTS: Reset table specific data in p
                                and resynchronizes the state of p
                                with other players at the board p is sitting at.
                        *)
                        fun resetPlayer (p as ref (Player {cards=cards, state=state, ...})) =
                            let in
                                setPlayerState (p, PlayerIdle);
                                setStake (p, 0);
                                cards := [];
                                syncPlayer p
                            end
                    in
                        List.app resetPlayer players;
                        sendToBoard board "cleanup" filterAll (JSON.empty |> JSON.add ("status", JSON.String "ok"));

                        List.app (fn (ref (Player {state=state, ...})) => state := PlayerInGame) players;

                        tableMessage (board, "Shuffling cards, drinking beer. Pre-flop coming.");

                        (* reset table *)
						sidePotList := startSidepot;
                        deck := newDeck;
                        cards := [];
                        setMoney (board, 0);

                        (* distribute two cards to each player at the table *)
                        List.app (fn p => (cardToPlayer (p, takeCard board); cardToPlayer (p, takeCard board))) players;
                        
                        setTableState (board, TableBet (TableFlop, BetSmallBlind, 0, 0, smallBlind))
                    end
                else
                    setTableState (board, TableIdle)
            end

      | handleTableEvent (board as (ref (Board {chairs=chairs, ...})), StateChanged (TableIdle)) = 
        sendToBoard board "cleanup" filterAll (JSON.empty |> JSON.add ("status", JSON.String "ok"))

      | handleTableEvent (board as (ref (Board {chairs=chairs, ...})), StateChanged (TableFlop)) = 
            let 
                fun sendBestHand (ref (Board {cards=bcards, ...})) (player as ref (Player {cards=pcards, id=id, state=ref PlayerInGame, ...})) =
                    let
                        val ref [c1, c2] = pcards
                        val ref [c3, c4, c5] = bcards
                        val rank = eval_5cards (c1, c2, c3, c4, c5)
                        val rank = printHand (handRank rank)

                        val d1 = JSON.empty
                              |> JSON.add ("hand", JSON.String rank)
                    in
                        send [player] "best_hand" filterAll d1
                    end
                  | sendBestHand _ _ = ()

                val chairs = nvectorToList (!chairs)
                val players = filterRefList chairs filterNotNull (* TODO: STATE *)
            in
                cardOnTable (board, takeCard (board));
                cardOnTable (board, takeCard (board));
                cardOnTable (board, takeCard (board));

                List.app (sendBestHand board) players;

                setTableState (board, TableBet (TableTurn, BetNormal, 0, 0, 0))
            end

      | handleTableEvent (board as (ref (Board {chairs=chairs, ...})), StateChanged (TableTurn)) = 
            let 
                fun sendBestHand (ref (Board {cards=bcards, ...})) (player as ref (Player {cards=pcards, id=id, state=ref PlayerInGame, ...})) =
                    let
                        val ref [c1, c2] = pcards
                        val ref [c3, c4, c5, c6] = bcards
                        val rank = eval_6hand (c1, c2, c3, c4, c5, c6)
                        val rank = printHand (handRank rank)

                        val d1 = JSON.empty
                              |> JSON.add ("hand", JSON.String rank)
                    in
                        send [player] "best_hand" filterAll d1
                    end
                  | sendBestHand _ _ = ()

                val chairs = nvectorToList (!chairs)
                val players = filterRefList chairs filterInGame
            in
                cardOnTable (board, takeCard (board));

                List.app (sendBestHand board) players;

                setTableState (board, TableBet (TableRiver, BetNormal, 0, 0, 0))
            end

      | handleTableEvent (board as (ref (Board {chairs=chairs, ...})), StateChanged (TableRiver)) = 
            let 
                fun sendBestHand (ref (Board {cards=bcards, ...})) (player as ref (Player {cards=pcards, id=id, state=ref PlayerInGame, ...})) =
                    let
                        val ref [c1, c2] = pcards
                        val ref [c3, c4, c5, c6, c7] = bcards
                        val rank = eval_7hand (c1, c2, c3, c4, c5, c6, c7)
                        val rank = printHand (handRank rank)

                        val d1 = JSON.empty
                              |> JSON.add ("hand", JSON.String rank)
                    in
                        send [player] "best_hand" filterAll d1
                    end
                  | sendBestHand _ _ = ()

                val chairs = nvectorToList (!chairs)
                val players = filterRefList chairs filterInGame
            in
                cardOnTable (board, takeCard (board));

                List.app (sendBestHand board) players;

                setTableState (board, TableBet (TableShowdown, BetNormal, 0, 0, 0))
            end

      | handleTableEvent (board as (ref (Board {chairs=chairs, sidePotList=sidePotList, startTimer=startTimer, ...})), StateChanged (TableShowdown)) = 
            let 
                fun prepareForShowdown (board as ref (Board {cards=bcards, ...}), player as ref (Player {cards=pcards, name=name, id=id, ...})) =
                    let
                        val ref [c1, c2] = pcards
                        val ref [c3, c4, c5, c6, c7] = bcards
                        val ref name = name
                        val rank = eval_7hand (c1, c2, c3, c4, c5, c6, c7)
                        val printRank = eval_print7hand (c1, c2, c3, c4, c5, c6, c7)
                        val printRank = printHand (handRank rank) ^ ", "^ printTypeHand printRank
                        
                        val hand = eval_print7hand(c1, c2, c3, c4, c5, c6, c7);
                        val hand = handToString(hand);
                        val strToChat = name^" shows " ^ hand ^": "^printRank^"."
                    in
                        tableMessage (board, strToChat);
                        (id, rank)
                    end
                val chairs = nvectorToList (!chairs)
                val players = filterRefList chairs filterInGame

                val playerList = map (fn p => prepareForShowdown (board, p)) players
                val ps = showDown (sh_updateHands((!sidePotList), playerList))

                
                (*
                    printShowDown l
                    TYPE:       sidepot list -> string
                    PRE:        (none)
                    POST:       l in text as a string. 
                    EXAMPLE:    showDown([(0, 1, 500), (1, 1, 700), (3, 1600, 2500), (7, 5068, 2000)]) =
                                "0 and 1 split a pot of $1000.\n1 won a pot of $400.\n3 won a pot of $1300.\n": string
                *)
                (*
                    INFO:       Returns information of all the players involved in the sidepot. 
                *)

                fun printShowDown([]) = ""
                | printShowDown(Sidepot(nr, players as (p, h, m)::xs, t, a, f)::rest) = 
                    let 
                        val antPlayers = length players
                        val intStr = Int.toString
                        val gamePlayer = getPlayerName(getPlayerById(p))

                        fun printShowDown'([], t, rest) = 
							if nr = 0 then
								"split the main pot of $"^intStr(t)^".\n"^printShowDown(rest)
							else
								"split side pot "^intStr(nr)^" of $"^intStr(t)^".\n"^printShowDown(rest)
                        | printShowDown'((p', h', m')::xs, t, rest) =
                            let
                                val gamePlayer' = getPlayerName(getPlayerById(p'))
                            in
                                if xs = [] then
                                    gamePlayer'^" "^printShowDown'(xs, t, rest)
                                else
                                    gamePlayer'^" and "^printShowDown'(xs, t, rest)
                            end
                    in
                        if antPlayers = 1 then
							if nr = 0 then
                                gamePlayer^" won the main pot ($"^intStr(t)^").\n"^printShowDown(rest)
							else
								gamePlayer^" won the side pot ($"^intStr(t)^").\n"^printShowDown(rest)
                        else
                            printShowDown'(players, t, rest)
                    end;
                
                val dealerChat = printShowDown ps

                fun sidePotUpd([]) = () 
                | sidePotUpd(Sidepot(nr, players, t, a, f)::spRest) =
                    let
                        fun sidePotUpd'([], spRest') = sidePotUpd(spRest')
                        | sidePotUpd'((p,h,m)::xs, spRest') =
                            let
                                val gamePlayer = getPlayerById(p)
                            in
                                (changeMoney(gamePlayer, m); sidePotUpd'(xs, spRest'))
                            end
                    in
                        sidePotUpd'(players, spRest)
                    end;
                
            in
                sidePotUpd(ps);
                tableMessage (board, dealerChat);
                List.app (fn (ref (Player {state=pstate, ...})) => pstate := PlayerIdle) players;
                ();
                
                (* TODO: we need delay preflop here *)
                (**)
                cancelTimer (!startTimer);
                delay (fn _ => setTableState (board, TablePreFlop)) 15;
                ()
            end

      | handleTableEvent (board as (ref (Board {
            chairs=chairs, 
            bigBlind=bigBlind, 
            smallBlind=smallBlind, 
            betTimer=betTimer,
			sidePotList=sidePotList,
            ...
        })), StateChanged (TableBet (nextState, betType, startPosition, position, maxBet))) = 
            let 
                val realPosition = position mod (Vector.length (!chairs))
                val chair = getChair (board, realPosition)
            in
                if getInGamePlayersCount board = 1 then
                    setTableState (board, nextState)
                else if position - startPosition >= Vector.length (!chairs) then
                    (sidePotList := sh_mkFull(!sidePotList);
                    updatePots board;
					setTableState (board, nextState))
                else
                    let in
                        updatePots board;
                        case chair of
                            player as (ref (Player {state=ref PlayerInGame, ...})) => 
                            let 
                                val chairIndex = valOf (getChairIndexByPlayer board player)
                                val d1 = JSON.empty
                                      |> JSON.add ("time", JSON.Int 30)
                                      |> JSON.add ("seat", JSON.Int chairIndex)
                                      |> JSON.add ("min", JSON.Int maxBet)

                                val (amount, blind, nextBet) = case betType of
                                    BetNormal => (maxBet, false, BetNormal)
                                  | BetSmallBlind => (smallBlind, true, BetBigBlind)
                                  | BetBigBlind => (bigBlind, true, BetNormal)
                            in
                                if blind then
                                    handleTableEvent (board, PlayerCall player)
                                else
                                    let in
                                        betTimer := delay (fn _ => handleTableEvent (board, PlayerFold player)) 30;
                                        sendToBoard board "bet" filterAll d1
                                    end
                            end
                          | _ => setTableState (board, TableBet (nextState, betType, startPosition, position + 1, maxBet))
                    end
            end

      | handleTableEvent (board as (ref (Board {chairs=chairs, state=ref state, ...})), PlayerLeaving (player, chairId)) = 
        let
            val playersCount = getTakenChairsCount board

            val d1 = JSON.empty
                  |> JSON.add ("seat", JSON.Int chairId)
        in
            sendToBoard board "user_leave" filterAll d1;

            case state of
                (* We don't care about players leaving a table where nothing happens (state: Idle ) *)
                TableIdle => ()

                (* this gets called when someone leaves a table with an ongoing game *)
              | _ =>
                let in
                    if playersCount < 2 then
                        (* game is going on, but there are not enough players in the chairs *)
                        ()
                    else
                        ()
                end
        end

        (* fold *)
      | handleTableEvent (board as (ref (Board {
            chairs=chairs, 
            bigBlind=bigBlind, 
            smallBlind=smallBlind, 
            betTimer=betTimer,
            state=ref (TableBet (nextState, betType, startPosition, position, maxBet)),
            ...
        })), PlayerFold (player as (ref (Player ({state=state as (ref PlayerInGame), cards=cards, ...}))))) = 
        let
            val realPosition = position mod (Vector.length (!chairs))
            val chair = getChair (board, realPosition)
        in
            if samePlayer (chair, player) then
                let in
                    cancelTimer (!betTimer);

                    cards := [];
                    state := PlayerFolded;

                    syncPlayer player;

                    setTableState (board, TableBet (nextState, betType, startPosition, position + 1, maxBet))
                end
            else
                serverMessage (player, "Not your turn.")
        end

        (* call *)
      | handleTableEvent (board as (ref (Board {
            chairs=chairs, 
            bigBlind=bigBlind, 
            smallBlind=smallBlind, 
            betTimer=betTimer,
			sidePotList=sidePotList,
            state=ref (TableBet (nextState, betType, startPosition, position, maxBet)),
            ...
        })), PlayerCall (player as (ref (Player ({state=ref PlayerInGame, id=id,...}))))) =
        let
            val amount = maxBet
            val realPosition = position mod (Vector.length (!chairs))
            val chair = getChair (board, realPosition)
        in
            if samePlayer (chair, player) then
                let 
                    val (amount, blind, nextBet) = case betType of
                        BetNormal => (maxBet, false, BetNormal)
                      | BetSmallBlind => (smallBlind, true, BetBigBlind)
                      | BetBigBlind => (bigBlind, true, BetNormal)

                    val tAmount = case nextState of
                        TableFlop => if getStake player <= amount then amount - getStake player else amount
                      | _ => amount

                    (* updated position in case we have a blind here *)
                    val realPosition = position mod (Vector.length (!chairs))
                    val startPosition = if blind then position + 1 else startPosition
                in
                    cancelTimer (!betTimer);

                    changeMoney (player, ~tAmount);
                    changeMoney (board, tAmount);
                    changeStake (player, tAmount);

                    if getMoney (player) <= 0 then
	                    let in
	                        setPlayerState (player, PlayerAllIn);
	                        syncPlayer player;
							sidePotList := sh_mkSidepot((!sidePotList), id, 9999, tAmount, true, false)
	                    end
                    else
                        sidePotList := sh_mkSidepot((!sidePotList), id, 9999, tAmount, false, false);();
						setTableState (board, TableBet (nextState, nextBet, startPosition, position + 1, amount))

                end
            else
                serverMessage (player, "Not your turn.")
        end      

        (* raise *)   
      | handleTableEvent (board as (ref (Board {
            chairs=chairs, 
            bigBlind=bigBlind, 
            smallBlind=smallBlind,
            betTimer=betTimer,
			sidePotList=sidePotList,
            state=ref (TableBet (nextState, betType, startPosition, position, maxBet)),
            ...
        })), x as PlayerRaise (player as (ref (Player ({state=ref PlayerInGame, id=id, ...}))), raiseAmount)) = 
        let
            val realPosition = position mod (Vector.length (!chairs))
            val chair = getChair (board, realPosition)

            val maxBet = maxBet + raiseAmount
            val startPosition = position
        in
            if samePlayer (chair, player) then
                let in
                    cancelTimer (!betTimer);

                    changeMoney (player, ~maxBet);
                    changeMoney (board, maxBet);
                    changeStake (player, maxBet);

                    if getMoney (player) <= 0 then
	                    let in
	                        setPlayerState (player, PlayerAllIn);
	                        syncPlayer player;
						    sidePotList := sh_mkSidepot((!sidePotList), id, 9999, maxBet, true, false)
	                    end
                    else
                        sidePotList := sh_mkSidepot((!sidePotList), id, 9999, maxBet, false, false);();
                    	setTableState (board, TableBet (nextState, betType, startPosition, position + 1, maxBet))
                end
            else
                serverMessage (player, "Not your turn.")
        end

      | handleTableEvent (_, _) =
        print "Not implemented\n"

    and setTableState (board as (ref (Board {state=state, ...})), newState) =
        let in
            state := newState;
            print "new state incoming: ";
            PolyML.print (StateChanged (newState));
            handleTableEvent (board, StateChanged (newState))
        end
      | setTableState (x, s) =
        let in
            print "unrecognized state incoming: ";
            PolyML.print (x);
            PolyML.print (s);
            handleTableEvent (x, StateChanged s)
        end

    fun getChair (ref (Board {chairs=chairs, ...}), id) =
        ((SOME (Vector.sub (!chairs, id))) handle Subscript => NONE)
      | getChair (_, _) = NONE

    fun joinTable (player, board as (ref (Board {chairs=chairs, spectators=spectators, bigBlind=bigBlind, ...})), id) =
        let
            val (ref s) = Vector.sub (!chairs, id)
        in
            if (getMoney player) >= bigBlind then
                case s of
                    Null => 
                        let in
                            chairs := Vector.update (!chairs, id, player);
                            handleTableEvent (board, PlayerJoined (player, id));
                            SOME id
                        end
                  | _ => NONE
            else
            let in
                tableMessage (player, "Not enough money");
                NONE
            end
        end

    fun handleCommand (player, command, arguments) =
        let in
            case command of
                "players" => serverMessage (player, "Players on-line:\n" ^ (implodeStrings "\n" (map getPlayerName (!players))))
              | "gimmemoney" => changeMoney (player, 10000)
              | "rooms" => 
                let 
                    val boards = map (fn (ref (Board {name=name, id=id, ...})) => "+ " ^ name ^ " (ID: " ^ Int.toString (id) ^ ")") (!boards);
                    val message = implodeStrings "\n" boards
                in
                    serverMessage (player, "Rooms:\n" ^ message)
                end
              | "sit" =>
                if validateArguments "d" arguments then
                    let
                        val id = Int.fromString arguments
                        val ref (Player {board=ref board, ...}) = player
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
                                            ()
                                        end
                                  | _ => ()
                            end
                    end
                else
                    serverMessage (player, "/sit [sit id]")
              | "getup" =>
                    let in
                        leaveTable (player, false)
                    end 
              | "fold" =>
                    let
                        val ref (Player {board=ref board, ...}) = player
                    in
                        handleTableEvent (board, PlayerFold player)
                    end
              | "raise" =>
                let in
                    if validateArguments "d" arguments then
                        let
                            val amount = Int.fromString arguments
                            val ref (Player {board=ref board, ...}) = player
                        in
                            handleTableEvent (board, PlayerRaise (player, valOf amount))
                        end
                    else
                        serverMessage (player, "/raise [amount]")
                end
              | "call" =>
                    let
                        val ref (Player {board=ref board, ...}) = player
                    in
                        handleTableEvent (board, PlayerCall player)
                    end
              | _ => raise InvalidCommand
        end

        fun handleEvent (player as (ref (Player {name=ref username, board=ref board, ...})), (e, r, d)) =
        case e of 
            "chat" => 
                let 
                    val d = JSON.update ("username", JSON.String username) d
                in
                    if not (JSON.hasKeys (d, ["message", "chatType"])) then
                        raise InvalidMessage
                    else 
                        let 
                            val message = JSON.toString (JSON.get d "message")
                            val chatType = JSON.toString (JSON.get d "chatType")
                        in
                            if size message = 0 orelse size message > 128 then
                                raise InvalidMessage
                            else
                                (case chatType of
                                    "server" => sendToAll 
                                  | "table" => sendToBoard board
                                  | _ => raise InvalidMessage) "server_message" filterAll d
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
          | "rooms" =>
                let
                    fun serializeBoard (board as ref (Board {
                        name=name, 
                        id=id, 
                        smallBlind=smallBlind,
                        bigBlind=bigBlind,
                        chairs=ref chairs,
                        spectators=ref spectators,
                        ...
                    })) =
                    let 
                        val playersCount = getTakenChairsCount board
                        val spectatorsCount = length spectators - playersCount
                        val r = JSON.empty
                             |> JSON.add ("id", JSON.Int id)
                             |> JSON.add ("name", JSON.String name)
                             |> JSON.add ("sb", JSON.Int smallBlind)
                             |> JSON.add ("bb", JSON.Int bigBlind)
                             |> JSON.add ("maxPlayers", JSON.Int (Vector.length chairs))
                             |> JSON.add ("players", JSON.Int playersCount)
                             |> JSON.add ("spectators", JSON.Int spectatorsCount)
                    in
                        r
                    end

                    val d = JSON.empty
                         |> JSON.add ("rooms", JSON.List (map serializeBoard (!boards)))
                in
                    sendResponse r player d
                end
          | "enter" => 
                let
                in
                    if not (JSON.hasKeys (d, ["id"])) then
                        raise InvalidMessage
                    else if (isInRoom player) then
                        (print "already in room"; raise InvalidMessage)
                    else
                        let 
                            val id = JSON.get d "id"
                        in
                            case id of
                                JSON.Int id => 
                                    let 
                                        val (ref board) = spectateTable (player, id)
                                    in
                                        case board of
                                            Board {name=name, ...} =>
                                            let 
                                                val d = JSON.empty
                                                     |> JSON.add ("status", JSON.String "OK")
                                            in
                                                tableMessage (player, "Welcome to " ^ name ^ ".");
                                                sendResponse r player d
                                            end
                                          | _ => raise InvalidMessage
                                    end
                              | _ => raise InvalidMessage
                        end
                end
          | "sync_board" =>
            let in
                syncBoard player
            end
          | "leave_room" =>
                let in
                    leaveTable (player, false);
                    unspectateTable (player, false)
                end
          | _ => raise InvalidMessage


    fun handleMessage (c, opcode, m) =
        (let
            val others = List.filter (fn x => not(WebsocketServer.sameConnection (c, x))) (!clients) 
            val pm as (e, r, d) = parseMessage m
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
                        val password = JSON.toString (JSON.get d "password")
                        val _ = if size username = 0 then raise InvalidMessage else ()
                    in

                        if db_loginPlayer(username, password) = true then
                            let
                                val player = createPlayer (c, username)
                                val money = db_getMoney(db_findPlayer(username))
                            in
                                case player of
                                    SOME (player) =>
                                        let 
                                            val response = JSON.empty
                                                        |> JSON.add ("id", JSON.Int (getPlayerId (player)))
                                                        |> JSON.add ("status", JSON.String "OK")

                                            val d1 = JSON.empty
                                                  |> JSON.add ("message", JSON.String (username ^ " has connected."))
                                                  |> JSON.add ("username", JSON.String "Server")
                                        in
                                            print "[server]\tClient logged in.\n";
                    
                                            sendClientResponse r c response;

                                            changeMoney (player, money);
                                            sendToAll "server_message" (filterOthers (!player)) d1;

                                            serverMessage (player, "Welcome, " ^ username ^ "!")
                                        end
                                  | _ => ()
                            end
                        else
                            let
                                val response = JSON.empty
                                            |> JSON.add ("status", JSON.String "wrongpwd")
                            in
                                sendClientResponse r c response
                            end
                    end
              | "register" =>
                    let
                        val username = JSON.toString (JSON.get d "username")
                        val password = JSON.toString (JSON.get d "password")
                    in
                        (let 
                            val response = JSON.empty
                                        |> JSON.add ("status", JSON.String "OK")
                        in
                            db_regPlayer(username, password);
                            sendClientResponse r c response
                        end)
                        handle usernameExists =>
                            let
                                val response = JSON.empty
                                        |> JSON.add ("status", JSON.String "nametaken")
                            in
                                sendClientResponse r c response
                            end
                    end
              | _ => ();
            ()
        end) handle InvalidMessage => (print "invalid message\n");

    fun handleDisconnect (c) =
        let 
            val _ = print "[server]\tClient disconnected.\n"
            val player = getPlayer c
        in
            case player of 
                SOME (player as ref (Player {id=id, board=board, ...})) => 
                    let in
                        leaveTable (player, true);
                        unspectateTable (player, true);

                        playerIndex := Vector.update (!playerIndex, id, ref Null);
                        players := filterServerPlayers (filterOthers (!player))
                    end
              | _ => ();

            clients := List.filter (fn x => not(WebsocketServer.sameConnection (c, x))) (!clients)
        end

    fun tick () =
        processTimers ()
end;

MLHoldemServer.createBoard ("FooBar 1", 8, (1, 2), (100, 1000));
MLHoldemServer.createBoard ("FooBar 2", 8, (5, 10), (100, 1000));
MLHoldemServer.createBoard ("FooBar High Rollers 1", 8, (50, 100), (100, 1000));
MLHoldemServer.createBoard ("FooBar High Rollers 2", 8, (50, 100), (100, 1000));
MLHoldemServer.createBoard ("FooBar High Rollers 3", 8, (50, 100), (100, 1000));
MLHoldemServer.createBoard ("FooBar High Rollers 4", 8, (50, 100), (100, 1000));
MLHoldemServer.createBoard ("FooBar V.I.P. only", 8, (500, 1000), (100, 1000));
MLHoldemServer.createBoard ("FooBar V.I.P. only", 8, (500, 1000), (100, 1000));

val s = WebsocketServer.create (9001, MLHoldemServer.handleConnect, MLHoldemServer.handleDisconnect, MLHoldemServer.handleMessage, MLHoldemServer.tick);
PolyML.exception_trace(fn () => WebsocketServer.run s);