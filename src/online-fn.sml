signature ONLINE_PARAMS =
sig

    val name : string
    val host : string
    val port : int

end


functor OnlineFn (structure S : SOLVER; structure P : ONLINE_PARAMS) =
struct

exception GameError of string

structure N = Socket
structure M = OnlineMSG
structure St = State

fun sendString (sock, s) =
    let val v = Vector.map (fn c => Word8.fromInt (Char.ord c)) s
        val n = Vector.length v
        fun send i =
            let val sent = N.sendVec (sock, VectorSlice.slice (v, i, NONE))
            in
                if sent = n - i then
                    ()
                else
                    send (i + sent)
            end
    in
        send 0
    end

fun sendMsg p (sock, m) =
    let val json = M.msg_to_json p m
        val s = (Int.toString (String.size json)) ^ ":" ^ json
    in
        print (s ^ "\n");
        sendString (sock, s)
    end

fun recvMsg sock =
    let val v = N.recvVec (sock, 10)
        val s = Vector.map (fn n => Char.chr (Word8.toInt n)) v
        val msg = case String.tokens (fn c => c = #":") s of
                      n::rest =>
                      let val s = String.concatWith ":" rest
                      in
                          (case Int.fromString n of
                               SOME n =>
                               s ^
                               (Vector.map
                                    (fn n => Char.chr (Word8.toInt n))
                                    (N.recvVec (sock, n - (String.size s))))
                             | NONE => raise (InvalidMSG "no n:"))
                      end
                    | _ => raise (InvalidMSG "no n:")
    in
        print (msg ^ "\n");
        M.json_to_msg msg
    end

fun play () =
    let val hostentry =
            case NetHostDB.getByName P.host of
                NONE => raise (GameError "no host name?")
              | SOME addr => NetHostDB.addr addr
        val sock = INetSock.TCP.socket ()
        val _ = N.Ctl.setREUSEADDR (sock, true)
        (* val _ = OS.Process.atExit (fn () => Socket.close sock) *)
        val addr = INetSock.toAddr (hostentry, P.port)
        val _ = N.connect (sock, addr)

        (* Handshake *)
        val _ = sendMsg 0 (sock, M.PSHandshake P.name);
        val _ = case recvMsg sock of
                    M.SPHandshake s => if s = P.name then ()
                                       else raise (GameError "handshake failed")
                  | _ => raise (GameError "handshake failed")

        (* Setup *)
        val (pid, npunters, map) =
            case recvMsg sock of
                M.Setup (p, n, m) => (p, n, m)
              | _ => raise (GameError "setup failed")
        val _ = sendMsg pid (sock, M.Ready)

        val state = St.init (pid, npunters, map)

        fun move (map, state, moves) =
            let val state' = List.foldl St.apply
                                        state
                                        moves
                val _ = print "solving\n"
                val m = S.solve (map, npunters, state', pid)
            in
                sendMsg pid (sock, M.Move m);
                St.apply ((pid, m), state')
            end

        (* Gameplay *)
        fun loop state =
            case recvMsg sock of
                M.Moves l => loop (move (map, state, l))
              | M.Stop (pml, sl) =>
                (case List.partition (fn (p, s) => p = pid) sl of
                     ([(_, me)], others) => (me, others)
                   | _ => raise (GameError "invalid scores"))
    in
        loop state
    end
end
