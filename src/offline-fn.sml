signature OFFLINE_PARAMS =
sig

    val name : string

end


functor OfflineFn (structure S : SOLVER; structure P : OFFLINE_PARAMS) =
struct

exception GameError of string

structure M = OfflineMSG
structure St = State

fun printMsg p m =
    let val json = M.msg_to_json p m
        val s = (Int.toString (String.size json)) ^ ":" ^ json
    in
        print (s ^ "\n")
    end

fun readMsg () =
    let val SOME s = TextIO.inputLine TextIO.stdIn
        val msg = case String.tokens (fn c => c = #":") s of
                      n::rest => String.concatWith ":" rest
                    | _ => raise (InvalidMSG "no n:")
    in
        M.json_to_msg msg
    end

fun play () =
    let
        (* Handshake *)
        val _ = printMsg 0 (M.PSHandshake P.name);
        val _ = case readMsg () of
                    M.SPHandshake s => if s = P.name then ()
                                       else raise (GameError "handshake failed")
                  | _ => raise (GameError "handshake failed")
    in
        case readMsg () of
            M.Setup (pid, npunters, map) =>
            let val st = St.init (pid, npunters, map)
            in
                printMsg pid (M.Ready st);
                NONE
            end
          | M.Moves (moves, state) =>
            let val state' = List.foldl St.apply
                                        state
                                        moves
                val {pid, map, punters, ...} = state'
                val m = S.solve (map, punters, state', pid)
                val st' = St.apply ((pid, m), state')
            in
                printMsg pid (M.Move (m, st'));
                NONE
            end
          | M.Stop (pml, sl, st) =>
                (case List.partition (fn (p, s) => p = #pid st) sl of
                     ([(_, me)], others) => SOME (me, others)
                   | _ => raise (GameError "invalid scores"))
    end
end
