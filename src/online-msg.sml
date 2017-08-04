structure OnlineMSG :> ONLINE_MSG =
struct

datatype online_msg =
         PSHandshake of string (* {"me" : name} *)
       | SPHandshake of string (* {"you" : name} *)
       | Setup of punter * int * map
         (* {"punter":p, "punters" : n, "map" : map} *)
       | Ready
       | Moves of (punter * move) list
       | Move of move
       | Stop of (punter * move) list * (punter * int) list
         (* {"stop" : {"moves" : moves, "scores" : scores}} *)

datatype json_data =
         JString of string
         | JInt of int
         | JMap of map
         | JList of json_data list
         | JMove of punter * move
         | JMoves of (punter * move) list
         | JScore of punter * int
         | JMovesScores of (punter * move) list * (punter * int) list
         | JPair of string * json_data
         | JSite of site
         | JRiver of river
         | JClaim of punter * site * site
         | JPass of punter
         | JIgnore
         | JM of online_msg

structure OnlineCallbacks : JSON_CALLBACKS =
struct

type json_data = json_data

fun unsite (JSite n) = n
  | unsite _ = raise (InvalidMSG "unsite")
fun unriver (JRiver r) = r
  | unriver _ = raise (InvalidMSG "unriver")
fun unmove (JMove (p, m)) = (p, m)
  | unmove _ = raise (InvalidMSG "unmove")
fun unscore (JScore (p, s)) = (p, s)
  | unscore _ = raise (InvalidMSG "unscore")
fun unint (JInt i) = i
  | unint _ = raise (InvalidMSG "unint")

fun json_object [JPair ("me", JString name)] = JM (PSHandshake name)
  | json_object [JPair ("you", JString name)] = JM (SPHandshake name)
  | json_object [JPair ("punter", JInt p),
                 JPair ("punters", JInt n),
                 JPair ("map", JMap m)] = JM (Setup (p, n, m))
  | json_object [JPair ("ready", _)] = JM (Ready)
  | json_object ((JPair ("sites", JList sites))::
                 (JPair ("rivers", JList rivers))::
                 (JPair ("mines", JList mines))::_) =
    JMap {sites = List.map unsite sites,
          rivers = List.map unriver rivers,
          mines = List.map unint mines}
  | json_object ((JPair ("id", JInt n))::_) = JSite n
  | json_object [JPair ("source", JInt s),
                 JPair ("target", JInt t)] = JRiver (s, t)
  | json_object [JPair ("move", JMoves pml)] = JM (Moves pml)
  | json_object [JPair ("moves", JList ms)] = JMoves (List.map unmove ms)
  | json_object [JPair ("claim", JClaim (p, s, t))] = JMove (p, Claim (s, t))
  | json_object [JPair ("pass", JPass p)] = JMove (p, Pass)
  | json_object [JPair ("punter", JInt p)] = JPass p
  | json_object [JPair ("punter", JInt p),
                 JPair ("source", JInt s),
                 JPair ("target", JInt t)] = JClaim (p, s, t)
  | json_object [JPair ("moves", JList ms),
                 JPair ("scores", JList ss)] =
    JMovesScores (List.map unmove ms, List.map unscore ss)
  | json_object [JPair ("stop", JMovesScores (ms, ss))] = JM (Stop (ms, ss))
  | json_object _ = raise (InvalidMSG "json_object")

fun json_pair (k, d) = JPair (k, d)

fun json_array ds = JList ds

fun json_value d = d

fun json_string s = JString s

fun json_int n = JInt n

fun json_real _ = JIgnore
fun json_bool _ = raise (InvalidMSG "bool")
fun json_null _ = raise (InvalidMSG "null")
fun error_handle (msg, pos, data) = raise (InvalidMSG ("Error: " ^ msg ^ " near " ^ Int.toString pos ^ ": " ^ data))

end

structure OnlineParser = JSONParser(OnlineCallbacks)

fun json_to_msg s =
    case OnlineParser.parse s of
        JM m => m
      | _ => raise (InvalidMSG "not a message")

fun msg_to_json p m =
    let fun pair_to_json (k, v) =
            "\"" ^ k ^ "\":" ^ v
        fun object_to_json kvs =
            "{" ^ (String.concatWith "," (List.map pair_to_json kvs)) ^ "}"
        fun list_to_json f l =
            "[" ^ (String.concatWith "," (List.map f l)) ^ "]"
        fun river_to_json (s, t) =
            object_to_json [("source", Int.toString s),
                            ("target", Int.toString t)]
        fun map_to_json {sites, rivers, mines} =
            object_to_json [("sites", list_to_json Int.toString sites),
                            ("rivers", list_to_json river_to_json rivers),
                            ("mines", list_to_json Int.toString mines)]
        fun move_to_json p (Claim (s, t)) =
            object_to_json [("claim",
                             object_to_json
                                 [("punter", Int.toString p),
                                  ("source", Int.toString s),
                                  ("target", Int.toString t)])]
          | move_to_json p Pass =
            object_to_json [("pass",
                             object_to_json [("punter", Int.toString p)])]
        fun score_to_json (p, s) =
            object_to_json [("punter", Int.toString p),
                            ("score", Int.toString s)]
    in
        case m of
            PSHandshake name => object_to_json [("me", "\"" ^ name ^ "\"")]
          | SPHandshake name => object_to_json [("you", "\"" ^ name ^ "\"")]
          | Setup (p, n, m) =>
            object_to_json [("punter", Int.toString p),
                            ("punter", Int.toString n),
                            ("map", map_to_json m)]
          | Ready => object_to_json [("ready", Int.toString p)]
          | Moves pml =>
            object_to_json [("move",
                             object_to_json
                                 [("moves",
                                   list_to_json (fn (p, m) => move_to_json p m)
                                                pml)])]
          | Move m => move_to_json p m
          | Stop (ms, ss) =>
            object_to_json [("stop",
                             object_to_json
                                 [("moves",
                                   list_to_json (fn (p, m) => move_to_json p m)
                                                ms),
                                  ("scores",
                                   list_to_json score_to_json ss)])]
    end
end
