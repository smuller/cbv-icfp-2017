signature OFFLINE_MSG =
sig

datatype offline_msg =
         PSHandshake of string (* {"me" : name} *)
       | SPHandshake of string (* {"you" : name} *)
       |  Setup of punter * int * map
         (* {"punter":p, "punters" : n, "map" : map} *)
       | Ready of State.t
       | Moves of (punter * move) list * State.t
       | Move of move * State.t
       | Stop of (punter * move) list * (punter * int) list * State.t
         (* {"stop" : {"moves" : moves, "scores" : scores}} *)

val json_to_msg : string -> offline_msg

val msg_to_json : punter -> offline_msg -> string
end
