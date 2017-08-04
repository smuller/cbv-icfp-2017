signature ONLINE_MSG =
sig

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

val json_to_msg : string -> online_msg
val msg_to_json : punter -> online_msg -> string

end
