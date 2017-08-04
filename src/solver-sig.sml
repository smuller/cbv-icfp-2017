signature SOLVER =
sig

    (* map, number of punters, state, punter *)
    val solve : map * int * State.t * punter -> move

end
