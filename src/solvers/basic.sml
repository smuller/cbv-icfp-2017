structure BasicSolver : SOLVER =
struct

fun solve (map, punters, state, punter) =
    let fun evalforall s =
            List.tabulate (punters, fn i => Score.score i map s)
        fun minimax d p state =
            if d <= 0 then
                (Pass, evalforall state)
            else
                let val moves = State.possible_moves state
                    val states =
                        List.map (fn m => State.apply ((p, m), state)) moves
                    val mevals =
                        List.map (minimax (d - 1) ((p + 1) mod punters)) states
                    val evals = List.map #2 mevals
                    val evaled = ListPair.zip (moves, evals)
                in
                    List.foldl (fn ((m, s), (m', s')) =>
                                   if List.nth (s, p) > List.nth (s', p)
                                   then (m, s) else (m', s'))
                               (Pass, (List.tabulate (punters, fn _ => ~1)))
                               evaled
                end
    in
        #1 (minimax 1 punter state)
    end

end
