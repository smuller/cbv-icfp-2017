structure Score =
struct

structure NodeKey =
struct
type ord_key = int
val compare = Int.compare
end

structure M = SplayMapFn(NodeKey)

fun bfs graph start goal =
    let fun nbrs i = Vector.sub (graph, i)
        fun insertall m visited l =
            Vector.foldl (fn (v, m) =>
                             case M.find (visited, v) of
                                 SOME _ => m
                               | NONE => M.insert (m, v, ()))
                         m
                         l
        fun bfs_rec d frontier visited =
            case M.find (frontier, goal) of
                SOME _ => SOME d
              | NONE =>
                if M.isempty frontier then
                    NONE
                else
                    let val frontier' =
                            M.foldli (fn (k, _, m) => insertall m visited (nbrs k))
                                     M.empty
                                     frontier
                        val visited' = M.unionWith (fn (_, _) => ())
                                                   (visited, frontier)
                    in
                        bfs_rec (d + 1) frontier' visited'
                    end
    in
        bfs_rec 0 (M.insert (M.empty, start, ())) M.empty
    end

fun score p map (state: State.t) =
    let val {sites, rivers, mines} = map
        val {graph = g, pgraph = pgraph, ...} = state
        val mg = Vector.sub (pgraph, p)
        fun h _ = 0.0
        fun msscore p m s =
            let val connected =
                    case bfs mg m s of
                        SOME _ => true
                      | NONE => false
            in
                if connected then
                    let val dist =
                            case bfs g m s of
                                SOME d => d
                              | NONE => raise (Fail "mine and site not connected?")
                    in
                        dist * dist
                    end
                else 0
            end
        fun mscore p m =
            List.foldl op+ 0 (List.map (msscore p m) sites)
    in
        List.foldl op+ 0 (List.map (mscore p) mines)
    end

end
