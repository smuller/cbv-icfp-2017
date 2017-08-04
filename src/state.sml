structure State =
struct

structure V = Vector

type graph = int Vector.vector Vector.vector

fun map_to_graph maxsite sites rivers =
    let val _ = print "converting\n"

        fun f ((s, t), v) =
            let val sl = Vector.sub (v, s)
                val tl = Vector.sub (v, t)
                val sl' = t::sl
                val tl' = s::tl
            in
                Vector.update (Vector.update (v, s, sl'), t, tl')
            end
        val v = Vector.tabulate (maxsite, fn _ => [])
        val v' = List.foldl f v rivers
    in
        Vector.map Vector.fromList v'
        before (print "converted\n")
    end

type t = {pid: punter,
          punters: int,
          map : map,
          claims: (punter * river) list,
          graph : graph,
          pgraph : graph Vector.vector}

fun init (pid, punters, (map: map)) =
    let val sites = #sites map
        val rivers = #rivers map
        val maxsite = List.foldl Int.max 1000000 sites
    in
        {pid = pid,
         punters = punters,
         map = map,
         claims = [],
         graph = map_to_graph maxsite sites rivers,
         pgraph = V.tabulate
                      (punters,
                       fn _ => V.tabulate (maxsite, fn _ => V.fromList []))
        }
    end

fun myrivers p claims =
    List.foldl (fn ((p', r), l) => if p = p' then r::l else l) [] claims

fun initc (pid, punters, (map: map), claims) =
    let val sites = #sites map
        val rivers = #rivers map
        val maxsite = List.foldl Int.max 1000000 sites
    in
        {pid = pid,
         punters = punters,
         map = map,
         claims = claims,
         graph = map_to_graph maxsite sites rivers,
         pgraph = V.tabulate (punters,
                              fn p => map_to_graph maxsite sites
                                                   (myrivers p claims)
                             )
        }
    end

fun apply ((punter, move), {pid, punters, map, claims, graph, pgraph}) : t =
    case move of
        Pass => {pid = pid, punters = punters, map = map, claims = claims,
                 graph = graph, pgraph = pgraph
                }
      | Claim (s, t) =>
        let val pg = V.sub (pgraph, punter)
            fun append v i =
                let val n = V.length v
                in
                    V.tabulate (n + 1,
                                fn j => if j < n then V.sub (v, j) else i)
                end
            val sl = V.sub (pg, s)
            val tl = V.sub (pg, t)
            val sl' = append sl t
            val tl' = append tl s
            val pg' = V.update (V.update (pg, s, sl'), t, tl')
        in
            {pid = pid, punters = punters, map = map,
             claims = (punter, (s, t))::claims,
             graph = graph,
             pgraph = V.update (pgraph, punter, pg')}
        end


fun possible_moves ({pid, punters, map, claims, ...}: t) =
    List.map Claim
             (List.filter (fn (s, t) =>
                              not (List.exists
                                       (fn (_, (s', t')) => s = s' andalso t = t')
                                       claims))
                          (#rivers map))

end
