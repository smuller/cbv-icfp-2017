structure P : ONLINE_PARAMS =
struct
val host = "punter.inf.ed.ac.uk"
val port = 9005
val name = "CBV-basic"
end

structure O = OnlineFn(structure S = BasicSolver
                       structure P = P)

val _ = O.play ()
        handle InvalidMSG s => raise Fail ("Invalid message: " ^ s)
