structure P : OFFLINE_PARAMS =
struct
val name = "CBV-basic"
end

structure O = OfflineFn(structure S = BasicSolver
                        structure P = P)

val _ = O.play ()
        handle InvalidMSG s => raise Fail ("Invalid message: " ^ s)
