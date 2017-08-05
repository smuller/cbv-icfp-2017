val port =
    case CommandLine.arguments () of
        port::_ => (case Int.fromString port of
                       SOME port => port
                     | NONE => (print "usage: ./basiconline <port>";
                                OS.Process.exit OS.Process.failure))
      | _ => (print "usage: ./basiconline <port>";
              OS.Process.exit OS.Process.failure)


val _ = print ("port " ^ (Int.toString port) ^ "\n")
              
structure P : ONLINE_PARAMS =
struct
val host = "punter.inf.ed.ac.uk"
val port = port
val name = "CBV-basic"
end

structure O = OnlineFn(structure S = BasicSolver
                       structure P = P)

val _ = O.play ()
        handle InvalidMSG s => raise Fail ("Invalid message: " ^ s)
