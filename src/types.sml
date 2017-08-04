type punter = int
type site = int
type river = site * site
type map = {sites: site list,
            rivers: river list,
            mines: site list}
datatype move =
         Claim of river
       | Pass
type result = int * ((punter * int) list) (* My score, other scores *)
exception InvalidMSG of string
