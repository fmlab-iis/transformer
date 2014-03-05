
val doit : Cil.file -> string list -> unit

val unwind : Cil.file -> string -> (Cil.fundec -> bool) -> int -> unit
