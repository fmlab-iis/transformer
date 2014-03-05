
open Cil

(** Prints a file to stdout. *)
val pFile : file -> unit

(** Dumps a CIL file to an external file. *)
val dFile : file -> string -> unit

(** Returns the string representation of a CIL expression. *)
val string_of_exp : exp -> string

(** Makes every function have only one return. *)
val oneret : file -> unit

(** Insert functions for verifiers. *)
val insert_verifier_functions : file -> unit

(** Makes every actual parameter a single variable. *)
val single_actual : file -> unit

(** Apply under-approximation. *)
val under : file -> unit

(** Apply over-approximation. *)
val over : file -> unit

(** Extends formal parameters of functions to capture the original values. *)
val extend : file -> unit

(** Unwinds specified functions in a function for a specified number of times. *)
val unwind : file -> string -> ?targets:string list -> int -> unit

(** Replaces a function call by the summary of the function. *)
val summarize : file -> string -> string -> string -> unit

(** 
    * Inserts a pre-condition and a post-condition to a function to see if the
    * post-condition is always satisfied under the pre-condition.
*)
val validity : file -> string -> string -> string -> unit

(** Returns all global variables in a file. *)
val globals : file -> varinfo list

(** Prints all global variables in a file. *)
val pGlobals : file -> unit

(** Inlines nonrecursive functions. *)
val inline : file -> unit
