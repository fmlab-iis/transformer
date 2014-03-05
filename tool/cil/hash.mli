(*pp $PP *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t

IFDEF OCAML4
THEN
val create : ?random:bool -> int -> ('a, 'b) t
ELSE
val create : int -> ('a, 'b) t
END

val clear : ('a, 'b) t -> ('a, 'b) t

IFDEF OCAML4
THEN
val reset : ('a, 'b) t -> ('a, 'b) t
END

val copy : ('a, 'b) t -> ('a, 'b) t

val add : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

val find : ('a, 'b) t -> 'a -> 'b

val find_all : ('a, 'b) t -> 'a -> 'b list

val mem : ('a, 'b) t -> 'a -> bool

val remove : ('a, 'b) t -> 'a -> ('a, 'b) t

val replace : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

val length : ('a, 'b) t -> int

IFDEF OCAML4
THEN
val randomize : unit -> unit

type statistics = Hashtbl.statistics

val stats : ('a, 'b) t -> statistics
END

val hash : 'a -> int

IFDEF OCAML4
THEN
val seeded_hash : int -> 'a -> int
END

val hash_param : int -> int -> 'a -> int

IFDEF OCAML4
THEN
val seeded_hash_param : int -> int -> int -> 'a -> int
END

(** Returns the keys in a hashtable. *)
val keys : ('a, 'b) t -> 'a list

(** Returns the values in a hashtable. *)
val values : ('a, 'b) t -> 'b list

(** Adds all entries in the second hashtable to the first hashtable. *)
val add_all : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

(** Inserts an element v to h(x) by insert h x v. *)
val insert : ('a, 'b list) t -> 'a -> 'b -> ('a, 'b list) t

(** Adds y to h(x) if there is some z in h(x) with y in h(z) where h is a hash table. *)
val saturate : ('a, 'a list) t -> ('a, 'a list) t
