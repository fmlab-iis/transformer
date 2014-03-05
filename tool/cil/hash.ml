(*pp $PP *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t

let create = Hashtbl.create

let clear m = Hashtbl.clear m; m

IFDEF OCAML4
THEN
let reset m = Hashtbl.reset m; m
END

let copy = Hashtbl.copy

let add m x y = Hashtbl.add m x y; m

let find = Hashtbl.find

let find_all = Hashtbl.find_all

let mem = Hashtbl.mem

let remove m x = Hashtbl.remove m x; m

let replace m x y = Hashtbl.replace m x y; m

let iter = Hashtbl.iter

let fold = Hashtbl.fold

let length = Hashtbl.length

IFDEF OCAML4
THEN
let randomize = Hashtbl.randomize

type statistics = Hashtbl.statistics

let stats = Hashtbl.stats
END

let hash = Hashtbl.hash

IFDEF OCAML4
THEN
let seeded_hash = Hashtbl.seeded_hash
END

let hash_param = Hashtbl.hash_param

IFDEF OCAML4
THEN
let seeded_hash_param = Hashtbl.seeded_hash_param
END

let keys m = fold (fun k v res -> k::res) m []

let values m = fold (fun k v res -> v::res) m []

let add_all m1 m2 =
  List.iter (fun v -> ignore(add m1 v (find m2 v))) (keys m2);
  m1

(** Inserts an element v to h(x) by insert h x v if v is not in h(x). *)
let insert h x v =
  try
    let l = find h x in
    if List.mem v l then
      h
    else
      replace h x (v::l)
  with Not_found ->
    add h x [v]

(** Adds y to h(x) if there is some z in h(x) with y in h(z) where h is a hash table. *)
let saturate h =
  let changed = ref true in
  let union l1 l2 =
    List.fold_left (
      fun l x -> 
        if List.mem x l then 
          l 
        else 
          let _ = changed := true in
          x::l
    ) l1 l2 in
  let helper h x =
    ignore(replace h x (
      List.fold_left (
        fun res y -> 
          try 
            union res (find h y) 
          with Not_found -> 
            res
      ) (find h x) (find h x)
    )) in
  let _ = 
    while !changed do
      let _ = changed := false in
      List.iter (helper h) (keys h)
    done in
  h
