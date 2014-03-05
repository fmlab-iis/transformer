
open Arg

type action = 
| Cilly
| Extend
| Globals
| Inline
| Insert
| OneReturn
| Over
| SingleVariable
| Summarize of string * string * string
| Test of string * string * string
| Under
| Unwind of string * int
| UnwindFunctions of string * string list * int

let actions = ref []

let ofile = ref None

let fname = ref ""

let utargets = ref []

let pre = ref ""

let post = ref ""

let aa a = actions := !actions@[a]

let args =
  [
    ("-e", Unit (fun () -> aa Extend), "Extend formal parameters to capture the original values.");
    ("-g", Unit (fun () -> aa Globals), "Print all global variables.");
    ("-i", Unit (fun () -> aa Insert), "Insert functions for verifiers.");
    ("-l", Unit (fun () -> aa Inline), "Inline nonrecursive functions.");
    ("-m", Tuple [
      Set_string fname;
      Set_string pre;
      String (fun str -> post := str; aa (Summarize (!fname, !pre, !post)))
    ], "Test if an assertion can be violated in a function under a specified\n     pre-condition.");
    ("-r", Unit (fun () -> aa OneReturn), "Make every function have only one return.");
    ("-s", Unit (fun () -> aa SingleVariable), "Make every actual parameter a single variable.");
    ("-t", Tuple [
      Set_string fname;
      Set_string pre;
      String (fun str -> post := str; aa (Test (!fname, !pre, !post)))
    ], "Test if an assertion can be violated in a function under a specified\n     pre-condition.");
    ("-u", Unit (fun () -> aa Under), "Apply under-approximation.");
    ("-v", Unit (fun () -> aa Over), "Apply over-approximation.");
    ("-w", Tuple [
      Set_string fname;
      Int (fun i -> aa (Unwind (!fname, i)))
    ], "Unwind all functions in a function for a specified number of times.");
    ("-wf", Tuple [
      Set_string fname;
      String (fun str -> utargets := Str.split (Str.regexp "[ ,]") str);
      Int (fun i -> aa (UnwindFunctions (!fname, !utargets, i)))
    ], "Unwind specified functions in a function for a specified number of times.");
    ("-o", String (fun str -> ofile := Some str), "Set the output file.")
  ]

let usage = "tran [-r] [-i] [-e] [-s] [-g | -m NAME EXP EXP | -t NAME EXP EXP | -u | -v | \n     -w HOST COUNT | -wf HOST TARGETS COUNT] [-o FILE] FILE"

let get_file filename =
  let fn = 
    if Sys.file_exists filename then
      filename
    else
      let fn = Filename.temp_file "cilengine" "cfile" in
      let ch = open_out fn in
      let _ = output_string ch filename in
      let _ = close_out ch in
      fn in
  Frontc.parse fn ()

let tran filename =
  let file = get_file filename in
  let pfile = ref true in
  let process action =
    match action with
      Cilly -> ()
    | OneReturn -> Tran.oneret file
    | Insert -> Tran.insert_verifier_functions file
    | SingleVariable -> Tran.single_actual file
    | Under -> Tran.under file
    | Over -> Tran.over file
    | Extend -> Tran.extend file
    | Globals -> pfile := false; Tran.pGlobals file
    | Summarize (fname, pre, post) -> Tran.summarize file fname pre post
    | Test (fname, pre, post)-> Tran.validity file fname pre post
    | Unwind (fname, n) -> Tran.unwind file fname n
    | UnwindFunctions (fname, targets, n) -> Tran.unwind file fname ~targets:targets n
    | Inline -> Tran.inline file
  in
  let _ = List.iter process !actions in
  if !pfile then
    match !ofile with
      None -> Tran.pFile file
    | Some fn -> Tran.dFile file fn
  else
    ()

let _ = 
  let _ = Errormsg.logChannel := stderr in
  parse args tran usage
