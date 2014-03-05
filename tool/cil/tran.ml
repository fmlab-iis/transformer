
open Cil
open Pretty

exception NotFound of string
exception Unsupported of string



(** ==================== CIL Utilities ==================== *)

let remove_builtin_functions = ref true

let verifier_assume_name = "__VERIFIER_assume"

let verifier_assert_name = "__VERIFIER_assert"

let assert_name = "assert"

let verifier_nondet_name = "__VERIFIER_nondet_int"

let comment_name = "__comment"

let main_name = "main"

let result_name = "r"

let result_var = makeVarinfo false result_name intType

let sep = ";"

let delim = ","

(** 
    * Returns true if a function name is a real function that needs
    * transformation.
*)
let real_fun name =
  name <> verifier_assume_name && name <> verifier_assert_name && 
    name <> assert_name && name <> verifier_nondet_name &&
    name <> comment_name

(** Returns a variable expression. *)
let mkVarExp varinfo = Lval (Var varinfo, NoOffset)

(** Returns an instruction that assumes a specified expression. *)
let mkAssume exp =
  let typ = TFun (voidType, None, false, []) in
  let vinfo = makeVarinfo true verifier_assume_name typ in
  let lval = Lval (Var vinfo, NoOffset) in
  let instr = Call (None, lval, [exp], locUnknown) in
  instr

(** 
    * Returns an instruction that assumes the conjunction of the
    * specified expressions.
*)
let mkAssumes exps =
  match exps with
    [] -> mkAssume one
  | exp::[] -> mkAssume exp
  | hd::tl -> mkAssume (List.fold_left (fun res exp -> BinOp (LAnd, res, exp, intType)) hd tl)

(** Returns an assert instruction. *)
let mkAssert exp =
  let typ = TFun (voidType, None, false, []) in
  let vinfo = makeVarinfo true verifier_assert_name typ in
  let lval = Lval (Var vinfo, NoOffset) in
  let instr = Call (None, lval, [exp], locUnknown) in
  instr

(**
   * Returns an instruction that assigns a nondeterministic value to a variable.
*)
let mkNondet v =
  let typ = TFun (intType, None, false, []) in
  let vinfo = makeVarinfo true verifier_nondet_name typ in
  let lval = Lval (Var vinfo, NoOffset) in
  let instr = Call (Some (Var v, NoOffset), lval, [], locUnknown) in
  instr

(** 
    * Returns a comment function. With myCilPrinter, this function call is
    * printed as comments.
*)
let mkComment str =
  let typ = TFun (voidType, None, false, []) in
  let vinfo = makeVarinfo true comment_name typ in
  let lval = Lval (Var vinfo, NoOffset) in
  let instr = Call (None, lval, [mkString str], locUnknown) in
  instr

(**
   * Returns a temporary variable and an instruction such that the variable
   * equals a specified expression.
*)
let mkTemp fundec exp =
  let tv = makeTempVar fundec ~insert:true ~name:("tmp") (typeOf exp) in
  let instr = Set ((Var tv, NoOffset), exp, locUnknown) in
  (tv, instr)

(**
   * Returns a temporary variable and an instruction such that the variable
   * equals a specified variable.
*)
let mkTempVar fundec v =
  let tv = makeTempVar fundec ~insert:true ~name:v.vname v.vtype in
  let instr = Set ((Var tv, NoOffset), (mkVarExp v), locUnknown) in
  (tv, instr)

(** Returns the string representation of a CIL expression. *)
let string_of_exp exp = sprint 80 (printExp defaultCilPrinter () exp)

let string_of_call_relation res formals actuals =
  let str = String.concat delim (List.map2 (fun v e -> v.vname ^ "=" ^ string_of_exp e) formals actuals) in
  match res with
    Some (Var v, NoOffset) -> result_name ^ "=" ^ v.vname ^ sep ^ str
  | _ -> sep ^ str

let mkCallComment fv res formals actuals =
  mkComment (fv.vname ^ sep ^ string_of_call_relation res formals actuals)

(** Save the values of returned variable and global variables after calling a function. *)
let post_save fd res gvars =
  snd (List.split (List.map (fun v -> mkTempVar fd v) ((match res with Some (Var v, NoOffset) -> [v] | _ -> [])@gvars)))

(** 
    * This class defines a printer that can print comment function calls as
    * comments.
*)
class myCilPrinterClass =
  (** Returns the string in an expression. *)
  let getConstantString e =
    match e with
      Const (CStr str) -> str
    | _ -> "" in
object(self)
  inherit defaultCilPrinterClass as super

  method pInstr () instr =
    match instr with
      Call (res, Lval (Var fv, NoOffset), args, loc) ->
        if fv.vname = comment_name then
          text ("// " ^ String.concat "," (List.map getConstantString args))
        else
          super#pInstr () instr
    | _ -> super#pInstr () instr

  method pGlobal () global =
    if !remove_builtin_functions then
      match global with
        GVarDecl (v, _) ->
          if not !printCilAsIs && Hash.mem builtinFunctions v.vname then
            nil
          else
            super#pGlobal () global
      | _ -> super#pGlobal () global
    else
      super#pGlobal () global
end

(** An object of class myCilPrinterClass. *)
let myCilPrinter = new myCilPrinterClass

(** Prints a CIL file to stdout with myCilPrinter. *)
let pFile file =
  let docs = List.map (fun global -> printGlobal myCilPrinter () global) file.globals in
  let doc = List.fold_left (fun res d -> res ++ d) nil docs in
  print_endline (sprint 80 doc)

(** Dumps a CIL file to an external file with myCilPrinter. *)
let dFile file filename = dumpFile myCilPrinter (open_out filename) "" file

(** 
    * Returns the free variables in a CIL expression. The free variables
    * are returned as a map from a variable name to its varinfo.
*)
let fvars exp = 
  let map = Hash.create 100 in
  let _ = visitCilExpr 
    (object(self)
      inherit nopCilVisitor
      method vvrbl v =
        ignore(Hash.add map v.vname v);
        DoChildren
     end) exp in
  map

(** 
    * Returns the global variables used in a CIL expression. The used global
    * variables are returned as a map from a variable name to its varinfo.
*)
let gvars_in_exp exp =
  let map = Hash.create 100 in
  let _ = visitCilExpr 
    (object(self)
      inherit nopCilVisitor
      method vvrbl v =
        let _ = 
          if v.vglob then
            ignore(Hash.add map v.vname v)
          else
            () in
        DoChildren
     end) exp in
  map

(** 
    * This class provides a visitor that can collect additional information.
    * Subclasses must call vglob and vfunc of this class.
*)
class myVisitor =
object(self)
  inherit nopCilVisitor

  val gmap = Hash.create 100

  (** The current function to be visited *)
  val mutable cfun = None

  (** Returns the current function in visiting. *)
  method cfun =
    match cfun with 
      Some f -> f
    | None -> raise (NotFound "Unable to determine the current function during visiting the CIL file.")

  method cgvars = gmap

  (** Remembers all the global variables seen so far. *)
  method vglob glob =
    let _ = 
      match glob with
        GVar (v, _, _) -> ignore(Hash.add gmap v.vname v)
      | _ -> () in
    DoChildren

  method vfunc fundec : fundec visitAction =
    let _ = cfun <- Some fundec in
    DoChildren
end

let union l1 l2 = List.fold_left (fun l x -> if List.mem x l then l else x::l) l1 l2

(**
   * A class containing supplementary functions for CIL.
   * file - a CIL file
*)
class cilHelper file =
  (** A map from a global variable name to its varinfo. *)
  let gmap : (string, varinfo) Hash.t = Hash.create 100 in

  (** A map from a function name to its definition. *)
  let fmap : (string, fundec) Hash.t = Hash.create 100 in

  (** A map from a function name to the global variables used in the function. *)
  let fgmap : (string, varinfo list) Hash.t = Hash.create 100 in

  (** A map from a function to a list of functions called in it. *)
  let cmap : (string, string list) Hash.t = Hash.create 100 in

  let visitor = object(self)
    inherit myVisitor as super

    (* Add global variables (not global functions) used in a function. *)
    method vvrbl v =
      let _ = 
        if v.vglob && Hash.mem gmap v.vname then
          try
            ignore(Hash.insert fgmap (super#cfun).svar.vname v)
          with (NotFound _) ->
            () in
      SkipChildren

    (* Construct an one-level call graph. *)
    method vinst instr =
      match instr with
        Call (res, Lval (Var fv, NoOffset), args, loc) -> 
          ignore(Hash.insert cmap (super#cfun).svar.vname fv.vname);
          DoChildren
      | _ -> DoChildren

    method vfunc fd =
      let _ = super#vfunc fd in
      let _ = Hash.add fmap fd.svar.vname fd in
      let _ = Hash.add fgmap fd.svar.vname [] in
      let _ = Hash.add cmap fd.svar.vname [] in
      DoChildren

    (** Remember the varinfo of global variables. *)
    method vglob global =
      let _ = super#vglob global in
      let _ = 
        match global with 
          GVar (v, _, _) -> ignore(Hash.add gmap v.vname v)
        | _ -> () in
      DoChildren
  end in
  let _ = visitCilFileSameGlobals (visitor :> cilVisitor) file in
  let _ = Hash.saturate cmap in
  let _ = Hash.iter (
    fun fn callees ->
      ignore(Hash.replace fgmap fn (List.fold_left (
        fun res callee -> 
          union res (try Hash.find fgmap callee with Not_found -> [] (* a function called may not be a user function *))
      ) (Hash.find fgmap fn) callees))
  ) cmap in
object(self)
  (** Returne the CIL file. *)
  method getFile = file

  (** Returns the function definition with a specified function name. *)
  method getFunction fn = Hash.find fmap fn

  (** Returns the formal parameters of a function with a specified name. *)
  method getFormals fn = 
    let fd = Hash.find fmap fn in
    fd.sformals

  (** Returns used global variables in a function. *)
  method getUsedGVars fn =
    try
      Hash.find fgmap fn
    with Not_found ->
      []

  (** Returns the global variable with a specified name. *)
  method getGVar vn = Hash.find gmap vn

  (** Returns all global variables. *)
  method getGVars = Hash.values gmap
end



(** ==================== Program Summary ==================== *)

(** A function summary contains a pre-condition and a post-condition. *)
type summary = {
  pre: exp;
  post: exp
}



(** ==================== One Return ==================== *)

(** Makes every function have only one return. *)
let oneret file =
  List.iter (fun global ->
    match global with
    | GFun (f, _) -> OneReturn.oneret f
    | _ -> ()
  ) file.globals



(** ==================== Verifier Functions ==================== *)

(** Insert functions for verifiers. *)
let insert_verifier_functions file =
  let fundec = emptyFunction verifier_assert_name in
  let cond = makeFormalVar fundec "cond" intType in
  let goto = {
    labels = [Label ("ERROR", locUnknown, false)];
    skind = Block {battrs = []; bstmts = []};
    sid = 1;
    succs = [];
    preds = []
  } in
  let _ = goto.skind <- Goto (ref goto, locUnknown) in
  let return = {
    labels = [];
    skind = Return (None, locUnknown);
    sid = 2;
    succs = [];
    preds = []
  } in
  let ifs = {
    labels = [];
    skind = If (
      UnOp (LNot, mkVarExp cond, intType),
      {battrs = []; bstmts = [goto]},
      {battrs = []; bstmts = []},
      locUnknown);
    sid = 0;
    succs = [];
    preds = []
  } in
  let _ = fundec.sbody.bstmts <- [ifs; return] in
  file.globals <- (GFun (fundec, locUnknown))::file.globals



(** ==================== Single Variable for Actuals ==================== *)

(** Makes every actual parameter a single variable. *)
let single_actual file =
  let single e = 
    match e with
      Const _
    | Lval _ -> true
    | _ -> false in
  let visitor = object(self)
    inherit myVisitor as super

    method vinst inst =
      match inst with
        Call (res, Lval (Var fv, NoOffset), args, loc) ->
          let cfun = super#cfun in
          if real_fun fv.vname then
            let (instrss, args) = List.split (List.map (
              fun arg -> 
                if single arg then
                  ([], arg)
                else
                  let (tv, instr) = mkTemp cfun arg in
                  ([instr], mkVarExp tv) 
            ) args) in
            ChangeTo ((List.flatten instrss)@[Call (res, Lval (Var fv, NoOffset), args, loc)])
          else
            DoChildren
      | _ -> DoChildren
  end in
  visitCilFileSameGlobals (visitor :> cilVisitor) file



(** ==================== Under-Approximation ==================== *)

(** A visitor that performs under-approximation *)
class underVisitor helper =
object(self)
  inherit myVisitor as super

  method vinst instr : (instr list) visitAction =
    match instr with
      Call (res, Lval (Var fv, NoOffset), args, loc) -> 
        (* Assign nondeterministic values to global variables and the variable capturing the returned value. *)
        let nondets = List.map (fun v -> mkNondet v) ((match res with Some (Var v, NoOffset) -> [v] | _ -> [])@(helper#getUsedGVars fv.vname)) in
        let posts = post_save self#cfun res (helper#getUsedGVars fv.vname) in
        if real_fun fv.vname then
          ChangeTo (nondets@[mkCallComment fv res (helper#getFormals fv.vname) args; mkAssume zero]@posts)
        else
          DoChildren
    | _ -> DoChildren
end

(** Applies under-approximation. *)
let under file = visitCilFileSameGlobals (new underVisitor (new cilHelper file) :> cilVisitor) file



(** ==================== Over-Approximation ==================== *)

(** A visitor that performs over-approximation *)
class overVisitor helper =
object(self)
  inherit myVisitor as super

  method vinst instr : (instr list) visitAction =
    match instr with
      Call (res, Lval (Var fv, NoOffset), args, loc) ->
        (* Assign nondeterministic values to global variables and the variable capturing the returned value. *)
        let nondets = List.map (fun v -> mkNondet v) ((match res with Some (Var v, NoOffset) -> [v] | _ -> [])@(helper#getUsedGVars fv.vname)) in
        let posts = post_save self#cfun res (helper#getUsedGVars fv.vname) in
        if real_fun fv.vname then
          ChangeTo (nondets@[mkCallComment fv res (helper#getFormals fv.vname) args; mkAssume one]@posts)
        else
          DoChildren
    | _ -> DoChildren
end

(** Applies over-approximation. *)
let over file = visitCilFileSameGlobals (new overVisitor (new cilHelper file) :> cilVisitor) file



(** ==================== Extension of Function Parameters ==================== *)

(** 
    * A visitor that extends formal parameters of functions to capture the
    * initial values.
*)
class extendVisitor =
object(self)
  inherit nopCilVisitor

  (** A list of global variables seen so far. *)
  val mutable gvars = []

  val mutable tmap = Hash.create 100 

  (** The current function to be visited *)
  val mutable cfun = None

  method private mkTempVars fundec gvars =
    let pairs = List.map (
      fun gv -> 
        (gv, makeTempVar fundec ~insert:true ~name:("initial_value_of_" ^ gv.vname ^ "_") gv.vtype)
    ) gvars in
    ignore(Hash.add tmap fundec.svar.vname pairs)

  method private getGlobalTempPairs name =
    try 
      Hash.find tmap name 
    with Not_found -> 
      raise (NotFound ("Function definition of " ^ name ^ " is not found."))

  (** Remembers all the global variables seen so far. *)
  method vglob glob =
    let _ = 
      match glob with
        GVar (v, _, _) -> gvars <- v::gvars
      | _ -> () in
    DoChildren

  (** Passes additional arguments for each function call. *)
  method vinst instr : (instr list) visitAction =
    match instr with
      Call (res, (Lval (Var fv, NoOffset) as name), args, loc) -> 
        if real_fun fv.vname then
          let cfun = match cfun with Some f -> f | None -> raise (NotFound ("The definition of function " ^ fv.vname ^ " is not found.")) in
          (* Assign the value of each global variable to its temporal version. *)
          let (tvars, assigns) = List.split (List.map (
            fun (gv, tv) -> (tv, Set ((Var tv, NoOffset), mkVarExp gv, locUnknown))
          ) (self#getGlobalTempPairs cfun.svar.vname)) in
          ChangeTo (assigns@[Call (res, name, args@args@(List.map mkVarExp tvars), loc)])
        else
          DoChildren
    | _ -> DoChildren

  (** 
      * Extends the formal parameters and inserts assumptions about the
      * additional parameters capturing the initial values.
  *)
  method vfunc fundec : fundec visitAction =
    (* Update the current function. *)
    let _ = cfun <- Some fundec in
    (* Create a temporal variable for each global variable to capture the
       initial of the global variable before a function call invoked in this
       function. *)
    let _ = self#mkTempVars fundec gvars in
    let _ = 
      (* Skip the extension for the main function and the verifier functions. *)
      if fundec.svar.vname <> main_name && fundec.svar.vname <> verifier_assert_name && fundec.svar.vname <> verifier_assume_name then
        (* Extends the function parameters and inserts the assumptions. *)
        let assumptions = List.map (fun formal -> 
          let original = makeFormalVar fundec ("initial_value_of_" ^ formal.vname) formal.vtype in
          BinOp (Eq, mkVarExp formal, mkVarExp original, intType)
        ) (fundec.sformals@gvars) in
        match assumptions with
          [] -> ()
        | _ -> fundec.sbody.bstmts <- (mkStmt (Instr [mkComment "assumptions about the initial variables"; mkAssumes assumptions]))::fundec.sbody.bstmts in
    DoChildren
end

(** Extends formal parameters of functions to capture the original values. *)
let extend file = visitCilFileSameGlobals (new extendVisitor) file



(** ==================== Function Unwinding ==================== *)

(** Unwinds specified functions in a function for a specified number of times. *)
let rec unwind file host ?targets:(targets=[]) num = 
  let selector =
    match targets with
      [] -> fun fd -> true
    | _ -> fun fd -> List.mem fd.svar.vname targets in
  Inliner.unwind file host selector num



(** ==================== Expression Parsing ==================== *)

let exp_of_lexbuf vmap lexbuf = (ExprParser.expr ExprLexer.token lexbuf) vmap

(** 
    * Parses a file as a CIL expression.
    * vmap - a map from a variable name to its varinfo
    * file - a file name
*)
let exp_of_file vmap file = exp_of_lexbuf vmap (Lexing.from_channel (open_in file))

(** 
    * Parses a string as a CIL expression. 
    * vmap - a map from a variable name to its varinfo
    * str - a string representing a CIL expression
*)
let exp_of_string vmap str = exp_of_lexbuf vmap (Lexing.from_string str)

(**
   * A visitor that substitutes expressions for variables.
   * map : a map from a variable name to the replacement
*)
class varSubVisitor (map : (string, exp) Hash.t) =
object (self)
  inherit nopCilVisitor

  method vexpr exp =
    match exp with
    | Lval (Var v, NoOffset)
    | AddrOf (Var v, NoOffset)
    | StartOf (Var v, NoOffset) ->
      begin
      try
        ChangeTo (Hash.find map v.vname)
      with Not_found ->
        DoChildren
      end
    | _ -> DoChildren
end

(** Substitutes an expression for a variable. *)
let sub_exp exp v e = visitCilExpr (new varSubVisitor (Hash.add (Hash.create 1) v e)) exp

(** 
    * Returns a string as an expression with a context.
    * The returned variable in the function will be substituted for the result
    * name "r" if repres is true.
*)
let exp_of_string_in_fun file fname str repres =
  let vmap = Hash.create 100 in
  let res = ref None in
  let _ = 
    visitCilFileSameGlobals (object(self)
      inherit myVisitor as super

      method vstmt stmt =
        if super#cfun.svar.vname = fname then
          match stmt.skind with
            Return (e, _) -> res := e; super#vstmt stmt
          | _ -> super#vstmt stmt
        else
          super#vstmt stmt
            
      method vfunc fundec =
        let _ = super#vfunc fundec in
        if fundec.svar.vname = fname then
          let _ = List.iter (fun v -> ignore(Hash.add vmap v.vname v)) ((Hash.values super#cgvars)@fundec.sformals) in
          DoChildren
        else
          DoChildren
    end :> cilVisitor) file in
  let _ = Hash.add vmap result_name result_var in
  let exp = exp_of_string vmap str in
  match !res with
    None -> exp
  | Some e -> if repres then sub_exp exp result_name e else exp



(** ==================== Function Abstraction ==================== *)

(** 
    * A visitor that replaces a function call to assert and assume
    * expressions.
    * helper - a cilHelper
    * smap - a map from a function name to its summary
*)
class replaceVisitor helper smap =
  (** 
      * Instantiates a function summary defined on the formal parameters as a
      * summary defined on the actual parameters.
  *)
  let instantiate res formals actuals summary =
    let m = Hash.create 100 in
    let _ = List.iter2 (fun v e -> ignore(Hash.add m v.vname e)) formals actuals in
    let _ = 
      match res with
        Some (Var v, NoOffset) -> ignore(Hash.add m result_name (mkVarExp v))
      | _ -> () in
    { pre = visitCilExpr (new varSubVisitor m) summary.pre;
      post = visitCilExpr (new varSubVisitor m) summary.post
    } in
object(self)
  inherit myVisitor as super

  method vinst instr : (instr list) visitAction =
    match instr with
      Call (res, Lval (Var fv, NoOffset), args, loc) ->
        begin
        try
          let formals = helper#getFormals fv.vname in
          let summary = instantiate res formals args (Hash.find smap fv.vname) in
          (* Add the assertion if the assertion is not 1. *)
          let pres =
            if summary.pre = one then
              []
            else
              [mkComment "Summary: pre"; mkAssert summary.pre] in
          (* Assign nondeterministic values to global variables and the variable capturing the returned value. *)
          let nondets = List.map (fun v -> mkNondet v) ((match res with Some (Var v, NoOffset) -> [v] | _ -> [])@(helper#getUsedGVars fv.vname)) in
          let posts = post_save self#cfun res (helper#getUsedGVars fv.vname) in
          ChangeTo (pres@nondets@[mkComment "Summary: post"; mkCallComment fv res formals args; mkAssume summary.post]@posts)
        with Not_found ->
          DoChildren
        end
    | _ -> DoChildren

end

(** Replaces a function call by the summary of the function. *)
let summarize file fname pre post = 
  let smap = Hash.add (Hash.create 100) fname {
    pre = exp_of_string_in_fun file fname pre false; 
    post = exp_of_string_in_fun file fname post false
  } in
  visitCilFileSameGlobals (new replaceVisitor (new cilHelper file) smap :> cilVisitor) file



(** ==================== Verifying Error Traces ==================== *)

(** 
    * Inserts a pre-condition and a post-condition to a function to see if the
    * post-condition is always satisfied under the pre-condition.
*)
let validity file fname pre post =
  let pre = exp_of_string_in_fun file fname pre true in
  let post = exp_of_string_in_fun file fname post true in
  let visitor = object(self)
    inherit myVisitor as super

    method vstmt stmt =
      if super#cfun.svar.vname = fname then
        match stmt.skind with
          Return (e, _) ->
            ChangeTo (mkStmt (Block {battrs = []; bstmts = [mkStmt (Instr [mkComment "CE: post"; mkAssert post]); stmt]}))
        | _ -> super#vstmt stmt
      else
        super#vstmt stmt

    method vfunc fundec =
      let _ = super#vfunc fundec in
      if fundec.svar.vname = fname then
        let _ = fundec.sbody.bstmts <- (mkStmt (Instr [mkComment "CE: pre"; mkAssume pre]))::fundec.sbody.bstmts in
        DoChildren
      else
        DoChildren
  end in
  visitCilFileSameGlobals (visitor :> cilVisitor) file



(** ==================== Print Global Variables ==================== *)

(** Returns all global variables in a file. *)
let globals file =
  let vmap = Hash.create 100 in
  let visitor = object(self)
    inherit nopCilVisitor as super

    method vglob g =
      match g with
        GVar (v, _, _) -> ignore(Hash.add vmap v.vname v); DoChildren
      | _ -> super#vglob g
  end in
  let _ = visitCilFileSameGlobals visitor file in
  Hash.values vmap

(** Prints all global variables in a file. *)
let pGlobals file =
  List.iter (fun v -> print_endline v.vname) (globals file)



(** ==================== Inline Nonrecursive Functions ==================== *)

(** Inlines nonrecursive functions. *)
let inline file =
  (* A map from a caller to it callees. *)
  let fmap = Hash.create 10 in
  (* A list of user functions. *)
  let fs = ref [] in
  let _ = visitCilFileSameGlobals (object
    inherit myVisitor as super
    method vinst instr =
      match instr with
        Call (res, Lval (Var fv, NoOffset), args, loc) ->
          let caller = (super#cfun).svar.vname in
          let _ =
            try
              let callees = Hash.find fmap caller in
              if not (List.mem fv.vname callees) then
                ignore(Hash.replace fmap caller (fv.vname::callees))
            with Not_found ->
              ignore(Hash.add fmap caller [fv.vname]) in
          DoChildren
      | _ -> DoChildren

    method vglob g =
      let _ = super#vglob g in
      let _ = 
        match g with
          GFun (fd, _) -> 
            if real_fun fd.svar.vname then
              fs := fd.svar.vname::!fs
        | _ -> () in
      DoChildren
  end :> cilVisitor) file in
  let _ = Hash.saturate fmap in
  let isRecursive fn = try List.mem fn (Hash.find fmap fn) with Not_found -> false in
  let toinline = List.fold_left (fun res fn -> if isRecursive fn then res else fn::res) [] !fs in
  Inliner.doit file toinline
