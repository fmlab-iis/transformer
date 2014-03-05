(*
 *
 * Copyright (c) 2007, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)


(** This module provides inlining functions. You can run it from the cilly 
 * command line by passing the names of the functions to inline:
 * 
 *    cilly --save-temps --inline=toinline module.c
 * 
 * This module has not been tested extensively, so you should run it with 
 * the --check argument to ensure that it does not break any CIL invariants 
 *
 * 
 * You can also call directly the [doFile] and [doFunction] functions.
 
 *)

(**
   * Modified by Ming-Hsien Tsai to support bounded unwinding.
   * 11/12/2013
*)

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl
module IH = Inthash
module A = Alpha

(** Modified by Ming-Hsien Tsai --> *)
let insert_nonterm_assertions = ref false

let verifier_assert_name = "__VERIFIER_assert"

let comment_name = "__comment"

let result_name = "r"

let sep = ";"

let delim = ","

let mkAssert exp =
  let typ = TFun (voidType, None, false, []) in
  let vinfo = makeVarinfo true verifier_assert_name typ in
  let lval = Lval (Var vinfo, NoOffset) in
  let instr = Call (None, lval, [exp], locUnknown) in
  instr

let mkComment str =
  let typ = TFun (voidType, None, false, []) in
  let vinfo = makeVarinfo true comment_name typ in
  let lval = Lval (Var vinfo, NoOffset) in
  let instr = Call (None, lval, [mkString str], locUnknown) in
  instr

let string_of_exp exp = sprint 80 (printExp defaultCilPrinter () exp)

let string_of_call_relation res formals actuals =
  let str = String.concat delim (List.map2 (fun v e -> v.vname ^ "=" ^ string_of_exp e) formals actuals) in
  match res with
    Some (Var v, NoOffset) -> result_name ^ "=" ^ v.vname ^ sep ^ str
  | _ -> sep ^ str

let mkCallComment fv res formals actuals = fv.vname ^ sep ^ string_of_call_relation res formals actuals

let mkNeqExp xs ys =
  match List.map2 (fun x y -> BinOp (Ne, x, y, intType)) xs ys with
    [] -> one
  | hd::tl -> List.fold_left (fun res e -> BinOp(LOr, res, e, intType)) hd tl

let rec mkNeqs xs ls =
  match List.map (fun ys -> mkNeqExp xs ys) ls with
    [] -> []
  | hd::tl -> [mkAssert (List.fold_left (fun res e -> BinOp(LAnd, res, e, intType)) hd tl)]

let addArgs h fn args =
  try
    let ls = H.find h fn in
    if not (List.mem args ls) then
      H.replace h fn (args::ls)
  with Not_found ->
    H.add h fn [args]

let mkTemps fd es =
  List.split (List.map (fun e ->
    let tv = makeTempVar fd ~insert:true ~name:("tmp") (typeOf e) in
    let instr = Set ((Var tv, NoOffset), e, locUnknown) in
    (tv, instr)) es)
(** <-- Modified by Ming-Hsien Tsai *)


let doInline = ref false
(** Modified by Ming-Hsien Tsai --> *)
(* let debug = true *)
let debug = false
(** <-- Modified by Ming-Hsien Tsai *)

exception Recursion (* Used to signal recursion *)

(* A visitor that makes a deep copy of a function body for use inside a host 
 * function, replacing duplicate labels, returns, etc. *)
class copyBodyVisitor     (host: fundec)                (* The host of the 
                                                         * inlining *) 
                          (inlining: varinfo)           (* Function being 
                                                         * inlined *)
                          (replVar: varinfo -> varinfo) (* Maps locals of the 
                                                         * inlined function 
                                                         * to locals of the 
                                                         * host *)
                          (retlval: varinfo option)     (* The destination 
                                                         * for the "return" *)
                          (replLabel: string -> string)  
                                                        (* A renamer for 
                                                         * labels *)
                          (retlab: stmt)                (* The label for the 
                                                         * return *)
                          = object (self)
  inherit nopCilVisitor

      (* Keep here a maping from statements to their copies, indexed by their 
       * original ID *)
  val stmtmap : stmt IH.t = IH.create 113

      (* Keep here a list of statements to be patched *)
  val patches : stmt list ref = ref []

  val argid = ref 0

      (* This is the entry point *)
  method vfunc (f: fundec) : fundec visitAction = 
    let patchfunction (f' : fundec) = 
      let findStmt (i: int) = 
        try IH.find stmtmap i 
        with Not_found -> E.s (bug "Cannot find the copy of stmt#%d" i)
      in
      (** Modified by Ming-Hsien Tsai --> *)
      (*E.log "Patching gotos\n";*)
      (** <-- Modified by Ming-Hsien Tsai *)
      let patchstmt (s: stmt) = 
        match s.skind with
          Goto (sr, l) -> 
            if debug then
              E.log "patching goto\n";
            (* Make a copy of the reference *)
            let sr' = ref (findStmt !sr.sid) in
            s.skind <- Goto (sr',l)
        | Switch (e, body, cases, l) -> 
            s.skind <- Switch (e, body, 
                               Util.list_map (fun cs -> findStmt cs.sid) cases, l)
        | _ -> ()
      in
      List.iter patchstmt !patches;
      f'
    in
    patches := [];
    IH.clear stmtmap;
    ChangeDoChildrenPost (f, patchfunction)
    
      (* We must replace references to local variables *)
  method vvrbl (v: varinfo) = 
    if v.vglob then 
      SkipChildren 
    else 
      let v' = replVar v in 
      if v == v' then 
        SkipChildren
      else
        ChangeTo v'


  method vinst (i: instr) = 
    match i with 
      Call (_, Lval (Var vi, _), _, _) when vi.vid == inlining.vid -> 
        (** Modified by Ming-Hsien Tsai --> *)
        (* raise Recursion*)
        DoChildren
        (** <-- Modified by Ming-Hsien Tsai *)

    | _ -> DoChildren

        (* Replace statements. *)
  method vstmt (s: stmt) : stmt visitAction = 
    (* There is a possibility that we did not have the statements IDed 
     * propertly. So, we change the ID even on the replaced copy so that we 
     * can index on them ! *)
    (match host.smaxstmtid with 
      Some id ->
        s.sid <- 1 + id
    | None -> 
        s.sid <- 1);
    (* Copy and change ID *)
    let s' = {s with sid = s.sid} in
    host.smaxstmtid <- Some s'.sid;

    IH.add stmtmap s.sid s'; (* Remember where we copied this statement *)
    (* if we have a Goto or a Switch remember them to fixup at end *)
    (match s'.skind with
      (Goto _ | Switch _) -> 
      (** Modified by Ming-Hsien Tsai --> *)
        (* E.log "Found goto\n"; *)
      (** <-- Modified by Ming-Hsien Tsai *)
        patches := s' :: !patches
    | _ -> ());
    
    (* Change the returns *)
    let postProc (s': stmt) : stmt = 
      (* Rename the labels if necessary *)
      s'.labels <- 
        Util.list_map (fun lb -> 
          match lb with
            Label (n, l, fromsrc) -> Label(replLabel n, l, fromsrc)
          | _ -> lb) s'.labels;

      (* Now deal with the returns *)
      (match s'.skind with 
      | Return (ro, l) -> begin
          (* Change this into an assignment followed by a Goto *)
          match ro, retlval with 
            _, None -> (* Function called with no return lval *)
              s'.skind <- Goto (ref retlab, l)
                
          | None, _ -> 
              ignore (warn "Found return without value in inlined function");
              s'.skind <- Goto (ref retlab, l)
                
          | Some rv, Some retvar-> 
              s'.skind <-
                Block (mkBlock [ mkStmt (Instr [(Set (var retvar, rv, l))]);
                                 mkStmt (Goto (ref retlab, l)) ])
      end
      | _ -> ());
      s'
    in
            (* Do the children then postprocess *)
    ChangeDoChildrenPost (s', postProc)

      (* Copy blocks since they are mutable *)
  method vblock (b: block) = 
    ChangeDoChildrenPost ({b with bstmts = b.bstmts}, fun x -> x)


  method vglob _ = E.s (bug "copyFunction should not be used on globals")
end

(** Replace a statement with the result of inlining *)
let replaceStatement (host: fundec)                         (* The host *)
                     (inlineWhat: varinfo -> fundec option) (* What to inline *)
                     (replLabel: string -> string)          (* label 
                                                             * replacement *)
                     (anyInlining: bool ref)                (* will set this 
                                                             * to true if we 
                                                             * did any 
                                                             * inlining *)
                     (amap: (string, exp list list) H.t)
                     depth
                     (s: stmt) : stmt = 
  match s.skind with 
    Instr il when il <> [] -> 
      let prevrstmts: stmt list ref = ref [] in (* Reversed list of previous 
                                                 * statements *)
      let prevrinstr: instr list ref = ref [] in (* Reverse list of previous 
                                                  * instructions, in this 
                                                  * statement *)
      let emptyPrevrinstr () = 
        if !prevrinstr <> [] then begin
          prevrstmts := mkStmt (Instr (List.rev !prevrinstr)) :: !prevrstmts;
          prevrinstr := []
        end
      in
        
      let rec loop (rest: instr list)      (* Remaining instructions *)
          : unit = 
        match rest with 
          [] -> (* Done *) ()

        | (Call (lvo, Lval (Var fvi, NoOffset), args, l) as i) :: resti -> begin
            if debug then 
              E.log "Checking whether to inline %s\n" fvi.vname;
          (** Modified by Ming-Hsien Tsai --> *)
          (** the following three lines are used to add assertions to prevent non-termination *)
          let _ =
            if !insert_nonterm_assertions then
              let terminstrs = mkNeqs args (try H.find amap fvi.vname with Not_found -> []) in
              let (tvs, instrs) = mkTemps host args in
              let _ = addArgs amap fvi.vname (List.map (fun v -> Lval (Var v, NoOffset)) tvs) in
              let _ = prevrinstr := instrs@terminstrs@(!prevrinstr) in
              () in
          let replo: fundec option = inlineWhat fvi in
(*          
            let replo: fundec option = 
              match inlineWhat fvi with 
                Some repl -> 
                  if repl.svar.vid = host.svar.vid then begin
                    ignore (warn "Inliner encountered recursion in inlined function %s" 
                              host.svar.vname);
                    None
                  end else
                    Some repl
              | None -> None
            in
*)
          (** <-- Modified by Ming-Hsien Tsai *)
           match replo with  
           | None -> prevrinstr := i :: !prevrinstr;
               loop resti
                 
           | Some repl -> begin
               anyInlining := true;
               (** Modified by Ming-Hsien Tsai --> *)
                (* E.log "Done inlining\n"; *)
               (** <-- Modified by Ming-Hsien Tsai *)

               (* We must inline *)
               (* Prepare the mapping of local variables *)
               let vmap : varinfo IH.t = IH.create 13 in 
               let replVar (vi: varinfo) = 
                 if vi.vglob then vi
                 else 
                   try IH.find vmap vi.vid
                   with Not_found -> begin
                     E.s (bug "Cannot find the new copy of local variable %s" 
                            vi.vname)
                   end
               in
               (* Do the actual arguments, and keep extending prevrinstr *)
               let rec loopArgs (args: exp list) (formals: varinfo list) = 
                 match args, formals with 
                   [], [] -> ()
                 | (a :: args'), (f :: formals') -> begin
                     (* We must copy the argument even if it is already a 
                      * variable, to obey call by value *)
                     (* Make a local and a copy *)
                     let f' = makeTempVar host ~name:f.vname f.vtype in
                     prevrinstr := (Set (var f', a, l)) :: !prevrinstr;
                     IH.add vmap f.vid f';
                     
                     loopArgs args' formals'
                 end
                 | _, _ -> E.bug "Invalid number of arguments"
               in
               loopArgs args repl.sformals;
               
               (* Copy the locals *)
               List.iter (fun loc -> 
                 let loc' = makeTempVar host ~name:loc.vname loc.vtype in 
                 IH.add vmap loc.vid loc') repl.slocals;
               
               
               (* Make the return statement *)
               let (ret : stmt), (retvar: varinfo option) = 
                 let rt, _, isva, _ = splitFunctionType repl.svar.vtype in
                 match rt with 
                   TVoid _  -> mkStmt (Instr []), None
                 | _ -> begin
                     match lvo with 
                       None -> mkStmt (Instr []), None
                     | Some lv -> 
                         (* Make a return variable *)
                         let rv = makeTempVar 
                             host ~name:("ret_" ^ repl.svar.vname) rt in
                         mkStmtOneInstr (Set (lv, Lval (var rv), l)), Some rv
                 end
               in
               ret.labels <- [Label (replLabel ("Lret_" ^ repl.svar.vname),
                                     l, false)];
               let oldBody = repl.sbody in 
               (* Now replace the body *)
               (try
                 ignore (visitCilFunction 
                           (new copyBodyVisitor host repl.svar replVar 
                              retvar replLabel ret) 
                           repl);
                 currentLoc := l;
                 let body' = repl.sbody in 
                 (** Modified by Ming-Hsien Tsai --> *)
                 let actuals' = List.map (fun vinfo -> Lval (Var vinfo, NoOffset)) (List.map (fun f -> IH.find vmap f.vid) repl.sformals) in
                 let retvar' = match retvar with None -> None | Some vinfo -> Some (Var vinfo, NoOffset) in
                 let comment = mkCallComment fvi retvar' repl.sformals actuals' ^ ";depth=" ^ string_of_int depth in
                 body'.bstmts <- mkStmt (Instr [mkComment ("Unwind Begin: " ^ comment)])::
                   body'.bstmts@[mkStmt (Instr [mkComment ("Unwind End: " ^ comment)])];
                 (** <-- Modified by Ming-Hsien Tsai *)
                 (* Replace the old body in the function to inline *)
                 repl.sbody <- oldBody;
                 
                 emptyPrevrinstr ();
                 prevrstmts := ret :: (mkStmt (Block body')) :: !prevrstmts
               with Recursion -> 
                 ignore (warn "Encountered recursion in function %s" 
                           repl.svar.vname);
                 prevrinstr := i :: !prevrinstr);

               loop resti
           end
        end
        | i :: resti -> 
            prevrinstr := i :: !prevrinstr; 
            loop resti
      in
      loop il;

      emptyPrevrinstr ();
      if List.length !prevrstmts > 1 then 
        s.skind <- Block (mkBlock (List.rev !prevrstmts));

      s

  | _ -> s
            
            
(** Apply inlining to a function, modify in place *)
let doFunction  (host: fundec) (* The function into which to inline *)
                (inlineWhat: varinfo -> fundec option) (* The functions to 
                                                        * inline, as a 
                                                        * partial map 
                                                        * from varinfo to 
                                                        * body *)
                (anyInlining: bool ref)                (* Will be set to true 
                                                        * if any inlining 
                                                        * took place *)
                (amap: (string, exp list list) H.t)
                depth
    : unit = 
  if debug then 
    E.log "Doing inlining for %s\n" host.svar.vname;

  (* Scan the host function and build the alpha-conversion table for labels *)
  let labTable: (string, unit A.alphaTableData ref) H.t = H.create 5 in
  ignore (visitCilBlock 
            (object 
              inherit nopCilVisitor
              method vstmt (s: stmt) = 
                List.iter 
                  (fun l ->
                    match l with 
                      Label(ln, _, _) -> 
                        ignore (A.registerAlphaName ~alphaTable:labTable 
                                  ~undolist:None ~data:() ~lookupname:ln)
                    | _ -> ())
                  s.labels;
                DoChildren
                  
            end)
            host.sbody);
  (* Now the label replacement function *)
  let replLabel (ln: string) : string = 
    let ln', _ = A.newAlphaName 
        ~alphaTable:labTable ~undolist:None ~lookupname:ln ~data:() in
    ln'
  in
  (* Now scan the function to do the inlining *)
  let body' : block = 
    visitCilBlock (object
      inherit nopCilVisitor
      method vstmt (s: stmt) = 
        ChangeDoChildrenPost (s, 
                              replaceStatement host inlineWhat 
                                replLabel anyInlining amap depth)
    end) host.sbody in 
  host.sbody <- body';
  ()


(** Apply inlining to a whole file *)
let doFile (inlineWhat: varinfo -> fundec option) (* What to inline. See 
                                                   * comments for [doFunction] *)
           (fl: file) = 
(** Modified by Ming-Hsien Tsai --> *)
(*
  iterGlobals fl (fun g -> 
    match g with 
      GFun(fd, l) -> 
        (* Keep doing inlining until there is no more. We will catch 
         * recursion eventually when we want to inline a function into itself*) 
        let anyInlining = ref true in
        while !anyInlining do
          anyInlining := false;
          doFunction fd inlineWhat anyInlining
        done

    | _ -> ())
*)
  (* We should do unwinding in a BFS manner. *)
  let anyInlining = ref true in
  let amap = H.create 0 in
  let flag = !insert_nonterm_assertions in
  let _ = insert_nonterm_assertions := false in
  let _ = 
    while !anyInlining do
      let _ = anyInlining := false in
      iterGlobals fl (fun g ->
        match g with
          GFun (fd, l) -> 
            doFunction fd inlineWhat anyInlining amap 0
        | _ -> ())
    done in
  insert_nonterm_assertions := flag
(** <-- Modified by Ming-Hsien Tsai *)
      
(** Modified by Ming-Hsien Tsai --> *)
let doit (fl: file) toinline = 
  (* Scan the file and build the hashtable of functions to inline *)
  let inlineTable: (string, fundec) H.t = H.create 5 in 
  visitCilFile (object
    inherit nopCilVisitor
    method vfunc (fd: fundec) =
      if List.mem fd.svar.vname toinline then 
(** <-- Modified by Ming-Hsien Tsai *)
(*        H.add inlineTable fd.svar.vname fd; *)
(* Since we may have recursive functions, make a copy of the original function
   definitions to prevent exponential unwinding. *)
        H.add inlineTable fd.svar.vname (copyFunction fd fd.svar.vname);
(** Modified by Ming-Hsien Tsai --> *)
      SkipChildren
  end) fl;
  let inlineWhat (vi: varinfo) : fundec option = 
    try Some (H.find inlineTable vi.vname)
    with Not_found -> None
  in
  (* Give warnings if we cannot find some fundecs *)
  List.iter (fun fn -> 
    if not (H.mem inlineTable fn) then 
      ignore (warn "Cannot inline function %s because we cannot find its definition" fn))
    toinline;

  doFile inlineWhat fl
(** <-- Modified by Ming-Hsien Tsai *)
















(** Added by Ming-Hsien Tsai to apply inlining to specified functions in a function definition --> *)
let unwindFile (inlineWhat: varinfo -> fundec option) (fl: file) host count = 
  let anyInlining = ref true in
  let amap = H.create 10 in
  let _ = 
    if !insert_nonterm_assertions then
      visitCilFileSameGlobals (object
        inherit nopCilVisitor
        method vglob g =
          match g with
            GFun (fd, l) ->
              let (tvs, instrs) = mkTemps fd (List.map (fun v -> Lval (Var v, NoOffset)) fd.sformals) in
              let _ = addArgs amap fd.svar.vname (List.map (fun v -> Lval (Var v, NoOffset)) tvs) in
              let _ = fd.sbody.bstmts <- (mkStmt (Instr instrs))::fd.sbody.bstmts in
              DoChildren
          | _ -> DoChildren
      end) fl in
  let c = ref 0 in
  while !anyInlining && !c < count do
    let _ = anyInlining := false in
    let _ = incr c in
    iterGlobals fl (fun g ->
      match g with
        GFun (fd, l) -> 
          if fd.svar.vname = host then
            doFunction fd inlineWhat anyInlining amap !c
      | _ -> ())
  done

let unwind (fl: file) host target_selector count = 
  let inlineTable: (string, fundec) H.t = H.create 5 in 
  visitCilFile (object
    inherit nopCilVisitor
    method vfunc (fd: fundec) =
      (if target_selector fd then
          H.add inlineTable fd.svar.vname (copyFunction fd fd.svar.vname));
      SkipChildren
  end) fl;
  let inlineWhat (vi: varinfo) : fundec option = 
    try Some (H.find inlineTable vi.vname)
    with Not_found -> None
  in
  unwindFile inlineWhat fl host count
(** <-- Added by Ming-Hsien Tsai *)
