open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst =
  struct
    type t = NonConst           (* Uid may take on multiple values at runtime *)
           | Const of int64     (* Uid will always evaluate to const i64 or i1 *)
           | UndefConst         (* Uid is not defined at the point *)

    let compare s t =
      match (s, t) with
      | (Const i, Const j) -> Int64.compare i j
      | (NonConst, NonConst) | (UndefConst, UndefConst) -> 0
      | (NonConst, _) | (_, UndefConst) -> 1
      | (UndefConst, _) | (_, NonConst) -> -1

    let to_string : t -> string = function
      | NonConst -> "NonConst"
      | Const i -> Printf.sprintf "Const (%LdL)" i
      | UndefConst -> "UndefConst"

    
  end

(* The analysis computes, at each program point, which UIDs in scope will evaluate 
   to integer constants *)
type fact = SymConst.t UidM.t



(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
 *)
let insn_flow (u,i:uid * insn) (d:fact) : fact =
  let d' = UidM.remove u d in
  let eval_id id i =
    match UidM.find_opt id d with
    | Some SymConst.NonConst -> UidM.add u SymConst.NonConst d'
    | Some SymConst.UndefConst -> UidM.add u SymConst.UndefConst d'
    | Some SymConst.Const i' -> UidM.add u (SymConst.Const (Int64.add i i')) d'
    | None -> d
  in
  match i with
  | Binop (_, _, op1, op2) | Icmp (_, _, op1, op2) ->
    begin match op1, op2 with
      (* todo: need to evaluate result with two constants 
       * (or operands that eventually evaluate to consts) *)
      | Const _, Const i2 -> UidM.add u (SymConst.Const i2) d'
      | Id id1, Id id2 ->
        begin match UidM.find_opt id1 d with
          | Some SymConst.NonConst -> UidM.add u SymConst.NonConst d'
          | Some SymConst.UndefConst -> UidM.add u SymConst.UndefConst d'
          | _ -> begin match UidM.find_opt id2 d with
              | Some SymConst.NonConst -> UidM.add u SymConst.NonConst d'
              | Some SymConst.UndefConst -> UidM.add u SymConst.UndefConst d'
              | _ -> d
            end
        end
      | Const i, Id id -> eval_id id i
      | Id id, Const i -> eval_id id i
      | _ -> d
    end
  | Store _ | Call (Void, _, _) ->
    (* todo: normalize not doing anything later rip *)
    (* UidM.add u SymConst.UndefConst *) d
  | _ -> UidM.add u SymConst.NonConst d'

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t:terminator) (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow
    
    let normalize : fact -> fact = 
      UidM.filter (fun _ v -> v != SymConst.UndefConst)

    let compare (d:fact) (e:fact) : int  = 
      UidM.compare SymConst.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymConst.to_string v)

    (* The constprop analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful *)
    let combine (ds:fact list) : fact =
      let meet key f1 f2 =
        match f1 with
        | Some f1' -> begin match f2 with
            | Some f2' ->
              if SymConst.compare f1' f2' == 0
              then f1
              (* todo: should change *)
              else Some (SymConst.NonConst)
            | None -> f1
          end
        | None -> f2
      in
      (* todo: why isn't normalize doing anything here rip *)
      List.fold_left (fun ih f ->
          UidM.merge meet ih (normalize f)
        ) UidM.empty ds  
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in = List.fold_right 
    (fun (u,_) -> UidM.add u SymConst.NonConst)
    g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg


(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper 
   functions.                                                                 *)
let run (cg:Graph.t) (cfg:Cfg.t) : Cfg.t =
  let open SymConst in
  

  let cp_block (l:Ll.lbl) (cfg:Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in
    failwith "Constprop.cp_block unimplemented"
  in

  LblS.fold cp_block (Cfg.nodes cfg) cfg
