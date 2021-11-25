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
  let eval_binop binop i1 i2 =
    let open Int64 in
    match binop with
    | Add -> add i1 i2 | Sub -> sub i1 i2 | Mul -> mul i1 i2
    | Shl -> shift_left i1 (to_int i2)
    | Lshr -> shift_right_logical i1 (to_int i2)
    | Ashr -> shift_right i1 (to_int i2)
    | And -> logand i1 i2 | Or -> logor i1 i2 | Xor -> logxor i1 i2
  in
  let eval_cnd cnd i1 i2 =
    let b = match cnd with
      | Eq -> i1 = i2 | Ne -> i1 != i2
      | Slt -> i1 < i2 | Sle -> i1 <= i2
      | Sgt -> i1 > i2 | Sge -> i1 >= i2
    in if b then 1L else 0L
  in
  let eval_op op d =
    match op with
    | Const i -> SymConst.Const i
    | Id id -> begin match UidM.find_opt id d with
        | Some SymConst.Const i' -> SymConst.Const i'
        | Some SymConst.NonConst ->  SymConst.NonConst
        | Some SymConst.UndefConst -> SymConst.UndefConst
        (* todo: not sure if this is the case, or why it makes sense, hmm *)
        | None -> SymConst.UndefConst
      end
    | _ -> failwith "constprop eval_op not sure how to handle op"
  in
  let eval_ops op1 op2 res_fun =
    match eval_op op1 d, eval_op op2 d with
    | Const i1, Const i2 ->
      UidM.add u (SymConst.Const (res_fun i1 i2)) d
    | NonConst, NonConst | NonConst, _ | _, NonConst ->
      UidM.add u SymConst.NonConst d
    | UndefConst, UndefConst | UndefConst, _ | _, UndefConst ->
      (* todo: normalize not doing anything later rip *)
      (* UidM.add u SymConst.UndefConst *) d
  in
  match i with
  | Binop (binop, _, op1, op2) -> 
    eval_ops op1 op2 (eval_binop binop)
  | Icmp (cnd, _, op1, op2) ->
    eval_ops op1 op2 (eval_cnd cnd)
  | Store _ | Call (Void, _, _) ->
    (* todo: normalize not doing anything later rip *)
    (* UidM.add u SymConst.UndefConst *) d
  | _ -> UidM.add u SymConst.NonConst d

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
              (* todo: should change, probably *)
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
  let eval_op uid op cb =
    match op with
    | Id id -> (match UidM.find_opt id (cb uid) with
        | Some (Const i) -> Ll.Const i | _ -> op)
    | _ -> op
  in
  let eval_insn (uid, insn) cb =
    match insn with
    | Binop (bop, ty, op1, op2) ->
      Binop (bop, ty, eval_op uid op1 cb, eval_op uid op2 cb)
    | Load (ty, op) ->
      Load (ty, eval_op uid op cb)
    | Store (ty, op1, op2) ->
      (* storing in op2, so op2 can't be Const *)
      Store (ty, eval_op uid op1 cb, op2) 
    | Icmp (cnd, ty, op1, op2) ->
      Icmp (cnd, ty, eval_op uid op1 cb, eval_op uid op2 cb)
    | Call (ty, op, ls) ->
      let new_ls = List.map (fun (ty', op') -> ty', eval_op uid op' cb) ls in
      Call (ty, op, new_ls)
    (* other insns shouldn't involve constprop *)
    | _ -> insn
  in 
  let eval_term (uid, term) cb =
    match term with
    | Ret (ty, op) -> begin match op with
        | None -> uid, term
        | Some op' -> uid, Ret (ty, Some (eval_op uid op' cb))
      end
    | Cbr (op, lbl1, lbl2) ->
      uid, Cbr (eval_op uid op cb, lbl1, lbl2)
    | _ -> uid, term
  in
  
  let cp_block (l:Ll.lbl) (cfg:Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in (* b : Ll.block *)
    let cb = Graph.uid_out cg l in (* cb : Ll.uid -> Fact.t / SymConst.t UidM.t *)
    let res_insns = List.map (fun (uid, insn) -> uid, eval_insn (uid, insn) cb) b.insns in
    let res_term = eval_term b.term cb in
    let res_blk = { insns = res_insns; term = res_term } in
    { blocks = LblM.update (fun _ -> res_blk) l cfg.blocks;
      preds = cfg.preds;
      ret_ty = cfg.ret_ty;
      args = cfg.args }
  in
  
  LblS.fold cp_block (Cfg.nodes cfg) cfg
