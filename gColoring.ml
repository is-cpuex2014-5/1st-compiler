(* this algorithm is based on the virtual code written in the tiger book.       *
 * few comments are written, so read the book will help you understand this code*)

(***** types and datas *****)

(* set for coalescing *)
module CS = Set.Make (struct type t = Id.t * Id.t let compare = compare end)

(* set for spilling (cost, varname) *)
module SS = Set.Make (struct type t = float * Id.t let compare = compare end)

(* data structure                  *
 * work lists, sets, stacks etc.   *
 * (tiger book p.225)              *)
type data = {
  (* precolored variables. note that regs are also in this set *)
  precolored : S.t;
  (* number of the available registers *)
  k : int;
  (* v -> cost(v) (cost is a heuristic function) *)
  cost : float M.t;
  (* variables s.t. deg(v) < k *)
  simplify_worklist : S.t;
  (* variables s.t. deg(v) < k /\ OperandofMov(v) *)
  freeze_worklist : S.t;
  (* variables s.t. deg(v) >= k *)
  spill_worklist : SS.t;
  (* stack of removed vertices *)
  select_stack : Id.t list;
  (* set of spilled variables *)
  spilled_nodes : S.t;
  (* Move operations : (x, y) means  x <- y. *)
  (* Move operaions that can be coalesced *)
  worklist_moves : CS.t;
  (* Move operaions that cannot be coalesced yet *)
  active_moves : CS.t;
  (* coalesced vertex  -> new vertex *)
  alias : Id.t M.t;
  (* var -> reg name *)
  color : Id.t M.t;
  (* vertices removed from the inference graph *)
  removed_set : S.t;
  (* edge set of the inference graph *)
  adj_set : CS.t;
  (* adjoint list of the inference graph *)
  adj_list : S.t M.t;
  (* v -> deg(v) *)
  degree : int M.t;
  (* variable -> move operations *)
  move_list : CS.t M.t;
}

(***** utility function *****)

(* operator to make a function as an infix operator *)
let (<|) x y = y x and (|>) x y = x y

let deg x data =
  assert (M.mem x data.degree);
  M.find x data.degree

let append x v movs =
  assert (if M.mem x movs then true else (Printf.fprintf stderr "%s\n" x; false));
  M.add x (CS.add v (M.find x movs)) movs

let remove_spill x data = SS.remove (M.find x data.cost, x) data.spill_worklist
let add_spill x data = SS.add (M.find x data.cost, x) data.spill_worklist
let mem_spill x data = SS.mem (M.find x data.cost, x) data.spill_worklist

let is_insignificant x data = not (S.mem x data.precolored) && deg x data < data.k

let rec get_alias x data =
  if M.mem x data.alias then
    get_alias (M.find x data.alias) data
  else
    x

let add_edge x y data =
  if x <> y && not (CS.mem (x, y) data.adj_set) then
    let data = { data with adj_set = CS.add (x, y) (CS.add (y, x) data.adj_set) } in
    let add x y data =
      if not (S.mem x data.precolored) then
        { data with
          adj_list = M.add x (S.add y (M.find x data.adj_list)) data.adj_list;
          degree = M.add x (deg x data + 1) data.degree; }
      else
        data
    in
    add x y (add y x data)
  else
    data

let adjacent x data =
  assert (M.mem x data.adj_list);
  (M.find x data.adj_list) <|S.diff|> data.removed_set

let node_moves x data =
  (M.find x data.move_list) <|CS.inter|> (data.active_moves <|CS.union|> data.worklist_moves)

let enable_moves x data =
  let moves = (M.find x data.move_list) <|CS.inter|> data.active_moves in
  CS.fold  (* forall m in nodemoves[n] *)
    (fun mov data ->
     { data with
       active_moves = CS.remove mov data.active_moves;
       worklist_moves = CS.add mov data.worklist_moves })
    moves data

let move_related x data =
  not (CS.is_empty (node_moves x data))

let decrement_degree x data =
  if S.mem x data.precolored then data
  else
    let d = deg x data in
    let data = { data with degree = M.add x (d - 1) data.degree } in
    if d = data.k then
        let data = enable_moves x data in
        let data = S.fold enable_moves (adjacent x data) data in
        let data = { data with spill_worklist = remove_spill x data } in
        let data =
          if move_related x data then
            { data with freeze_worklist = S.add x data.freeze_worklist }
          else
            { data with simplify_worklist = S.add x data.simplify_worklist }
        in
          data
      else
        data




(***** build *****)

let init_cost func data =
  let data' =
    let f stmt data = 
      let add x data =
        if M.mem x data.cost then
          { data with cost = M.add x (M.find x data.cost +. 1.0) data.cost }
        else data
      in
      let data = List.fold_right add stmt.Block.use data in
      let data = List.fold_right add (List.map fst stmt.Block.def) data in
      data
    in
    Block.CFG.fold 
      (fun _ stmts acc -> List.fold_left (fun acc stmt -> f stmt acc) acc stmts)
      func.Block.blocks data
  in
  (* (number of the use)  / deg(v) is the heuristic *)
  let data'' =
    { data' with
      cost =
        M.mapi
          (fun x cost ->
	   assert (M.mem x data'.cost);
           M.find x data'.cost /. float_of_int (deg x data'))
          data'.cost }
  in
  data''
    
(* build a inference graph of type [Int | Float] *)
let build typ func =
  (* initial is the objects those will be colored *)
  let (data, initial) =
    let precolored = S.of_list ((match typ with Type.Float ->  "$f00" :: Asm.allfregs | _ -> "$r00" :: Asm.allregs)) in
    let f' stmt =
      List.fold_right
        (fun (x, t) acc ->
         (* note that the type needs to be checked as well *)
         if (t = Type.Float) = (typ = Type.Float) && not (S.mem x precolored) then
           S.add x acc
         else acc)
        stmt.Block.def
    in
    let initial = Block.CFG.fold 
		    (fun _ stmts acc -> List.fold_left (fun acc stmt -> f' stmt acc) acc stmts)
		    func.Block.blocks 
		    S.empty
    in
    (*Printf.fprintf stderr "initials\n";
    S.iter (fun x -> Printf.fprintf stderr "%s " x) initial;
    Printf.fprintf stderr "\n\n";*)
    let data = (* initializing the data structure *)
      { precolored = precolored;
        k = S.cardinal precolored;
        cost = S.fold (fun x y -> M.add x 0.0 y) initial M.empty;
        simplify_worklist = S.empty;
        freeze_worklist = S.empty;
        spill_worklist = SS.empty;
        select_stack = [];
        spilled_nodes = S.empty;
        active_moves = CS.empty;
        worklist_moves = CS.empty;
        alias = M.empty;
        color = M.empty;
        removed_set = S.empty;
        adj_set = CS.empty;
        adj_list = S.fold (fun x y -> M.add x S.empty y) initial M.empty;
        degree = S.fold (fun x y -> M.add x 0 y) initial M.empty;
        move_list = S.fold (fun x y -> M.add x CS.empty y) (initial <|S.union|> precolored) M.empty; }
    in
      (data, initial)
  in

  let is_target x = S.mem x data.precolored || S.mem x initial in

  let liveness = Liveness.add_zero_reg (Liveness.f func.Block.blocks) typ in

  let data =
    Block.CFG.fold
      (fun b stmts data ->
         snd
           (List.fold_right
              (fun stmt (live, data) ->
               let (live, data) =
                 match (stmt.Block.def, stmt.Block.inst, typ = Type.Float) with
                 | [x, _], Block.FMov(y), true
                 | [x, _], Block.Mov(y), false ->
                    (S.remove y live,
                     { data with
                       move_list = append x (x, y) (append y (x, y) data.move_list);
                       worklist_moves = CS.add (x, y) data.worklist_moves })
                 | _ -> (live, data)
               in
	       let data =
                 let def = List.filter is_target (List.map fst stmt.Block.def) in
                 let set = S.filter is_target live in
                 List.fold_right
                   (fun x -> S.fold (fun y data -> add_edge x y data) set) 
		   def data
	       in 
	       (Liveness.transfer stmt live, data))
              stmts 
	      (let blive_out = M.find b liveness.Liveness.out in blive_out, data)))
      func.Block.blocks data
  in
  (* initialize the spill cost*)
  let data = init_cost func data in
  (* make_worklist() *)
  let data =
    S.fold
      (fun x data ->
       match () with
       | _ when deg x data >= data.k ->
          { data with spill_worklist = add_spill x data }
       | _ when move_related x data ->
          { data with freeze_worklist = S.add x data.freeze_worklist }
       | _ ->
          { data with simplify_worklist = S.add x data.simplify_worklist })
      initial data
  in
  data
    
    
(***** simplify *****)

let simplify data =
  let x = S.choose data.simplify_worklist in
  let data' =
    { data with
         simplify_worklist = S.remove x data.simplify_worklist;
         select_stack = x :: data.select_stack;
         removed_set = S.add x data.removed_set }
  in
    S.fold decrement_degree (adjacent x data') data'

(***** coalesce *****)

(* Briggs *)
let conservative xs data =
  let k = S.fold 
	    (fun x acc ->
	     if deg x data >= data.k then acc + 1 else acc)
	    xs 0
  in
  k < data.k

let add_worklist x data =
  if is_insignificant x data && not (move_related x data) then (* TODO *)
    { data with
      freeze_worklist = S.remove x data.freeze_worklist;
      simplify_worklist = S.add x data.simplify_worklist; }
  else
    data

let ok t x data = 
  S.mem t data.precolored ||
  CS.mem (t, x) data.adj_set ||
  is_insignificant t data

let combine x y data =
  let data =
    assert (S.mem y data.freeze_worklist || mem_spill y data);
    if S.mem y data.freeze_worklist then
      { data with freeze_worklist = S.remove y data.freeze_worklist }
    else
      { data with spill_worklist = remove_spill y data }
  in
  (* alias[y] <- x *)
  let data = { data with alias = M.add y x data.alias } in
  (* remove y from the graph *)
  let data = { data with removed_set = S.add y data.removed_set } in
  let data = enable_moves y data in
  let data =
    { data with
      move_list = M.add x ((M.find x data.move_list) <|CS.union|> (M.find y data.move_list)) data.move_list } in
  (* contract x y *)
  let data =
    S.fold
      (fun z data ->
       let data = add_edge z x data in
       let data = decrement_degree z data in
       data)
      (adjacent y data) data
  in
  let data =
    if not (S.mem x data.precolored) &&
       deg x data >= data.k &&
       S.mem x data.freeze_worklist
    then
      { data with
        freeze_worklist = S.remove x data.freeze_worklist;
        spill_worklist = add_spill x data; }
    else
      data
  in
    data

let coalesce data =
  let (x, y) as mov = CS.choose data.worklist_moves in
  let data = { data with worklist_moves = CS.remove mov data.worklist_moves } in
  let x = get_alias x data in
  let y = get_alias y data in
  let (x, y) =
    if S.mem y data.precolored then
      (y, x)
    else (x, y)
  in
    if x = y then
      add_worklist x data
    else if S.mem y data.precolored || CS.mem (x, y) data.adj_set then
      if S.mem y data.precolored then 
	(assert (S.mem x data.precolored);
	 let data = add_worklist x data in
	 let data = add_worklist y data in
	 data)
      else
	data
    else if (S.mem x data.precolored &&
	    S.for_all (fun t -> ok t x data) (adjacent y data)) ||
            (not (S.mem x data.precolored) &&
            conservative (S.of_list [x; y]) data)
    then
      let data = combine x y data in
      let data = add_worklist x data in
      data
    else
      { data with active_moves = CS.add mov data.active_moves }

(***** freeze *****)

let freeze_moves x data =
  assert (x = get_alias x data);
  CS.fold (* forall mov in NodeMoves(x) *)
    (fun ((u, v) as mov) data ->
       let u = get_alias u data in
       let v = get_alias v data in 
       assert (x = u || x = v);
       let t = if x = u then v else u in
       let data = { data with active_moves = CS.remove mov data.active_moves } in
         if not (move_related t data) && S.mem t data.freeze_worklist then
           { data with
                 freeze_worklist = S.remove t data.freeze_worklist;
                 simplify_worklist = S.add t data.simplify_worklist }
         else
           data)
    (node_moves x data) data


let freeze data =
  let x = S.choose data.freeze_worklist in
  let data =
    { data with
          freeze_worklist = S.remove x data.freeze_worklist;
          simplify_worklist = S.add x data.simplify_worklist }
  in
    freeze_moves x data


(***** spill *****)

let select_spill data =
  let (cost, x) as y = SS.min_elt data.spill_worklist in
  let data =
    { data with
      spill_worklist = SS.remove y data.spill_worklist;
      simplify_worklist = S.add x data.simplify_worklist }
  in
    freeze_moves x data


(***** select *****)

let assign_colors data =
  (* var -> color *)
  let get_color x data =
    if M.mem x data.color then
      Some (M.find x data.color)
    else if S.mem x data.precolored then
      Some (x)
    else
      None
  in
  let data =
    List.fold_left (* while SelectStack not empty *)
      (fun data x -> (* x = pop(SelectStack) *)
       let ok_colors =
         S.fold
           (fun y acc ->
            let y = get_alias y data in
            match get_color y data with
            | Some (c) -> S.remove c acc
            | _ -> acc)
           (M.find x data.adj_list) data.precolored 
       in
       if S.is_empty ok_colors then
         let () = Printf.fprintf stderr"actual spill %s\n" x in
         (* actual spill *)
         { data with spilled_nodes = S.add x data.spilled_nodes }
       else
         { data with color = M.add x (S.choose ok_colors) data.color })
      data data.select_stack
  in
  let data =
    M.fold (* forall n in coalescedNodes *)
      (fun x _ data ->
       let y = get_alias x data in
       match get_color y data with
       | Some (c) -> { data with color = M.add x c data.color }
       | _ -> assert (S.mem y data.spilled_nodes); data)
      data.alias data
  in
  data

(***** rewrite *****)

let rewrite_program data func =
  S.iter (fun x -> assert (not (Asm.is_reg x))) data.spilled_nodes;
  (* generate a vitual pos on the stack *)
  let stackpos =
    S.fold
      (fun x acc ->
       M.add x (Id.genid ("stack." ^ x )) acc)
      data.spilled_nodes M.empty
  in
  let func =
    let cfg = func.Block.blocks in
    (* add save after def of spilled variables *)
    let tenv = ref M.empty in
    let cfg =
      Block.map_stmt_list
        (function
           | { Block.def = def } as stmt when List.exists (fun (x, _) -> S.mem x data.spilled_nodes) def ->
	      (* make env for type  *)
              let () = List.iter (fun (x, t) -> if S.mem x data.spilled_nodes then tenv := M.add x t !tenv) def in
              let (env, saves) =
                 List.fold_left
                   (fun (env, saves) (x, t) ->
                    if M.mem x stackpos then
                      let x' = Id.genid x in
		      let pos = M.find x stackpos in
                      (M.add x x' env,
                       Block.to_stmt (Block.Save(x',pos)) [] :: saves)
                    else
                      (env, saves))
                   (M.empty, []) def
              in
              let def = List.map (fun (x, t) -> if M.mem x env then (M.find x env, t) else (x, t)) def in
                 { stmt with Block.def = def } :: saves
           | stmt -> [stmt])
        cfg
    in
    (* add restore before def of spilled variables *)
    let cfg =
      Block.map_stmt_list
        (function
          | { Block.use = use } as stmt when List.exists (fun x -> S.mem x data.spilled_nodes) use ->
             let (env, restores) =
                 List.fold_left
                   (fun (env, restores) x ->
                      if M.mem x stackpos then
                        let x' = Id.genid x in
			let pos = M.find x stackpos in
                        (M.add x x' env,
                         Block.to_stmt
			   (Block.Restore(pos))
                           [x', (assert (M.mem x !tenv); M.find x !tenv)]
			 :: restores)
                      else
                        (env, restores))
                   (M.empty, []) use
             in
             let stmt = Block.replace_stmt env stmt in
             restores @ [stmt]
          | stmt -> [stmt])
        cfg
    in
    { func with Block.blocks = cfg }
  in
    func

(***** main *****)

let rec f func =
  let rec g typ func =
    Printf.fprintf stderr "Register allocation (type = %s)\nthis may take several minutes\n" 
		   (match typ with Type.Float -> "float" | _ -> "int");
    
    let data = build typ func in

    let rec h = function
      | { simplify_worklist = w } as data when not (S.is_empty w) -> h (simplify data)
      | { worklist_moves = w } as data when not (CS.is_empty w) -> h (coalesce data)
      | { freeze_worklist = w } as data when not (S.is_empty w) -> h (freeze data)
      | { spill_worklist = w } as data when not (SS.is_empty w) -> h (select_spill data)
      | data -> data
    in
    
    let data = h data in
    let data = assign_colors data in 
    if S.is_empty data.spilled_nodes then (* finishes *)
        (data.color, func)
    else (* when spills, rewrite the profram and retry *)
      g typ (rewrite_program data func)
  in

  let (color1, func) = g Type.Float func in
  let (color2, func) = g Type.Int func in
  let color =
    M.merge
      (fun x a b ->
       (if not (Asm.is_reg x) then () else (Printf.fprintf stderr "%s\n" x; assert false));
         match (a, b) with
           (* type conflict  *)
         | Some a, Some b -> Printf.fprintf stderr "%s -> %s and %s\n" x a b;  assert false
         | Some _, _ -> a
         | _ -> b)
      color1 color2
  in

  let replace x = if M.mem x color then M.find x color else x in

  (* replace the vars with regs and return *)
  let func =
    { func with
      Block.blocks  = Block.map_stmt (Block.replace_stmt' color) func.Block.blocks;
      Block.args = List.map replace func.Block.args;
      Block.fargs = List.map replace func.Block.fargs;
    }
  in
  func
    
(***** for debug *****)
let sanitize b = 
  let r = Str.regexp "\\." in
  Str.global_replace r "_" b

let output_for_graphviz oc data =
  
  Printf.fprintf oc "graph cfg {\n";
  Printf.fprintf oc "node [shape=circle];\n";
  M.iter
    (fun v _  ->
     Printf.fprintf oc "%s [label=\"%s\"];\n" (sanitize v) v)
     data.adj_list;
  CS.iter
    (fun (u, v) ->
     Printf.fprintf oc "%s -- %s ;\n" (sanitize u) (sanitize v)) 
    data.adj_set;
  CS.iter
    (fun (u, v) ->
     Printf.fprintf oc "%s -- %s [style = dotted];\n" (sanitize u) (sanitize v)) 
    data.worklist_moves;
  Printf.fprintf oc "}\n"

