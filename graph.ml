module type GraphType =
sig
  type vertex
  type v_info
  type edge
end

module Make =
  functor (GT : GraphType) -> struct

module M = Map.Make (struct type t = GT.vertex 
			    let compare = compare 
		     end)
module S = Set.Make(struct type t = GT.vertex
			   let  compare = compare
		    end)

type node = { succs : GT.edge M.t; preds : GT.edge M.t; info : GT.v_info; }
type t = node M.t


let empty : t = M.empty 
let is_empty : t -> bool  = M.is_empty

let size : t -> int = M.cardinal

(* operations for the vertices *)

let mem : GT.vertex -> t -> bool = M.mem 
let add_v v v_info g =
  if mem v g then 
    M.add v { (M.find v g) with info = v_info } g
  else
    M.add v { succs = M.empty; preds = M.empty; info = v_info } g
(* fails if v is not in g *)
let remove_v v g = 
  assert(M.mem v g);
  let { succs = ss; preds = ps } = M.find v g in
  (* removes v from preds of the adjoint vertices *)
  let g' = M.fold
	     (fun u _ g -> 
	      let n = M.find u g in
	      M.add u { n  with preds = M.remove v n.preds } g)
	     ss g
  in
  (* removes v from succs of the adjoint vertices *)
  let g'' = M.fold
	     (fun u _ g -> 
	      let n = M.find u g in
	      M.add u { n  with succs = M.remove v n.succs } g)
	     ps g'
  in 
  M.remove v g''
  

(* returns v_info not vertex. fails if v is not ing *)
let find v graph = (M.find v graph).info

let vertices : t -> GT.vertex list -> GT.vertex list = 
  M.fold (fun v _ xs -> v :: xs)

let out_deg v graph = M.cardinal (M.find v graph).succs
let in_deg v graph = M.cardinal (M.find v graph).preds

(* operations for the edges *) 

let mem_e u v g = 
  if (M.mem u g && M.mem v g)
  then M.mem u (M.find v g).preds && M.mem u (M.find v g).preds
  else false
(* fails if u and v are not in g *)
let add_e u v e g = 
  assert (M.mem u g && M.mem v g);
  let g' = 
    let n = M.find u g in
    M.add u { n with succs = M.add v e n.succs } g 
  in
  let n = M.find v g' in
  M.add v { n with preds = M.add u e n.preds } g'
(* fails if u and v are not in g *)
let remove_e u v g = 
  assert (M.mem u g && M.mem v g);
  let g' = 
    let n = M.find u g in
    M.add u { n with succs = M.remove v n.succs } g
  in
  let n = M.find v g' in
  M.add v { n with preds = M.remove u n.preds } g'

(* fails if u and v are not in g *)
let find_e u v g = 
  assert (M.mem u g && M.mem v g);
  let n = M.find u g in
  M.find v n.succs 

let edges g = 
  M.fold 
    (fun u n acc -> 
	  (M.fold 
	     (fun v _ acc' -> (u, v) :: acc')
	     n.succs
	     []) @ acc)
    g []
		     
let preds v g =
  M.fold (fun v _ acc -> v :: acc) (M.find v g).preds []
let succs v g =
  M.fold (fun v _ acc -> v :: acc) (M.find v g).succs []


let reverse g =
  M.map (fun p -> { p with succs = p.preds; preds = p.succs }) g

(* f : vertex -> v_info -> v_info *)
let map f g =
  M.mapi (fun v n -> { n with info = f v n.info }) g
let fold f g init = M.fold (fun v n acc -> f v n.info acc) g init
let iter f g = M.iter (fun v n -> f v n.info) g    
let fold_e f g init = M.fold (fun u n acc -> M.fold (fun v e acc -> f u v e acc) n.succs acc) g init

(* assumes that the graph is a DAG                *
 * returns a list of vertices in topoloical order *)

let top_sort g = 
  let vs = ref [] in
  let vis = ref S.empty in
  let rec dfs stack v = 
    if not (S.mem v !vis) then (
      vis := S.add v !vis;
      let stack = S.add v stack in
      M.iter (fun w _ -> dfs stack w) (M.find v g).succs;
      vs := v :: !vs)
    else if S.mem v stack then 
      failwith "error: a loop has been found during topological sort\n"
  in
  M.iter (fun v _ -> dfs S.empty v) g;
        !vs


end

