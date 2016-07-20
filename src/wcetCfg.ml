open WcetSyntax
open WcetUtil       

(* This module defines the CFG type and gives functions to use it *)
       
(* new types for construction of CFG*)
type edge = int
type edgelist = edge list


(* a cfg node *)
type node = | Start of edge
	    | End of edge
	    | Join of edge * edge * edge
	    | Assign of variable * expr * edge * edge
	    | Test of test * edge * edge * edge 

type cfg = node list

(* correspondance between one edge and one line of code or one program point *)
type edgeline = {edge : edge; line : int}


                  
let rec edgetoline (edge : edge) (list : edgeline list) =
  match list with
  | [] -> 0
  | e::m when e.edge = edge -> e.line
  | e::m -> edgetoline edge m


(* the list of all edges coming from a given node *)
let outedge (node : node) = 
  match node with
  | Start (a) -> [a]
  | End (a) -> []
  | Join (a,b,c) -> [c]
  | Assign (v,e,a,b) -> [b]
  | Test (t,a,b,c) -> [b;c]

(* the list of all edges going to a given node *)                        
let inedge (node : node) = 
  match node with 
  | Start (a) -> []
  | End (a) -> [a]
  | Join (a,b,c) -> [a;b]
  | Assign (v,e,a,b) -> [a]
  | Test (t,a,b,c) -> [a]

(* returns the node that an edge comes from *)
let rec beginning (cfg : cfg) (edge : edge) =
  match cfg with 
  | [] -> failwith "edge not in CFG"
  | node :: m when inlist edge (outedge node) -> node
  | node :: m -> beginning m edge 

(* returns the node where an edge goes *)
let rec ending (cfg : cfg) (edge : edge) =
  match cfg with
  | [] -> failwith "edge not in CFG"
  | node :: m when inlist edge (inedge node) -> node
  | node :: m -> ending m edge 


(* the list of all edges reachabmle from a given edge *)
let sons (e : edge) (cfg : cfg) =
  let seen = ref [e] in
  let rec rec_sons (edge : edge) =
    match outedge (ending cfg edge) with
    | [] -> ()
    | [a] when inlist a (!seen) -> ()
    | [a] -> seen := a::(!seen); rec_sons a
    | [a;b] when inlist a (!seen) -> begin
        if inlist b (!seen) then () else
          seen :=b::(!seen); rec_sons b end
    | [a;b] when inlist b (!seen) -> seen := a::(!seen); rec_sons a
    | [a;b] -> seen := a::(!seen); rec_sons a; seen := b::(!seen); rec_sons b 
    | _ -> failwith "your cfg hast too many edges"
  in rec_sons e;
     !seen

                        

(* get the edgelist *)
let rec alledges (nodelist : cfg) =
  match nodelist with
  | [] -> []
  | n::l -> List.append (outedge n) (alledges l)
