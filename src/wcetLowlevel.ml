open WcetSyntax
open WcetCfg
open Printf       

(* This module creates the objective function of the final COP by creating a edgetime file *)       

       
(* execution time for each edge coming from a node *)            
let node_time (node : node) =
  (* these are arbitrary values for now *)
  match node with
  | Start(a) -> 0
  | End(a) -> 0
  | Join(a,b,c) -> 0
  | Assign(v,e,a,b) -> 1
  | Test(t,a,b,c) -> 1

(* writes each edge next to its execution time for the objective of the final problem *)
let each_edge (time : int) (edge : edge) =
  String.concat "" ["c"; string_of_int edge; " "; string_of_int time; "\n"]

(* creates the edgetime file *)
let create_edgetime (cfg : cfg) =
  let rec edgetime (nodelist : cfg) =
    match nodelist with
    | [] -> ""
    | node::l -> String.concat "" (List.append (List.map
                                                  (each_edge (node_time node))
                                                  (outedge node))
                                               [(edgetime l)])
  in let string = edgetime cfg in
     let time = open_out "edgetime" in
     fprintf time "%s" string;
     close_out time


                 
       
