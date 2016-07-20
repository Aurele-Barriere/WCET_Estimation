open WcetSyntax
open WcetCfg       
open Printf

(* This module write the constraints of the node law *)
       
(* write the node's law for one node for AbSolute *)
let nodelawtostring (n : node) =
  match n with 
  | Start(a) -> String.concat "" ["c"; string_of_int a; " = 1;\n"]
  | End(a) -> String.concat "" ["c"; string_of_int a; " = 1;\n"]
  | Join(a,b,c) -> String.concat "" ["c"; string_of_int a;
                                     " + c"; string_of_int b;
                                     " = c"; string_of_int c;
                                     ";\n"]
  | Assign(v,e,a,b) -> String.concat "" ["c"; string_of_int a;
                                         " = c"; string_of_int b;
                                         ";\n"]
  | Test(t,a,b,c) -> String.concat "" ["c"; string_of_int a;
                                       " = c"; string_of_int b;
                                       " + c"; string_of_int c;
                                       ";\n"]
(* wrote the node's law for all nodes *)
let rec kirchhoff (cfg : cfg) =
  match (cfg : cfg) with
  | [] -> "\n"
  | n::l ->  String.concat "" [nodelawtostring n  ; kirchhoff l]


(* creates the kirchhoff file used for the final problem *)
let create_kirchhoff (cfg : cfg) = 
  let kir = open_out "kirchhoff" in
  fprintf kir "%s" (kirchhoff cfg);
  close_out kir
