open String
open List
open Printf
open WcetSyntax
open WcetSetree       
open WcetCfg
open WcetUtil       
open WcetToConstraints
open WcetControlvars

       
(* this modules creates AbSolute CSPs *)       
(* creates a .csp file according to AbSolute's rules *)
let create_csp (cfg : cfg) (e : edge) (vars : varlist) (aicons : string) =
  let det_vars = control_variables e cfg in
  let string_det_vars = String.concat " " (List.map vartostring det_vars) in
  global_version_list := [];
  global_version_relations := "";
  let filename = String.concat "" ["c"; string_of_int e; ".csp"] in
  let csp = open_out filename in
  fprintf csp "// control variables : ";
  fprintf csp "%s" string_det_vars;
  fprintf csp "\n";
  let rec init_versions (varlist : varlist) =
    match varlist with 
    | v::m -> global_version_list := ({var = v; ind = 0})::(!global_version_list); init_versions m
    | [] -> ()
  in init_versions vars;
  let tree = construct_setree cfg e in
  fprintf csp "init{\n";
  let rec print_vars (l : versionlist) =
    match l with
    | vers :: m -> begin fprintf csp "%s" (String.concat "" ["int "; vers.var; string_of_int vers.ind; " = [-20; 100];\n"]); print_vars m end
    | [] -> fprintf csp "}\n"
  in print_vars (!global_version_list);
  fprintf csp "\nconstraints{\n";
  fprintf csp "\n//Abstract Interpretation constraints\n";
  fprintf csp "%s" aicons;
  fprintf csp "\n\n//symbolic execution tree constraint\n";
  fprintf csp "%s" (settoconstraint (tree));
  fprintf csp  ";\n\n//version relations\n";
  fprintf csp "%s" (!global_version_relations);
  fprintf csp  "\n}\n";

  close_out csp



(* read line.ai files to get a constraint from Abstract Interpretation *)  
let get_ai_constraints (edge : edge) (ellist : edgeline list) =  
  let line = edgetoline edge ellist in
  let filename = String.concat "" [string_of_int line; ".ai"] in
  let aifile = open_in filename in
  let cons = input_line aifile in
  close_in aifile;
  cons

    
(* creates all the .csp for a cfg *)
let create_all_csps (cfg : cfg) (varlist : varlist) (ellist : edgeline list) =          
  let edgelist = alledges cfg in
  let rec each_csp (list : edgelist) =
    match list with
    | [] -> ()
    | e::l -> create_csp cfg e varlist (get_ai_constraints e ellist); each_csp l
  in each_csp edgelist
