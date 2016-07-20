open WcetSyntax
open WcetUtil
open WcetCfg


(* This module gets the control variables globally *)
 

let global_control_variables (cfg : cfg) = 
  let rec test_variables (l : node list) =
    match l with
    | [] -> []
    | Test(t,i,j,k)::m -> listappend_unique (test_variables m) (usedvariables_test t)
    | n::m -> test_variables m in
  let control_vars = ref (test_variables cfg) in
  let change = ref true in
  let rec add_var (l : node list) =
    match l with
    | [] -> ()
    | Assign(v,e,i,j)::m when (inlist v (!control_vars)) -> begin
      let card = List.length (!control_vars) in 
      control_vars := listappend_unique (!control_vars) (usedvariables e);
      if List.length (!control_vars) > card then change := true;
      add_var m
    end 
    | n::m -> add_var m
  in 
  while (!change) 
  do
    change := false;
    add_var cfg;
  done;
    !control_vars

      
let control_variables (edge : edge) (cfg : cfg) = global_control_variables cfg

