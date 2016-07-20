open WcetCfg
open WcetSyntax
open WcetUtil
open WcetToConstraints       


(* This module creates the Symbolic Execution Tree for an edge of a CFG *)

(* creating a setree from a cfg and an edge *)
let construct_setree (graph : cfg) (e : edge) =   
  let rec rec_setree (cfg : cfg) (edge : edge) (seen_edges : edgelist) (affected_vars : varlist) (current_version : versionlist) =
    (* sides effects will create constraints between versions, in global_version_relations *)
    if inlist edge seen_edges then setrue
    else 
      match beginning cfg edge with 
      | Start(a) -> setrue
      | End(a) -> failwith "an edge shouldn't be coming from an end node"
      | Join(a,b,c) -> Or ( rec_setree cfg a (edge::seen_edges) affected_vars current_version,
			    rec_setree cfg b (edge::seen_edges) affected_vars current_version)
      | Assign(v,e,a,b) -> let tree =
	match (listinter (usedvariables e) affected_vars, listinter affected_vars [v]) with 
	| (x,y) when (x != [] || y != []) -> setrue (* when one of the variable has already been assigned *)
	| (_,_) -> let tree2 = 
		     match inlist v (usedvariables e) with
		     | true ->
		       let newvers = newversion v in 
		       newassigntoconstraint v e newvers current_version;
		       rec_setree cfg a (edge::seen_edges) affected_vars (newvers::current_version)
		     | false -> And ( Formula (assigntoconstraint v e current_version),
				      rec_setree cfg a (edge::seen_edges) (v::affected_vars) current_version) 
		   in tree2
			   in tree
      | Test(t,a,b,c) when (edge = b) -> And ( rec_setree cfg a (edge::seen_edges) affected_vars current_version, 
	match listinter (usedvariables_test t) affected_vars with
	| [] -> Formula(testtoconstraint t current_version)
	| _  -> setrue )
      | Test(t,a,b,c) ->  And ( rec_setree cfg a (edge::seen_edges) affected_vars current_version, 
	match listinter (usedvariables_test t) affected_vars with
	| [] -> Formula(testtoconstraint (not t) current_version)
	| _  -> setrue )
                                in rec_setree graph e [] [] (!global_version_list);;
