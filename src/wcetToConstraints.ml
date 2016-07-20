open WcetSyntax
(*open WcetSetree*)

(* This module defines the constraints for AbSolute the constraint solver *)


type constraints  = string
                   
(* symbolic execution tree *)
type setree =
  Formula of constraints
| Or of setree * setree
| And of setree * setree

let setrue = Formula("true")
(* all the *tostring functions are designed to produce a CSP
   with the syntax of AbSolute *)
let rec exprtoconstraint (expr : expr) (vl : versionlist) =
  match expr with
  | Constant(c) -> string_of_int c
  | Var (v) -> let vers = lastversion v vl in String.concat "" [vers.var; string_of_int vers.ind]
  | Plus(a,b) -> String.concat " " ["("; exprtoconstraint a vl; "+"; exprtoconstraint b vl; ")"]
  | Minus(a,b) -> String.concat " " ["("; exprtoconstraint a vl; "-"; exprtoconstraint b vl; ")"]
  | Mod(a,b) -> String.concat " " ["("; exprtoconstraint a vl; "%"; exprtoconstraint b vl; ")"]
  | Mult(a,b) -> String.concat " " ["("; exprtoconstraint a vl; "*"; exprtoconstraint b vl; ")"]
  | Opposite (a) -> String.concat " " ["-"; exprtoconstraint a vl]

let rec testtoconstraint (test : test) (vl : versionlist) =
  match test with
  | Or(a,b) -> String.concat " " ["("; testtoconstraint a vl; "||"; testtoconstraint b vl; ")"]
  | And(a,b) -> String.concat " " ["("; testtoconstraint a vl; "&&"; testtoconstraint b vl; ")"]
  | Equal(a,b) -> String.concat " " ["("; exprtoconstraint a vl; "="; exprtoconstraint b vl; ")"]
  | Neq(a,b) ->  String.concat " " ["("; exprtoconstraint a vl; "!="; exprtoconstraint b vl; ")"]
  | Leq(a,b) ->  String.concat " " ["("; exprtoconstraint a vl; "<="; exprtoconstraint b vl; ")"]  
  | Less(a,b) ->  String.concat " " ["("; exprtoconstraint a vl; "<"; exprtoconstraint b vl; ")"]
  | Geq(a,b) ->  String.concat " " ["("; exprtoconstraint a vl; ">="; exprtoconstraint b vl; ")"]
  | Great(a,b) ->  String.concat " " ["("; exprtoconstraint a vl; ">"; exprtoconstraint b vl; ")"]
                              


let rec settoconstraint (set : setree) =
  match set with
  | Formula(s) -> s
  | Or(a,b) -> let sta = settoconstraint a in
               let stb = settoconstraint b in
               String.concat " " ["("; sta; "||"; stb; ")"]
  | And(a,b) -> let sta = settoconstraint a in
                let stb = settoconstraint b in
                String.concat " " ["("; sta; "&&"; stb; ")"]



let assigntoconstraint (var : variable) (expr : expr) (versionlist : versionlist) =
  String.concat "" [exprtoconstraint (Var(var)) versionlist;
                    " = ";
                    exprtoconstraint expr versionlist;]

let newassigntoconstraint (v : variable) (e : expr) (newvers : version) (current_version : versionlist) =
  global_version_relations := String.concat "" [exprtoconstraint (Var(v)) current_version;
		                           " = ";
		                           exprtoconstraint e (newvers::current_version);
		                           ";\n";
		                           !global_version_relations
		                          ] 
  

let vartostring (v : variable) =
  String.concat "" [v; string_of_int 0]


