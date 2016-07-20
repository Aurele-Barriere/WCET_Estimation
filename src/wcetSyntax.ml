(* This module defines the syntax of the language used, and the version variables used for the SEtree *)

type variable = string

type version = {var : variable; ind : int}

type expr = Constant of int
          | Var of variable
          | Plus of expr * expr
          | Minus of expr * expr
          | Mod of expr * expr
          | Mult of expr * expr
          | Opposite of expr


type test = 
          | Or of test * test
          | And of test * test
          | Equal of expr * expr
          | Neq of expr * expr
          | Leq of expr * expr
          | Less of expr * expr
          | Geq of expr * expr
          | Great of expr * expr


let rec not (test : test) =
  match test with 
  | Equal(a,b) -> Neq(a,b)
  | Neq(a,b) -> Equal(a,b)
  | Leq(a,b) -> Less(b,a)
  | Less(a,b) -> Leq(b,a)
  | Geq(a,b) -> Leq(a,b)
  | Great(a,b) -> Less(a,b)
  | Or(a,b) -> And(not a, not b)
  | And(a,b) -> Or(not a, not b)


type varlist = variable list
type versionlist = version list



(* a list of all variables used by an expression *)
let rec usedvariables (expr : expr) =
  match expr with 
  | Constant(_) -> []
  | Var(v) -> [v]
  | Plus(a,b) -> List.append (usedvariables a) (usedvariables b)
  | Minus(a,b) -> List.append (usedvariables a) (usedvariables b)
  | Mod(a,b) -> List.append (usedvariables a) (usedvariables b)
  | Mult(a,b) -> List.append (usedvariables a) (usedvariables b)
  | Opposite(a) -> usedvariables a

(* a list of all variables used by a test *)
let rec usedvariables_test (test : test) =
  match test with 
  | Equal(a,b) -> List.append (usedvariables a) (usedvariables b)
  | Neq(a,b) -> List.append (usedvariables a) (usedvariables b)
  | Leq(a,b) -> List.append (usedvariables a) (usedvariables b)
  | Less(a,b) -> List.append (usedvariables a) (usedvariables b)
  | Geq(a,b) -> List.append (usedvariables a) (usedvariables b)
  | Great(a,b) -> List.append (usedvariables a) (usedvariables b)
  | Or(a,b) -> List.append (usedvariables_test a) (usedvariables_test b)
  | And(a,b) -> List.append (usedvariables_test a) (usedvariables_test b)



(* given a version list, returns the most recent version for a given variable *)
let rec lastversion (v : variable) (l : versionlist) =
  match l with
  | vers :: m when (vers.var = v) -> vers
  | vers :: m -> lastversion v m
  | [] -> failwith "uninitialized variable"

(* versions should be added at the beginning *)
let addversion (vers : version) (l : versionlist) = vers :: l


let global_version_list = ref []
let global_version_relations = ref ""
(* the string where all versions relations will be printed *)                                   

(* creates a new version for a given variable *)
let newversion (v : variable) =
  let last = lastversion v (!global_version_list) in
  global_version_list := addversion ({var = v; ind = last.ind + 1}) (!global_version_list);
  {var = v; ind = last.ind + 1} 

