(* check the presence of an element in a list *)
let rec inlist a l = match l with
  | e::m when (e = a) -> true
  | e::m -> inlist a m
  | [] -> false  

(* returns the list given by the intersection of two lists *)
let rec listinter l m = 
  match m with
  | [] -> []
  | a::n when (inlist a l) -> a::(listinter l n)
  | a::n -> listinter l n


(* add to a list the elements of another that aren't already in the list *)
let rec listappend_unique l m =
  match m with
  | [] -> l
  | e::n when inlist e l -> listappend_unique l n
  | e::n -> listappend_unique (e::l) n
