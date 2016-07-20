open String
open List
open Printf
open WcetSyntax
open WcetSetree       
open WcetCfg
open WcetUtil       
open WcetToConstraints
open WcetCsp
open WcetKirchhoff
open WcetLowlevel            
open WcetGetcfg

(* This modules uses the CFG given in WcetGetcfg, then create all CSPs, the node's law constraints and low-level analysis *)
       
let _ =
  create_all_csps cfg varlist edgelinelist;
  print_string "wrote all .csp files\n";
  create_kirchhoff cfg;
  print_string "Nodes law written in file kirchhoff\n";
  create_edgetime cfg;
  print_string "created edgetime file\n"


