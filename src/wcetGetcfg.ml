open WcetSyntax
open WcetCfg


(* This module sould construct the CFG from an input file : input, containing the code to analyze. *)

(* Since the code for using SawjaCard's CFG cannot be given, you'll have to input the CFG here manually *)



let cfg = [
    Start(1);
    Assign("i",Constant(0),1,2);
    Join(2,10,3);
    Assign("j",Constant(0),3,4);
    Join(4,7,5);
    Test(Less(Var("j"),Constant(25)),5,6,8);
    Assign("j",Plus(Var("j"),Constant(1)),6,7);
    Test(Less(Var("i"),Constant(75)),8,9,11);
    Assign("i",Plus(Var("i"),Constant(1)),9,10);
    End(11)]
            
let edgelinelist = [
    {edge = 1; line = 2};
    {edge = 2; line = 3};
    {edge = 3; line = 4};
    {edge = 4; line = 5};
    {edge = 5; line = 5};
    {edge = 6; line = 6};
    {edge = 7; line = 7};
    {edge = 8; line = 8};
    {edge = 9; line = 8};
    {edge = 10; line = 9};
    {edge = 11; line = 10}]

                     
let varlist = ["i";"j"]
