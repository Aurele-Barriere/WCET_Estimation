#!/bin/bash

#This script implements our methos for high-level WCET estimation with Constraint Programming and Abstract Interpretation.
#First, we create .ai files for each programm point, that contains the constract from Abstract Interpretation. We use here Interproc.
#The we create all CSPs for a constraint solver to solve.
#Then we create a final COP to solve. Its syntax used here is AbSolute's.

ABSOLUTE=~/Stage/AbSolute-master/solver.opt
ABSOLUTEFLAGS=
OCAML=ocaml
INTERPROC=interproc.opt
INTERPROCFLAGS="-widening 300 300 -depth 300 -domain box"
WCETANALYSIS=./wcetanalysis
INPUT=./input

echo "Executing abstract interpretation"
$INTERPROC $INTERPROCFLAGS $INPUT > airesult

#one comment -> one line
tr '\n' ' ' < airesult > airesult2
sed -i 's/\/\*/\n\/\*/g' airesult2
sed -i 's/\*\//\*\/\n/g' airesult2
cp airesult2 airesult
rm airesult2


#cleaning the result

#selecting only the lines with constraints
grep '\/\*' airesult > aiconstraints
#removing the color of it
sed -i 's/\x1b\[[0-9;]*m//g' aiconstraints
#removing [| |] and end of constraints
sed -i 's/\[|/ /g' aiconstraints
sed -i 's/|]/;/g' aiconstraints
sed -i 's/\*\// /g' aiconstraints
#removing what's left of the code
sed -i -n -e 's/^.*\/\* //p' aiconstraints
#removing the 2 first characters (L
sed -i 's/^..//g' aiconstraints
# we only keep the line number
sed -i 's/ [^ ]*//'  aiconstraints
#top means no constraints
sed -i 's/top//g' aiconstraints

#create one file per line
echo "creating the AI constraints"
while read line;
do
    echo $line > line
    echo $line | awk '{print $1}' > name
    touch $(cat name)".ai"
    sed -e 's/^\w*\ *//' line  > constraint
    cat constraint >> $(cat name)".ai"
done < aiconstraints

#in case one edge doesn't have a line
touch 0.ai
echo " " >> 0.ai


#creating all .csp files, edgetime and kirchhoff
echo "creating all CSP files"
#$OCAML analysis.ml
$WCETANALYSIS
#replacing true
for edge in *.csp
do
    sed -i 's/true/(0=0)/g' $edge
done




#the different parts of the final CSP
touch init
touch objective
touch constraints

echo "init{" >> init
echo "constraints{" >> constraints
echo "objective{" >> objective
#taking the opposite of the objective to maximize
echo "-(0" >> objective

#solving each CSP
for edge in *.csp
do
    echo "Solving CSP "$edge
    $ABSOLUTE $ABSOLUTEFLAGS $edge > result
    grep '#solutions: ' result | awk '{print $2}' > bound
    if grep -q 'Unique solution' result; then
	echo "1" >> bound
    fi
    if grep -q 'No solutions' result; then
	echo "0" >> bound
    fi
    ci=${edge%.csp}
    grep $ci" " edgetime | awk '{print $2}' > vi

    echo "int " $ci " = [0;" $(cat bound) "];" >> init
    #echo $ci "<=" $(cat bound) ";" >> constraints
    echo "+" $ci "*" $(cat vi) >> objective

done


#adding kirchhoff's law
echo "" >> constraints
echo "//Kirchhoff's law" >> constraints
cat kirchhoff >> constraints

#creating and solving the final CSP
echo ")" >> objective
echo "}" >> init
echo "}" >> objective
echo "}" >> constraints

cat init objective constraints > final.problem

cat final.problem

$ABSOLUTE -m -t final.problem


#cleaning - not removing final.problem
rm result
rm bound
rm vi
rm init
rm objective
rm constraints
rm airesult
rm name
rm line
rm constraint
rm *.ai
