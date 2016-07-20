analysis : wcetAnalysis.ml wcetSyntax.ml wcetSetree.ml wcetCfg.ml wcetToConstraints.ml wcetUtil.ml wcetCsp.ml wcetKirchhoff.ml wcetLowlevel.ml wcetGetcfg.ml wcetControlvars.ml
	ocamlc -c wcetUtil.ml
	ocamlopt -c wcetUtil.ml
	ocamlc -c wcetSyntax.ml
	ocamlopt -c wcetSyntax.ml
	ocamlc -c wcetToConstraints.ml
	ocamlopt -c wcetToConstraints.ml
	ocamlc -c wcetCfg.ml
	ocamlopt -c wcetCfg.ml
	ocamlc -c wcetControlvars.ml
	ocamlopt -c wcetControlvars.ml
	ocamlc -c wcetGetcfg.ml
	ocamlopt -c wcetGetcfg.ml
	ocamlc -c wcetSetree.ml
	ocamlopt -c wcetSetree.ml
	ocamlc -c wcetCsp.ml
	ocamlopt -c wcetCsp.ml
	ocamlc -c wcetKirchhoff.ml
	ocamlopt -c wcetKirchhoff.ml
	ocamlc -c wcetLowlevel.ml
	ocamlopt -c wcetLowlevel.ml
	ocamlc -c wcetAnalysis.ml
	ocamlopt -c wcetAnalysis.ml
	ocamlc wcetUtil.cmo wcetSyntax.cmo wcetToConstraints.cmo wcetCfg.cmo wcetControlvars.cmo wcetGetcfg.cmo wcetSetree.cmo wcetCsp.cmo wcetKirchhoff.cmo wcetLowlevel.cmo wcetAnalysis.cmo -o wcetanalysis

clean :
	-rm *.cmi
	-rm *.cmo
	-rm *.cmx
	-rm *.o

cleanall : clean
	-rm wcetanalysis
