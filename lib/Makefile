2OCAMLC   = ocamlfind ocamlc -g
OCAMLMLI = ocamlfind ocamlc
OCAMLOPT = ocamlfind opt -unsafe -noassert -inline 100
OCAMLOPT = ocamlfind opt -g
OCAMLDEP = ocamldep.opt
OCAMLDOC = ocamldoc -html
#CONTRAINTES = ./contraintes_coop
INCLUDES = -I .
#LIBS_USR = facile.cma
#LIBS_OPAM = -package zmq
#LIBS_OPT = $(LIBS_USR:.cma=.cmxa)
#LIBS_BIN = $(LIBS_USR)
SCML = geom.ml const.ml aircraft.ml env.ml fly2d.ml
SCCMO = $(SCML:.ml=.cmo)
SCCMX = $(SCML:.ml=.cmx)
all: opt
byte: orca.out
opt: orca.opt
orca.out: $(SCCMO)
	$(OCAMLC) -o $@ $(SCCMO)
orca.opt: $(SCCMX)
	$(OCAMLOPT) -o $@ $(SCCMX)
DIRS = sol reduced
.SUFFIXES: .ml .mli .cmi .cmo .cmx
.ml.cmo:
	$(OCAMLC) $(INCLUDES) $(LIBS_OPAM) -c $<
.mli.cmi:
	$(OCAMLMLI) $(INCLUDES) $<
.ml.cmx:
	$(OCAMLOPT) $(INCLUDES) $(LIBS_OPAM) -c $<
.depend:
	$(OCAMLDEP) *.mli *.ml > .depend
include .depend
init:
	mkdir -p $(DIRS)
cleanall: clean #cleandoc
clean:
	rm -f *.cm* *.annot *.o *.out *.opt *.a *~ .depend
