#################################################################################
#                Gtktop                                                         #
#                                                                               #
#    Copyright (C) 2005-2012 Institut National de Recherche en Informatique     #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License version        #
#    3 as published by the Free Software Foundation.                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU General Public License for more details.                               #
#                                                                               #
#    You should have received a copy of the GNU General Public License          #
#    along with this program; if not, write to the Free Software                #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

#
include master.Makefile

PACKAGES=gtktop,lablgtk2.init

OF_FLAGS=-package $(PACKAGES)

INCLUDES= \
	-I $(OCAML_SRC_DIR)/parsing \
	-I $(OCAML_SRC_DIR)/driver \
	-I $(OCAML_SRC_DIR)/utils

COMPFLAGS= -annot $(INCLUDES)

LIB=ocamltop.cma
OCAMLTOP=ocamltop

CMOFILES=otop_messages.cmo otop_outvalue.cmo
CMIFILES=$(CMOFILES:.cmo=.cmi)

all: $(OCAMLTOP)

$(LIB): $(CMIFILES) $(CMOFILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(OF_FLAGS) $(COMPFLAGS) \
	toplevellib.cma $(CMOFILES)

$(OCAMLTOP): $(LIB) ocamltop.cmo
	$(OCAMLFIND) ocamlc -o $@ $(OF_FLAGS) -linkpkg $(COMPFLAGS) \
	$(LIB) ocamltop.cmo

.PHONY: doc depend

doc: all
	mkdir -p html
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) -t Gtktop -d html -html gtktop.mli gtktop_installation.mli

webdoc: doc
	mkdir -p ../gtktop-gh-pages/refdoc
	cp html/* ../gtktop-gh-pages/refdoc/
	cp web/index.html web/style.css ../gtktop-gh-pages/

.depend depend:
	$(OCAMLDEP) odiff*.ml odiff*.mli > .depend

# installation :
################
install: all
	$(OCAMLFIND) install $(PACKAGE) META LICENSE \
	$(LIB) $(CMIFILES)
	$(MKDIR) $(OCAMLBIN)
	$(CP) mk-ocamltop-gtk $(OCAMLBIN)/

uninstall:
	ocamlfind remove $(PACKAGE)
	$(RM) $(OCAMLBIN)/mk-ocamltop-gtk

# archive :
###########
archive:
	git archive --prefix=gtktop-$(VERSION)/ HEAD | gzip > ../gtktop-gh-pages/gtktop-$(VERSION).tar.gz

# Cleaning :
############
clean:
	rm -f *.cm* *.a *.annot *.o gtktop_base.ml

distclean: clean
	rm -fr master.Makefile gtktop_installation.ml \
		ocaml_config.sh config.status config.log autom4te.cache META

# headers :
###########
HEADFILES=Makefile *.ml *.mli *.ml.in
.PHONY: headers noheaders
headers:
	headache -h header -c ~/.headache_config $(HEADFILES)

noheaders:
	headache -r -c ~/.headache_config $(HEADFILES)

include .depend

# Additional dependencies :
###########################
gtktop_base.ml: gtktop.glade
	$(LABLGLADECC) -hide-default $< > $@

