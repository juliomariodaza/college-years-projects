#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*                                                                           *
#*                  This file is part of the program and library             *
#*         SCIP --- Solving Constraint Integer Programs                      *
#*                                                                           *
#*    Copyright (C) 2002-2008 Konrad-Zuse-Zentrum                            *
#*                            fuer Informationstechnik Berlin                *
#*                                                                           *
#*  SCIP is distributed under the terms of the ZIB Academic Licence.         *
#*                                                                           *
#*  You should have received a copy of the ZIB Academic License              *
#*  along with SCIP; see the file COPYING. If not email to scip@zib.de.      *
#*                                                                           *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# $Id: Makefile,v 1.6 2010/11/22 17:52:45 bzfheinz Exp $

#@file    Makefile
#@brief   Makefile for the SCIP Bidimensional Cutting Stock Problem
#@author  Alberto De Bortoli


#-----------------------------------------------------------------------------
# paths
#-----------------------------------------------------------------------------

SCIPDIR         =       /Users/alberto/Desktop/SCIP/scip-1.2.0

#-----------------------------------------------------------------------------
# include default project Makefile from SCIP
#-----------------------------------------------------------------------------
include $(SCIPDIR)/make/make.project




#-----------------------------------------------------------------------------
# Main Program
#-----------------------------------------------------------------------------

MAINNAME	=	bcsp_PLI
MAINOBJ		=	bcsp_main.o master.o slave.o
MAINSRC		=	$(addprefix $(SCIPDIR)/src/,$(MAINOBJ:.o=.cpp))
MAINDEP		=	$(SCIPDIR)/src/depend.cppmain.$(OPT)
BINDIR		=	./bin

GENNAME     = 	generator
GENSRC		=	./src/generator.cpp
GENOBJ	 	=	$(BINDIR)/$(GENNAME)
HEURISTICNAME	=	heuristic
HEURISTICSRC	=	./src/heuristic.cpp
HEURISTICOBJ	=	$(BINDIR)/$(HEURISTICNAME)

MAIN		=	$(MAINNAME).$(BASE).$(LPS)$(EXEEXTENSION)
MAINFILE	=	$(BINDIR)/$(MAIN)
MAINSHORTLINK	=	$(BINDIR)/$(MAINNAME)
MAINOBJFILES	=	$(addprefix $(OBJDIR)/,$(MAINOBJ))
CXXFLAGS	=	-Wall


#-----------------------------------------------------------------------------
# Rules
#-----------------------------------------------------------------------------

ifeq ($(VERBOSE),false)
.SILENT:	$(MAINFILE) $(MAINOBJFILES) $(MAINSHORTLINK)
endif

.PHONY: all
all:            $(SCIPDIR) $(MAINFILE) $(MAINSHORTLINK)

.PHONY: lint
lint:		$(MAINSRC)
		-rm -f lint.out
		$(SHELL) -ec 'for i in $^; \
			do \
			echo $$i; \
			$(LINT) $(SCIPDIR)/lint/scip.lnt +os\(lint.out\) -u -zero \
			$(FLAGS) -UNDEBUG -UWITH_READLINE -UROUNDING_FE $$i; \
			done'

.PHONY: doc
doc:
		cd doc; $(DOXY) $(MAINNAME).dxy

$(MAINSHORTLINK):	$(MAINFILE)
		@rm -f $@
		cd $(dir $@) && ln -s $(notdir $(MAINFILE)) $(notdir $@)

$(OBJDIR):
		@-mkdir -p $(OBJDIR)

$(BINDIR):
		@-mkdir -p $(BINDIR)

.PHONY: clean
clean:
		-rm -rf $(OBJDIR)/* $(MAINFILE)

.PHONY: depend
depend:		$(SCIPDIR)
		$(SHELL) -ec '$(DCXX) $(FLAGS) $(DFLAGS) $(MAINSRC) \
		| sed '\''s|^\([0-9A-Za-z\_]\{1,\}\)\.o *: *$(SRCDIR)/\([0-9A-Za-z\_]*\).cpp|$$\(OBJDIR\)/\2.o: $(SRCDIR)/\2.cpp|g'\'' \
		>$(MAINDEP)'

-include	$(MAINDEP)

$(MAINFILE):	$(BINDIR) $(OBJDIR) $(SCIPLIBFILE) $(LPILIBFILE) $(MAINOBJFILES)
		@echo "-> linking $@"
		$(LINKCXX) $(MAINOBJFILES) \
		$(LINKCXX_L)$(SCIPDIR)/lib $(LINKCXX_l)$(SCIPLIB)$(LINKLIBSUFFIX) \
                $(LINKCXX_l)$(OBJSCIPLIB)$(LINKLIBSUFFIX) $(LINKCXX_l)$(LPILIB)$(LINKLIBSUFFIX) \
                $(OFLAGS) $(LPSLDFLAGS) \
		$(LDFLAGS) $(LINKCXX_o)$@

$(OBJDIR)/%.o:	$(SRCDIR)/%.c
		@echo "-> compiling $@"
		$(CC) $(FLAGS) $(OFLAGS) $(BINOFLAGS) $(CFLAGS) -c $< $(CC_o)$@

$(OBJDIR)/%.o:	$(SRCDIR)/%.cpp
		@echo "-> compiling $@"
		$(CXX) $(FLAGS) $(OFLAGS) $(BINOFLAGS) -c $< $(CXX_o)$@

generator: $(GENSRC) $(BINDIR)
		g++ $(CXXFLAGS) $(GENSRC) -o $(GENOBJ)

heuristic: $(HEURISTICSRC) $(BINDIR)
		g++ $(CXXFLAGS) $(HEURISTICSRC) -o $(HEURISTICOBJ)


#---- EOF --------------------------------------------------------------------
