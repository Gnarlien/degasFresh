# $Id$

#######################################################
# The following may be overridden by Makefile.local

# These are all yes/no flags

# Activate debugging source and compilation?
DEBUG=yes

# Build directly from web files (needs ftangle)?
STANDALONE=yes

# Use Fortran 90?
FORTRAN90=yes

# Using old (2.x) version of NETCDF?
NETCDFV2=no

# The "root" of the degas2 tree, by default this is $HOME, so the .f files
# are in ~/degas2/MACHINE.
#DEGASROOT=$(HOME)/src/degas2
# Set by environment now

ifndef DEGASROOT
  DEGASROOT=..
endif

# Will instruct users to put Triangle here, but allow them to put elsewhere
# if needed.
TRIDIR=$(DEGASROOT)/triangle

# Default settings
FCF77=f77
FCF90=f90
FCMPI77=mpif77
FCMPI90=mpif90
LDF77=$(FCF77)
LDF90=$(FCF90)
LDMPI77=$(FCMPI77)
LDMPI90=$(FCMPI90)
FFLAGS=
F77FLAGS=
DEBUGFLAGS=-g
OPTFLAGS=-O
LIBROOT=/usr/local
PROFILE=
PROF_LD=
# Some systems using a "module" utility
# will set shell variables for these.  In
# turn, gmake will automatically define
# these using those values.
ifndef NETCDF
  NETCDF=-L$(LIBROOT)/lib -lnetcdf
endif
ifndef HDF
  HDF=-L$(LIBROOT)/lib -ldf -ljpeg -lz
endif
MPILIBS=
PVM=-L$(PVM_ROOT)/pvm3/lib/$(PVMARCH) -lfpvm3 -lpvm3
FTANGLE=ftangle
FWEAVE=fweave
LATEX=latex
PDFLATEX=pdflatex
# These are the old tangle flags -- not needed any more since we
# now don't stress tangle so much
# TANGLEFLAGS=-mINLINE -W[ -ytt40000000 -yn8000 -ymb65536 -n/ -n')' -Tv -j
# TANGLEFLAGS=-mINLINE -W[ -n/ -n')' -Tv -j -yn6000 -- it's a little stressed
# Code has evolved enough that we are stressing some more
# TANGLEFLAGS=-mINLINE -W[ -n/ -n')' -Tv -j -yn7000 -yb100000
TANGLEFLAGS=-mINLINE -W[ -n/ -n')' -Tv -j -yn15000 -yb200000 -ybs15000
WEAVEFLAGS=-f -i! -W[ -ybs15000 -ykw800 -ytw40000 -j -n/  # Added the '!', OK? (DPS)
F90MODS=-I.
F90FLAGS=
SYSDEPC=no
COFLAGS=
# These variables facilitate cross compilation, i.e.,
# where STANDALONE=no on a remote machine.  The 
# "unknown"s below indicate that the user needs to
# override these values in a Makefile.local file
TRANSFER=scp
SSHFLAGS=-q
REMOTEMACH=unknown.host.gov
REMOTEDIR=unknown/dir
REMOTELOGIN=source .login
REXEC=ssh
# GRAPH_FILE=HDF4
# GRAPH_FILE=SILO
GRAPH_FILE=SILO_HDF5
MASTER_SAMPLE=no

# Figure out the system type
SYSTEM:=$(shell uname)

ifeq ($(SYSTEM),SunOS)
  MACHINE=SUN
  PVMARCH=SUN4
  NETCDF=-L$(LIBROOT)/lib -lnetcdf -lnsl
  HDF=-L$(LIBROOT)/lib -ldf -ljpeg -R/opt/SUNWspro/lib:/usr/local/lib -lz
  F77FLAGS=-u
  F90MODS=-M.
  FCMPI77=hf77
  OPTFLAGS=-fast
endif
ifeq ($(SYSTEM),AIX)
  MACHINE=IBM
  FCF77=xlf
  FCF90=xlf90
# This removes the "_" from the end of the name
  FFLAGS=-qextname=poly2triangles
  F90FLAGS=-qfixed
  SYSDEPC=yes
endif
ifeq ($(SYSTEM),OSF1)
  MACHINE=ALPHA
#  OPTFLAGS=-fast  <-- math_library_fast and noaccuracy_sensitive 
#                      lead to noticeable differences in results
#  HDF=-L$(LIBROOT)/lib -ldf -ljpeg1 -lz
  HDF=-L$(LIBROOT)/lib -ldf $(LIBROOT)/lib/libjpeg.a -lz
  OPTFLAGS=-O4
endif
ifeq ($(SYSTEM),HP-UX)
  MACHINE=HP
endif
ifeq ($(SYSTEM),IRIX)
  MACHINE=SGI
  PVMARCH=SGI
  OPTFLAGS=-O3
endif
ifeq ($(SYSTEM),IRIX64)
  MACHINE=SGI
  PVMARCH=SGI
  FFLAGS=-64
  OPTFLAGS=-O3
endif
ifeq ($(SYSTEM),Darwin)
  MACHINE=MACOSX
  FFLAGS=
  OPTFLAGS=
endif
ifeq ($(SYSTEM),Linux)
  PVMARCH=LINUX
#
# Both the 32 and 64 bit Linux OS have uname = Linux; use uname -m to
# differentiate.
# 
  IS64:=$(shell uname -m | grep 64)
  ifneq ($(strip $(IS64)),)
    MACHINE=LINUX64
  else
    MACHINE=LINUX
  endif
endif
ifeq ($(findstring sn,$(SYSTEM)),sn)
  MACHINE=CRAY
  PVMARCH=CRAY
  FTANGLE=/afs/pppl.gov/@sys/usr/local/bin/ftangle
  F90MODS=-p.
  F90FLAGS=-Ca
  OPTFLAGS=-O3
endif
# Figure out target machine
TARGET=$(notdir $(shell pwd | sed 's+-[^/]*$$++'))

ifeq ($(TARGET),SGI)
  MPI=yes
endif

ifeq ($(TARGET),LINUX)
  COMPILER=PGROUP
  FORTRAN90=no
endif
ifeq ($(TARGET),LINUX64)
  COMPILER=INTEL
  FORTRAN90=yes
#
# This defaults to pgcc if we don't override
#
  CC=cc
endif

ifneq ($(TARGET),$(MACHINE))
  CROSSCOMPILE=yes
endif

utils: makemodule idtof Makefile

# Cancel some built-in rules
%: %.f
%: %.c
%: %.o
%: %.mod
%.c: %.l
%.r: %.l
%.o %: %.mod
%.p: %.web
%.tex: %.web
%.dvi: %.tex
%.f: %.F
%.o: %.F

# End of variables to be overridden
#######################################################

-include Makefile.local

ifeq ($(STANDALONE),yes)
ifndef SOURCEDIR
  SOURCEDIR=$(DEGASROOT)/src
endif
  vpath %.web $(SOURCEDIR)
  vpath %.hweb $(SOURCEDIR)
  Makefile: $(SOURCEDIR)/Makefile
	cp $< $@
  Makefile.depends: $(SOURCEDIR)/Makefile.depends
	cp $< $@
  idtof: idtof.sh
	cp $< $@
	chmod a+x $@
endif

ALADIR=$(DEGASROOT)/Aladdin

CRAMDIR=$(DEGASROOT)/CRAMD

BSPLINEDIR=$(SOURCEDIR)/EFIT2dg2d

DETECTORFILE=$(shell if ([ -s $(SOURCEDIR)/usr2ddetector.web ]) || ([ -s $(SOURCEDIR)/usr2ddetector.f ]); then echo usr2ddetector; else echo def2ddetector; fi)

PLASMAFILE=$(shell if ([ -s $(SOURCEDIR)/usr2dplasma.web ]) || ([ -s $(SOURCEDIR)/usr2dplasma.f ]); then echo usr2dplasma; else echo def2dplasma; fi)

%.sh: $(SOURCEDIR)/%.sh
	cp $< $@
%.sty: $(SOURCEDIR)/%.sty
	cp $< $@

# Cancel some built-in rules
%: %.f
%: %.c
%: %.h
%: %.mod
%.c: %.l
%.r: %.l
%.o %: %.mod
%.p: %.web
%.tex: %.web
%.dvi: %.tex
%: %.mod

# Set up compilers etc.
ifeq ($(PRINTER),)
 LPRFLAGS=-h
else
 LPRFLAGS=-h -P$(PRINTER)
endif

ifeq ($(DEBUG),yes)
  TANGLEFLAGS+=-mDEBUG
endif

ifeq ($(COMPILER),PGROUP)
  TANGLEFLAGS+=-mPGROUP
  FCF77=pgf90
  FCF90=pgf90
  FCMPI77=mpif90
  FCMPI90=mpif90
  ifeq ($(DEBUG),yes)
#
# These appear to give results very similar to Sun
#
    FFLAGS+=-Kieee -pc 64
  else
    FFLAGS+=-fast	
  endif
else
 ifeq ($(COMPILER),NAGWARE)
  TANGLEFLAGS+=-mNAGWARE
  SYSDEPC=yes
  FCF90=f95
  DEBUGFLAGS=-g90
  F90FLAGS=-mismatch_all -w
 endif
endif
ifeq ($(COMPILER),FUJITSU)
  TANGLEFLAGS+=-mFUJITSU
  FCF90=lf95
#
# The Lahey-Fujitsu compiler has a recommended lists of flags for
# use in debugging. Since they are all for only the compile phase,
# added the COFLAGS variable.  However, even in good code, use of these
# flags generates alot of garbage, so they will be enabled only in
# for testing of new code.
#
#  COFLAGS=--ap --chkglobal --f95 --lst --pca --sav --trace --info --xref
endif
ifeq ($(COMPILER),PATHSCALE)
  TANGLEFLAGS+=-mPATHSCALE
  FCF90=pathf90
  FFLAGS+=-fno-fast-math -mx87-precision=64
endif

ifeq ($(COMPILER),INTEL)
  TANGLEFLAGS+=-mINTEL
  FCF77=ifort
  FCF90=ifort
  FFLAGS+=-pc64 -heap_arrays
endif

ifeq ($(GRAPH_FILE),HDF4)
  TANGLEFLAGS+=-mHDF4
  GRAPH_LIB=$(HDF)
  POST_LIB=$(HDF)
endif
ifeq ($(GRAPH_FILE),SILO)
  TANGLEFLAGS+=-mSILO
  GRAPH_LIB=-lsilo -lm
  POST_LIB=
endif
#
# Note that SILO has multiple options for writing HDF5 formatted
# files (see the manual).  The macro that determines the particular
# one used here is in sysdep.hweb.  If the SILO library was compiled
# with HDF5, SILO_HDF5 must be used (because there is no libsilo.a).
#
ifeq ($(GRAPH_FILE),SILO_HDF5)
  TANGLEFLAGS+=-mSILO_HDF5
  GRAPH_LIB=-lhdf5 -lsiloh5 -lm
endif

FC=$(FCF77)

ifeq ($(NETCDFV2),yes)
  TANGLEFLAGS+=-mNETCDFV2
endif

ifeq ($(MPI),yes)
  TANGLEFLAGS+=-mMPI
  FC=$(FCMPI77)
  FCF90=$(FCMPI90)
  MPILIBSA=$(MPILIBS)
endif

LD=$(FC)
ifeq ($(DEBUG),yes)
  COMPFLAGS=$(DEBUGFLAGS)
#  CFLAGS=-g -I. -I$(TRIDIR)
  CFLAGS=-g -I. 
else
  COMPFLAGS=$(OPTFLAGS)
#  CFLAGS=-O -I. -I$(TRIDIR)
  CFLAGS=-O -I. 
endif

ifeq ($(FORTRAN90),yes)
  TANGLEFLAGS+=-mFORTRAN90
  FC=$(FCF90)
  COMPFLAGS+=$(F90MODS) $(F90FLAGS)
else
  COMPFLAGS+=$(F77FLAGS)
endif
# Have to repeat this here since we don't
# know everything about remote machine yet.
ifdef CROSSCOMPILE
  REMOTESYS:=$(shell $(REXEC) $(SSHFLAGS) $(REMOTEMACH) uname)
  ifeq ($(REMOTESYS),AIX)
    SYSDEPC=yes
  endif
endif

ifeq ($(MASTER_SAMPLE),yes)
  TANGLEFLAGS+=-mMASTER_SAMPLE
endif

ifeq ($(SYSDEPC),yes)
  SYSDEPO=sysdep.$O sysdepc.$O
else
  SYSDEPO=sysdep.$O
endif

ifndef CROSSCOMPILE
  O=o
  %.o: %.f
	$(FC) -c $(FFLAGS) $(COFLAGS) $(COMPFLAGS) $(PROFILE) $<
  %: %.o $(SYSDEPO) string.$O idtof 
	./idtof $(patsubst %.$O,%.f,$^) > $*.id.f
	$(FC) -c $(FFLAGS) $(COMPFLAGS) $*.id.f
	$(LD) $(FFLAGS) $(COMPFLAGS) $(PROFILE) -o $@ $(filter %.$O,$^) $*.id.o $($@_libs) $(PROF_LD)
 DG2D=definegeometry2d.o definegeometry2dc.o triangle.o

%.a: %.o $(SYSDEPO) string.$O idtof 
	./idtof $(patsubst %.$O,%.f,$^) > $*.id.f
	$(FC) -c $(FFLAGS) $(COMPFLAGS) $*.id.f
	ar r $@ $(filter %.$O,$^) $*.id.o
	ranlib $@

%.so: FFLAGS += -fpic
%.so: %.o $(SYSDEPO) string.$O idtof 
	./idtof $(patsubst %.$O,%.f,$^) > $*.id.f
	$(FC) -c $(FFLAGS) $(COMPFLAGS) $*.id.f
	$(FC) -shared -o $@ $(filter %.$O,$^) $*.id.o

else # We're cross compiling
# The approach here is to explicitly tranfer files to the
# remote machine, but maintain locally a timestamp (.ts)
# indicating the age of those files. Note: the order of these
# rules may be important.
  O=f.ts
  define SENDFILES
    $(TRANSFER) $(SSHFLAGS) $< $(REMOTEMACH):$(REMOTEDIR)
    touch $@
  endef
  %.f.ts: %.f
	$(SENDFILES)
  %.f.ts: %.c
	$(SENDFILES)
  triangle.ts: triangle.c triangle.h
	$(TRANSFER) $(SSHFLAGS) $^ $(REMOTEMACH):$(REMOTEDIR)
	touch $@
  Makefile.ts: Makefile
	$(SENDFILES)
  Makefile.depends.ts: Makefile.depends
	$(SENDFILES)
  idtof.ts: idtof
	$(SENDFILES)
  %.o: %.f
  %: %.o
  %.ts: %.$O $(SYSDEPO) string.$O idtof.ts Makefile.ts Makefile.depends.ts
	$(REXEC) $(SSHFLAGS) $(REMOTEMACH) "$(REMOTELOGIN) && cd $(REMOTEDIR) && make $*"
	touch $@
  %: %.ts
	@echo Made $*
  DG2D=definegeometry2d.f.ts definegeometry2dc.f.ts triangle.ts
endif

# This stuff grabs the CVS keyword information from the source files
# and appends it to the code files. Note that idtof separately takes the
# information associated with the Id keyword and inserts it into a
# common block in the .id.f files.

IDENTAPPEND=cat $^ | ident -q | sort | uniq | sed 's/^/c/' >> $@

.PRECIOUS: %.mweb %.tex %.dvi %.f %_mod.f %.cft %.o %.dum fweb.sty fweboptions.sty %.ts

%.f: %.web
	$(FTANGLE) $(TANGLEFLAGS) -m$(TARGET) $<
	$(IDENTAPPEND)

# Special rule for files containing both FORTRAN and C code
# Could we use the commented out pattern rule as a more general
# version of the above .f pattern rule? I don't think the added
# switch has any impact on a pure FORTRAN file.
# This dependency does appear to add to those in Makefile.depends

# %.f %c.c: %.web

ifeq ($(STANDALONE),yes)
definegeometry2d.f definegeometry2dc.c: definegeometry2d.web triangle.h
	$(FTANGLE) $(TANGLEFLAGS) -m$(TARGET) -=c=#c.c $<
	$(IDENTAPPEND)
triangle.%: $(TRIDIR)/triangle.%
	cp $< $@
endif

triangle.o: triangle.c triangle.h
	$(CC) -DTRILIBRARY -O -c $<

ifeq ($(STANDALONE),yes)
# include files
endif

%.mweb: %.hweb makemodule
	./makemodule $< $@

%_mod.f: %.mweb array.hweb sysdep.hweb
	$(FTANGLE) -I$(SOURCEDIR) $(TANGLEFLAGS) -m$(TARGET) -=n=#_mod.f $<

ifeq ($(SYSDEPC),yes)
  ifeq ($(STANDALONE),yes)
    sysdep.f sysdepc.c: sysdep.web macros.hweb
	$(FTANGLE) $(TANGLEFLAGS) -m$(TARGET) -=c=#c.c $<
	$(IDENTAPPEND)
  endif
endif

%.tex: %.web fweb.sty
	$(FWEAVE) $(WEAVEFLAGS) $<

define RUN-LATEX
  if [ -r $*.aux ]; then cp $*.aux $*.aux0; fi
  $(LATEX) $*.tex
  if [ -r $*.aux ]; then cmp -s $*.aux $*.aux0 || $(LATEX) $*.tex; fi
  rm -f $*.aux0
endef

%.dvi: %.tex fweboptions.sty fweb.sty
	$(RUN-LATEX)

define RUN-PDFLATEX
  if [ -r $*.aux ]; then cp $*.aux $*.aux0; fi
  $(PDFLATEX) $*.tex
  if [ -r $*.aux ]; then cmp -s $*.aux $*.aux0 || $(PDFLATEX) $*.tex; fi
  rm -f $*.aux0
endef

%.pdf: %.tex fweboptions.sty fweb.sty
	$(RUN-PDFLATEX)

%.bbl: %.aux ../../degas.bib
	bibtex $*

%.print: %.dvi
ifndef PAGES
	dvips -f $^
#	lpr $(LPRFLAGS) -d $^
else
	dviselect $(PAGES) $^ | lpr $(LPRFLAGS) -d
#	dviselect $(PAGES) $^ | dvips-filter | rsh leo.gat.com lp
endif

%.view: %.dvi
	xdvi $^

%.ps: %.dvi
	dvips -f $^ > $@

# Include dependencies

-include Makefile.depends

all: programs documentation idtof
programs: randomtest geomtest sysdeptest
documentation: random.dvi randomtest.dvi geometry.dvi geomsetup.dvi \
  geomtest.dvi sysdep.dvi vector.dvi

foof:
	echo MACHINE=$(MACHINE) TARGET=$(TARGET) SOURCEDIR=$(SOURCEDIR) STANDALONE=$(STANDALONE) FC=$(FC) FORTRAN90=$(FORTRAN90) CROSSCOMPILE=$(CROSSCOMPILE) MPI=$(MPI) SYSDEPC=$(SYSDEPC) COMPILER=$(COMPILER)

sysdeptest = sysdeptest.$O
sysdeptest_libs = 
sysdeptest: $(sysdeptest) $(foreach arg,$(subst .$O,_mods,$(sysdeptest)),$($(arg)))

randomtest = randomtest.$O random.$O
randomtest_libs =
randomtest: $(randomtest) $(foreach arg,$(subst .$O,_mods,$(randomtest)),$($(arg)))

pvmmaster = pvmmaster.$O random.$O stat.$O geometry.$O readfilenames.$O
pvmmaster_libs = $(NETCDF) $(PVM)
pvmmaster: $(pvmmaster) $(foreach arg,$(subst .$O,_mods,$(pvmmaster)),$($(arg)))

pvmslave = pvmslave.$O random.$O randother.$O stat.$O geometry.$O elementread.$O pmimatread.$O flight.$O reaction.$O interpolate.$O geomfront.$O readfilenames.$O
pvmslave_libs = $(NETCDF) $(PVM)
pvmslave: $(pvmslave) $(foreach arg,$(subst .$O,_mods,$(pvmslave)),$($(arg)))

flighttest = flighttest.$O doflights.$O elementread.$O pmimatread.$O geometry.$O random.$O stat.$O randother.$O reaction.$O interpolate.$O geomfront.$O flight.$O sources.$O collide.$O readfilenames.$O plate.$O degasinit.$O score.$O conversions.$O voigt.$O writeback.$O aux2dsetup.$O geomsetup.$O geomfaces.$O geomutils.$O allocate.$O composite.$O
flighttest_libs = $(NETCDF) $(MPILIBSA)
flighttest: $(flighttest) $(foreach arg,$(subst .$O,_mods,$(flighttest)),$($(arg)))

degas2_xgc = degas2_xgc.$O doflights.$O elementread.$O pmimatread.$O geometry.$O random.$O stat.$O randother.$O reaction.$O interpolate.$O geomfront.$O flight.$O sources.$O collide.$O readfilenames.$O plate.$O degasinit.$O score.$O conversions.$O voigt.$O writeback.$O aux2dsetup.$O geomsetup.$O geomfaces.$O geomutils.$O allocate.$O composite.$O
degas2_xgc_libs = $(NETCDF) $(MPILIBSA)
degas2_xgc.a: $(degas2_xgc) $(foreach arg,$(subst .$O,_mods,$(degas2_xgc)),$($(arg)))
degas2_xgc.so: $(degas2_xgc) $(foreach arg,$(subst .$O,_mods,$(degas2_xgc)),$($(arg)))

poly_to_tri = poly_to_tri.$O geometry.$O allocate.$O readfilenames.$O 
poly_to_tri_libs = $(NETCDF)
poly_to_tri: $(poly_to_tri) $(foreach arg,$(subst .$O,_mods,$(poly_to_tri)),$($(arg)))

ucd_plot = ucd_plot.$O geometry.$O allocate.$O readfilenames.$O elementread.$O pmimatread.$O  reaction.$O interpolate.$O random.$O degasinit.$O
ucd_plot_libs = $(NETCDF) $(GRAPH_LIB)
ucd_plot: $(ucd_plot) $(foreach arg,$(subst .$O,_mods,$(ucd_plot)),$($(arg)))

adaswrite = adaswrite.$O adas_xxdata_11.$O elementread.$O reaction.$O interpolate.$O readfilenames.$O random.$O
adaswrite_libs = $(NETCDF) 
adaswrite: $(adaswrite) $(foreach arg,$(subst .$O,_mods,$(adaswrite)),$($(arg)))

efit2dg2d.o: bspline.mod
efit2dg2d = bspline.mod bspline90_22.o efit2dg2d.$O aux2dsetup.$O psiinterp.$O interpolate.$O
efit2dg2d_libs =
efit2dg2d: $(efit2dg2d) $(foreach arg,$(subst .$O,_mods,$(efit2dg2d)),$($(arg))) 

geomtest = geomtest.$O geometry.$O geomsetup.$O allocate.$O geomutils.$O geomfaces.$O composite.$O string.$O readfilenames.$O geomfront.$O dediagsetup.$O
geomtest_libs = $(NETCDF)
geomtest: $(geomtest) $(foreach arg,$(subst .$O,_mods,$(geomtest)),$($(arg)))

geomtesta = geomtesta.$O geometry.$O plotgeom.$O elementread.$O pmimatread.$O readfilenames.$O reaction.$O interpolate.$O collide.$O randother.$O random.$O conversions.$O voigt.$O degasinit.$O 
geomtesta_libs =  $(NETCDF) $(GRAPH_LIB)
geomtesta: $(geomtesta) $(foreach arg,$(subst .$O,_mods,$(geomtesta)),$($(arg)))

matchout = matchout.$O pmimatread.$O elementread.$O geometry.$O readfilenames.$O degasinit.$O
matchout_libs = $(NETCDF)
matchout: $(matchout) $(foreach arg,$(subst .$O,_mods,$(matchout)),$($(arg)))

matcheir = matcheir.$O pmimatread.$O elementread.$O geometry.$O readfilenames.$O degasinit.$O
matcheir_libs = $(NETCDF)
matcheir: $(matcheir) $(foreach arg,$(subst .$O,_mods,$(matcheir)),$($(arg)))

acc_output = acc_output.$O pmimatread.$O elementread.$O geometry.$O readfilenames.$O degasinit.$O conversions.$O
acc_output_libs = $(NETCDF)
acc_output: $(acc_output) $(foreach arg,$(subst .$O,_mods,$(acc_output)),$($(arg)))

reactionwrite = reactionwrite.$O
reactionwrite_libs = $(NETCDF)
reactionwrite: $(reactionwrite) $(foreach arg,$(subst .$O,_mods,$(reactionwrite)),$($(arg)))

pmiwrite = pmiwrite.$O interpolate.$O random.$O readfilenames.$O elementread.$O 
pmiwrite_libs = $(NETCDF)
pmiwrite: $(pmiwrite) $(foreach arg,$(subst .$O,_mods,$(pmiwrite)),$($(arg)))

datasetup = datasetup.$O elementread.$O pmimatread.$O elementsetup.$O speciesetup.$O reactionsetup.$O materialsetup.$O pmisetup.$O readfilenames.$O
datasetup_libs = $(NETCDF)
datasetup: $(datasetup) $(foreach arg,$(subst .$O,_mods,$(datasetup)),$($(arg)))

problemsetup = problemsetup.$O elementread.$O pmimatread.$O reaction.$O interpolate.$O readfilenames.$O plate.$O geometry.$O random.$O randother.$O
problemsetup_libs = $(NETCDF) 
problemsetup: $(problemsetup) $(foreach arg,$(subst .$O,_mods,$(problemsetup)),$($(arg)))

cramdtesta = cramdtesta.$O cramdadd.$O sysdep.$O string.$O pmimatread.$O
cramdtesta_libs = $(CRAMD) $(NETCDF)  
cramdtesta: $(cramdtesta) $(foreach arg,$(subst .$O,_mods,$(cramdtesta)),$($(arg)))

readgeometry = readgeometry.$O geomsetup.$O allocate.$O geomutils.$O geomfaces.$O geometry.$O composite.$O string.$O readdegas.$O readfilenames.$O elementread.$O pmimatread.$O dediagsetup.$O aux2dsetup.$O $(DETECTORFILE).$O
readgeometry_libs = $(NETCDF)
readgeometry: $(readgeometry) $(foreach arg,$(subst .$O,_mods,$(readgeometry)),$($(arg)))

definegeometry2d = $(DG2D) string.$O geomsetup.$O allocate.$O composite.$O geomutils.$O geomfaces.$O geometry.$O aux2dsetup.$O pmimatread.$O dediagsetup.$O readfilenames.$O plotgeom.$O $(DETECTORFILE).$O
definegeometry2d_libs = $(NETCDF) $(GRAPH_LIB) $(OPENMP)
definegeometry2d: $(definegeometry2d) $(foreach arg,$(subst .$O,_mods,$(definegeometry2d)),$($(arg)))

psiinterp.o: bspline.mod
defineback = defineback.$O string.$O writeback.$O pmimatread.$O geometry.$O elementread.$O geomsetup.$O aux2dsetup.$O allocate.$O geomutils.$O geomfaces.$O readfilenames.$O reaction.$O interpolate.$O random.$O composite.$O $(PLASMAFILE).$O psiinterp.$O bspline.mod bspline90_22.o 
defineback_libs = $(NETCDF)
defineback: $(defineback) $(foreach arg,$(subst .$O,_mods,$(defineback)),$($(arg)))

boxgen = boxgen.$O readfilenames.$O geomsetup.$O allocate.$O composite.$O geomutils.$O geomfaces.$O geometry.$O pmimatread.$O elementread.$O writeback.$O cramdadd.$O dediagsetup.$O reaction.$O interpolate.$O random.$O aux2dsetup.$O
boxgen_libs = $(CRAMD) $(NETCDF)
boxgen: $(boxgen) $(foreach arg,$(subst .$O,_mods,$(boxgen)),$($(arg)))

cramdproblem = cramdproblem.$O pmimatread.$O cramdadd.$O readfilenames.$O elementread.$O geometry.$O
cramdproblem_libs = $(CRAMD) $(NETCDF)
cramdproblem: $(cramdproblem) $(foreach arg,$(subst .$O,_mods,$(cramdproblem)),$($(arg)))

readfilenames=  readfilenames.$O readfilenames.$O sysdep.$O string.$O
readfilename_libs =
readfilenames: $(readfilenames) $(foreach arg,$(subst .$O,_mods,$(readfilenames)),$($(arg)))

readbackground = readbackground.$O readdegas.$O geometry.$O elementread.$O readfilenames.$O geomsetup.$O allocate.$O reaction.$O interpolate.$O composite.$O geomutils.$O geomfaces.$O geometry.$O writeback.$O pmimatread.$O cramdadd.$O random.$O aux2dsetup.$O
readbackground_libs = $(CRAMD) $(NETCDF)
readbackground: $(readbackground) $(foreach arg,$(subst .$O,_mods,$(readbackground)),$($(arg)))

updatebackground = updatebackground.$O elementread.$O readfilenames.$O geomsetup.$O allocate.$O writeback.$O pmimatread.$O random.$O geomutils.$O geometry.$O geomfaces.$O reaction.$O interpolate.$O
updatebackground_libs = $(CRAMD) $(NETCDF)
updatebackground: $(updatebackground) $(foreach arg,$(subst .$O,_mods,$(updatebackground)),$($(arg)))

reactiontest = reactiontest.$O elementread.$O reaction.$O interpolate.$O readfilenames.$O random.$O randother.$O collide.$O pmimatread.$O geometry.$O
reactiontest_libs = $(NETCDF)
reactiontest: $(reactiontest) $(foreach arg,$(subst .$O,_mods,$(reactiontest)),$($(arg)))

pmitest = pmitest.$O elementread.$O pmimatread.$O plate.$O readfilenames.$O random.$O randother.$O geometry.$O interpolate.$O
pmitest_libs = $(NETCDF)
pmitest: $(pmitest) $(foreach arg,$(subst .$O,_mods,$(pmitest)),$($(arg)))

sourcetest = sourcetest.$O sources.$O readfilenames.$O randother.$O random.$O pmimatread.$O elementread.$O geometry.$O cramdadd.$O interpolate.$O score.$O collide.$O reaction.$O conversions.$O
sourcetest_libs = $(CRAMD) $(NETCDF)
sourcetest: $(sourcetest) $(foreach arg,$(subst .$O,_mods,$(sourcetest)),$($(arg)))

voigttest = voigttest.$O voigt.$O
voigttest_libs = 
voigttest: $(voigttest) $(foreach arg,$(subst .$O,_mods,$(voigttest)),$($(arg)))

voigtspec = voigtspec.$O voigt.$O
voigtspec_libs = 
voigtspec: $(voigtspec) $(foreach arg,$(subst .$O,_mods,$(voigtspec)),$($(arg)))

ratecalc = ratecalc.$O aladdin_subr.o readfilenames.$O interpolate.$O
ratecalc_libs = $(NETCDF)
ratecalc: $(ratecalc) $(foreach arg,$(subst .$O,_mods,$(ratecalc)),$($(arg)))

dataexam = dataexam.$O readfilenames.$O elementread.$O
dataexam_libs = $(NETCDF)
dataexam: $(dataexam) $(foreach arg,$(subst .$O,_mods,$(dataexam)),$($(arg)))

outputbrowser = outputbrowser.$O readfilenames.$O degasinit.$O pmimatread.$O elementread.$O geometry.$O
outputbrowser_libs = $(NETCDF)
outputbrowser: $(outputbrowser) $(foreach arg,$(subst .$O,_mods,$(outputbrowser)),$($(arg)))

postdetector = postdetector.$O readfilenames.$O degasinit.$O pmimatread.$O elementread.$O geometry.$O dediagsetup.$O $(DETECTORFILE).$O
postdetector_libs = $(NETCDF) $(POST_LIB)
postdetector: $(postdetector) $(foreach arg,$(subst .$O,_mods,$(postdetector)),$($(arg)))

datamatch = datamatch.$O 
datamatch_libs = $(NETCDF)
datamatch: $(datamatch) $(foreach arg,$(subst .$O,_mods,$(datamatch)),$($(arg)))

tallysetup = tallysetup.$O elementread.$O pmimatread.$O readfilenames.$O string.$O geometry.$O
tallysetup_libs = $(NETCDF)
tallysetup: $(tallysetup) $(foreach arg,$(subst .$O,_mods,$(tallysetup)),$($(arg)))

allocate = allocate.$O
allocate_libs =
allocate: $(allocate) $(foreach arg,$(subst .$O,_mods,$(allocate)),$($(arg)))

array: $(SYSDEPO)
teststring: $(SYSDEPO) string.$O
elementlookup: $(SYSDEPO) string.$O
speciesetup: elementread.$O $(SYSDEPO) string.$O

netcdf: netcdf.$O
	$(FC) -o $@ $^ -lnetcdf

makemodule: makemodule.sh
	cp $< $@
	chmod a+x $@

makedepend: makedepend.sh
	cp $< $@
	chmod a+x $@

depend: makedepend
	./makedepend *.web *.hweb

aladdin_subr.f: $(ALADIR)/aladdin_subr.f
	cp $< $@

aladdin_subr.o: aladdin_subr.f ALPCOM.FOR ALCOM.FOR
	$(FC) -c $(FFLAGS) $(DEBUGFLAGS) aladdin_subr.f

%.FOR: $(ALADIR)/%.FOR
	cp $< $@

bspline90_22.f90: $(BSPLINEDIR)/bspline90_22.f90
	cp $< $@

bspline90_22.o: bspline90_22.f90
	$(FC) -c $(FFLAGS) $(DEBUGFLAGS) bspline90_22.f90

bspline.mod: bspline90_22.o

TAGS:
	etags --language=fortran --regex='/[ 	]*define_[a-z]+_pk([a-z]+,[A-Za-z0-9_]+,/' --regex='/[ 	]*@m [A-Za-z0-9_]+(?/' *.web *.hweb

.PHONY: clean %.view %.print TAGS depends

clean:
	rm -f *.o *.f *.cft *.dvi *.tex *.dvi *.ind *.mod *.con *.ps *.aux *.log *.l *.c.c *.view *.print *.tcov *.d *.trace *.blg *.aux0 *.dum *.M *.mweb *.T *.c *.h *.g90 *.ts


# DO NOT DELETE
