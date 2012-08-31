# Compile TheSky software package
# g95 can't compile statically yet...

.PHONY: clean cleanwarnings code_version exit
.PRECIOUS: %.o



#No text/space after statements below!
FORT = gfortran#  Use GNU Fortran compiler.  Need >= 4.5
#FORT = g95#       USE g95 compiler
#FORT = ifort#     Use Intel Fortran compiler
#FORT = nagfor#     Use NAG Fortran compiler

WARN = yes#    # compile with compile-time warnings
CHECK = yes#   # compile with run-time checks
#DEBUG = yes#   # compile with debug options
F2003 = yes#   # compile with Fortran 2003 standard
#OPENMP = yes#  # compile with OpenMP support


#General:
#OPT = -O0
#OPT = -O1
OPT = -O2
#OPT = -O3
CFLAGS =  #-I/home/sluys/usr/include
LFLAGS = #-L/home/sluys/usr/lib 
GLFLAGS = -lpgplot
STATLIBS = #/home/sluys/usr/lib/libSUFR.a
GSTATLIBS = ~/usr/lib/libpgplot-ppmonly.a # Static libraries for programs with graphical output

# Directories for the source, object, module, libary and executable files:
SRCDIR = src
OBJDIR = obj
MODDIR = mod
LIBDIR = lib
#BINDIR = bin
BINDIR = .

# Standard objects:
#STDOBJ  = $(OBJDIR)/modules.o
STDOBJ  = $(OBJDIR)/code_version.o $(OBJDIR)/functions.o
STDLOBJ = $(STDOBJ)




#gfortran: similar in speed to ifort, but file reading is ~2.5x slower!
#ifeq ($(FORT),gfortran)
ifneq (,$(filter $(FORT),gfortran gfortran-4.6))
   CFLAGS += -I/home/sluys/usr/include/libSUFR/gfortran
   LFLAGS += -lSUFR_gfortran
   STATLIBS += /home/sluys/usr/lib/libSUFR_gfortran.a

   CFLAGS += -fwhole-file #-fwhole-program
   #LFLAGS += -Wl,-rpath=/usr/lib/gcc/x86_64-pc-linux-gnu/4.6.0-pre9999/ #for 4.6

   ifeq ($(CHECK),yes)
      OPT = -O0
      CFLAGS += -fcheck=all
      CFLAGS += -fbacktrace
   endif

   ifeq ($(WARN),yes)
      CFLAGS += -Wall -Wextra
      CFLAGS += -Wcharacter-truncation -Wunderflow
#      CFLAGS += -Warray-temporaries
#      CFLAGS += -Wconversion
#      CFLAGS += -Wimplicit-interface
      CFLAGS += -fmax-errors=10
# Use implicit none everywhere:
#      CFLAGS += -fimplicit-none
# Turn warnings into errors:
      CFLAGS += -Werror
   endif

   #To debug code:
   ifeq ($(DEBUG),yes)
      OPT = -O0
      CFLAGS += -g
   endif

   ifeq ($(F2003),yes)
      CFLAGS += -std=f2008 -fall-intrinsics -pedantic
   else
      CFLAGS += -ffree-line-length-none
   endif

   ifeq ($(OPENMP),yes)
      CFLAGS += -fopenmp
      LFLAGS += -lgomp
   endif

   CFLAGS += -I$(MODDIR) -J$(MODDIR)
   STATIC = -static
endif


#g95:
ifeq ($(FORT),g95)
   CFLAGS += -I/home/sluys/usr/include/libSUFR/g95
#   LFLAGS += -lgcc_s -lSUFR_g95
#   STATLIBS += /home/sluys/usr/lib/libSUFR_g95.a /usr/lib64/gcc/x86_64-pc-linux-gnu/4.5.2/libgcc_eh.a
   LFLAGS += -lSUFR_g95 -L/opt/g95-0.93-x86_64/lib/gcc-lib/x86_64-unknown-linux-gnu/4.0.3/ -lgcc_s 
#   STATLIBS += /opt/g95-0.93-x86_64/lib/gcc-lib/x86_64-unknown-linux-gnu/4.0.3/libgcc_eh.a
   STATLIBS += ~/usr/lib/libSUFR_g95.a
   GSTATLIBS += ~/usr/lib/libgfortran.a

   ifeq ($(CHECK),yes)
      OPT = -O0
      CFLAGS += -fbounds-check
      CFLAGS += -ftrace=full
   endif

   ifeq ($(WARN),yes)
      CFLAGS += -Wall -Wextra 
      CFLAGS += -Wno=102,112,136,165
      CFLAGS += -Wno=163  # Actual argument does not have an INTENT
      CFLAGS += -Wno=140  # Implicit conversion at (1) may cause precision loss
# Turn warnings into errors:
      CFLAGS += -Werror
   endif

   #To debug code:
   ifeq ($(DEBUG),yes)
      OPT = -O0
      CFLAGS += -g
   endif

   ifeq ($(F2003),yes)
      CFLAGS += -std=f2003 
#   else
#      CFLAGS += 
   endif

   ifeq ($(OPENMP),yes)
      #CFLAGS += -fopenmp
      #LFLAGS += -lgomp
   endif

   CFLAGS += -I$(MODDIR) -fmod=$(MODDIR)
   STDOBJ += $(OBJDIR)/nagfor.o
   STATIC = -static
endif


#ifort:
ifeq ($(FORT),ifort)
   CFLAGS += -I/home/sluys/usr/include/libSUFR/ifort
   LFLAGS += -lSUFR_ifort
   STATLIBS += /home/sluys/usr/lib/libSUFR_ifort.a
   GSTATLIBS += ~/usr/lib/libgfortran.a

   #To develop code  (don't mix -check bounds with -O>0!):
   ifeq ($(CHECK),yes)
      OPT = -O0
      CFLAGS += -check -check noarg_temp_created
   endif

   ifeq ($(WARN),yes)
      CFLAGS += -warn -warn errors -WB
   endif

   ifeq ($(F2003),yes)
      CFLAGS += -stand -diag-disable 6894,8290
   endif

   ifeq ($(OPENMP),yes)
      CFLAGS += -openmp
      LFLAGS += -openmp
   endif

   CFLAGS += -vec-report0 -traceback -error-limit 10
   CFLAGS += -module $(MODDIR) -I$(MODDIR)

   #To debug code:
   ifeq ($(DEBUG),yes)
      OPT = -O0
      CFLAGS += -g
   endif


   CFLAGS += -nogen-interfaces
   STATIC = -static

   #OPT += -ipo
   #LFLAGS += -ipo
   #OPT += -xsse4.1
   #OPT += -xhost
   #CFLAGS += -static $(STATLIBS)
   #CFLAGS += -prof-gen -prof-dir./  #Don't use -ipo. Run the code thus compiled (a few times) and use -prof-use below for the final version
   #CFLAGS += -prof-use -prof-dir./

endif


#nagfor:
ifeq ($(FORT),nagfor)
   ifeq ($(CHECK),yes)
      CFLAGS = -O0
      CFLAGS += -C=all
   endif
   ifneq ($(WARN),yes)
      CFLAGS += -w=all  #Warnings are on by default
   endif
   CFLAGS += -colour -f2003 -u -gline
   STDOBJ += $(OBJDIR)/nagfor.o
   CFLAGS += -I$(MODDIR) -mdir $(MODDIR)
   STATIC = -Bstatic
endif





#CFLAGS += -fPIC


GLFLAGS += $(LFLAGS)





all: rect2spher

exit:


$(OBJDIR)/%.o: $(SRCDIR)/%.f90   # $(SRCDIR)/modules.f90
	$(FORT) $(OPT) $(CFLAGS) -c $< -o $@



#modules: $(SRCDIR)/modules.f90
#	$(FORT) $(OPT) $(CFLAGS) -c $(SRCDIR)/modules.f90  -o $(OBJDIR)/modules.o




ddd2dms: $(STDOBJ) $(OBJDIR)/ddd2dms.o
	$(FORT) $(GLFLAGS) -o $(BINDIR)/ddd2dms $(STDLOBJ) $(OBJDIR)/ddd2dms.o

rect2spher: $(STDOBJ) $(OBJDIR)/rect2spher.o
	$(FORT) $(GLFLAGS) -o $(BINDIR)/rect2spher $(STDLOBJ) $(OBJDIR)/rect2spher.o





$(SRCDIR)/code_version.f90: code_version    #code_version is PHONY and always out-of-date
	@echo 'Generating '$(SRCDIR)'/code_version.f90'
	@echo '!> \file code_version.f90  Source file automatically generated by Makefile to report the code version used.' > $(SRCDIR)/code_version.f90
	@echo 'subroutine print_code_version(unit)' >> $(SRCDIR)/code_version.f90
	@echo '   implicit none' >> $(SRCDIR)/code_version.f90
	@echo '   integer, intent(in) :: unit' >> $(SRCDIR)/code_version.f90
	@echo "   character :: svn_revision*(99) = '"`svnversion`"'" >> $(SRCDIR)/code_version.f90
	@echo "   character :: compile_date*(99) = '"`date`"'" >> $(SRCDIR)/code_version.f90
	@echo "   character :: compiler*(99) = '$(FORT)'" >> $(SRCDIR)/code_version.f90
	@echo "   character :: compiler_flags*(99) = '$(OPT)'" >> $(SRCDIR)/code_version.f90
	@echo "   write(unit,'(A)', advance='no')'svn revision '//trim(svn_revision)//', compiled on '//trim(compile_date)// &" >> $(SRCDIR)/code_version.f90
	@echo "        ' with '//trim(compiler)//' '//trim(compiler_flags)" >> $(SRCDIR)/code_version.f90
	@echo 'end subroutine print_code_version' >> $(SRCDIR)/code_version.f90
#	@$(FORT) $(OPT) $(CFLAGS) -c $(SRCDIR)/code_version.f90 -o $(OBJDIR)/code_version.o
#	@touch -d "2 Jan 2001" $(SRCDIR)/code_version.f90 # Make it old, so that otherwise unchanged programs are not relinked
#	@touch -d "1 Jan 2001" $(OBJDIR)/code_version.o   # Make it old, so that otherwise unchanged programs are not relinked




clean:
	rm -f $(OBJDIR)/*.o $(MODDIR)/*.mod $(SRCDIR)/code_version.f90

cleanoptimisation:
	rm -f ????????_?????.dyn pgopti.dpi pgopti.dpi.lock

cleanwarnings:
	rm -f *__genmod.f90 *__genmod.mod

cleanall:  clean cleanoptimisation cleanwarnings




#Mac:
#LFLAGS = -lpgplot_ifort -L/usr/X11R6/lib -lX11 -lm -L/opt/local/lib -Wl,-framework -Wl,Foundation -lifcore -limf -lirc -lsvml -lifport -Xlinker -multiply_defined suppress -bind_at_load

#FORT = lf95
#CFLAGS = --wide -O
#CFLAGS = --info --wide -O
#CFLAGS = --chkglobal --wide -O
#CFLAGS = -g --wide
#LFLAGS = -lpgplot -lX11

