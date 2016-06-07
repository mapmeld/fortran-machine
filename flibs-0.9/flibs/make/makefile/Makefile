PROJECTDIR = ../..
srcdir = $(PROJECTDIR)/src

#
# General configuration
#
include Makefile-cfg
#
# Objects
#
include Makefile-flibs-objects

ifeq ($(FC),g95)
	F90OPTS += -I$(srcdir)/strings
endif

#
# Name of the binaries
#
libdir = $(PROJECTDIR)/lib/$(OS)/$(ARCH)/$(FC)/$(VERSION)
bindir = $(PROJECTDIR)/bin/$(OS)/$(ARCH)/$(FC)/$(VERSION)
testdir = $(PROJECTDIR)/tests
EXE_TEST_FILSYS = $(bindir)/test_m_filedir.exe
EXE_TEST_PLATFORM = $(bindir)/test_m_platform.exe
EXE_TEST_VSTRING = $(bindir)/test_m_vstring.exe
EXE_TEST_VSTRINGLIST = $(bindir)/test_m_vstringlist.exe
EXE_TEST_VFILE = $(bindir)/test_m_vfile.exe
EXE_TEST_LOGGER = $(bindir)/test_m_logger.exe
#
# List of the archive files
#
archive_flibs = $(PROJECTDIR)/bin/$(OS)/$(ARCH)/$(FC)/$(VERSION)/flibs.a

all: $(archive) tests
	$(ECHO) "Target all OK."

archive: $(libdir) $(bindir) $(archive_flibs) 

tests: $(libdir) $(bindir) $(archive_flibs) \
$(EXE_TEST_FILSYS) \
$(EXE_TEST_PLATFORM) \
$(EXE_TEST_VSTRING) \
$(EXE_TEST_VSTRINGLIST) \
$(EXE_TEST_VFILE) \
$(EXE_TEST_LOGGER)

$(libdir): 
	$(MKDIR) -p $(libdir)
	
$(bindir):
	$(MKDIR) -p $(bindir)

debug: 
	@echo "Project dir : $(PROJECTDIR)"
	@echo "libdir:$(libdir)"
	@echo "bindir:$(bindir)"
	@echo "FC : $(FC)"
	@echo "F90OPTS : $(F90OPTS)"
	@echo "F77OPTS : $(F77OPTS)"
	@echo "FOPTS_BASE : $(FOPTS_BASE)"
	@echo "archive_flibs:$(archive_flibs)"
	@echo "EXE_TEST_FILSYS:$(EXE_TEST_FILSYS)"
	@echo "EXE_TEST_PLATFORM:$(EXE_TEST_PLATFORM)"
	@echo "srcdir:$(srcdir)"
	@echo "testdir:$(testdir)"


$(archive_flibs): $(objects)

$(EXE_TEST_FILSYS): $(libdir)/test_m_filedir.o $(objects)
	$(ECHO) "Linking : $(shell basename $@)"
	$(LINK) $(LOPTS) -o $@ $< $(archive_flibs)

$(EXE_TEST_PLATFORM): $(libdir)/test_m_platform.o $(objects)
	$(ECHO) "Linking : $(shell basename $@)"
	$(LINK) $(LOPTS) -o $@ $< $(archive_flibs)

$(EXE_TEST_VSTRING): $(libdir)/test_m_vstring.o $(objects)
	$(ECHO) "Linking : $(shell basename $@)"
	$(LINK) $(LOPTS) -o $@ $< $(archive_flibs)

$(EXE_TEST_VSTRINGLIST): $(libdir)/test_m_vstringlist.o $(objects)
	$(ECHO) "Linking : $(shell basename $@)"
	$(LINK) $(LOPTS) -o $@ $< $(archive_flibs)

$(EXE_TEST_VFILE): $(libdir)/test_m_vfile.o $(objects)
	$(ECHO) "Linking : $(shell basename $@)"
	$(LINK) $(LOPTS) -o $@ $< $(archive_flibs)

$(EXE_TEST_LOGGER): $(libdir)/test_m_logger.o $(objects)
	$(ECHO) "Linking : $(shell basename $@)"
	$(LINK) $(LOPTS) -o $@ $< $(archive_flibs)

#
# Deletes all generated files
#
clean:
	$(REMOVE) --force $(libdir)/*
	$(REMOVE) --force $(bindir)/*
	
cleanall: clean 
	

#
# Rules for the project
#
#
# Rules for sources
#

$(libdir)/%.o : $(srcdir)/computing/%.f90
	$(FC) $(F90OPTS) -c $< -o $@
	$(AR) cr $(archive_flibs) $@

$(libdir)/%.o : $(srcdir)/controlstructures/%.f90
	$(FC) $(F90OPTS) -c $< -o $@
	$(AR) cr $(archive_flibs) $@

$(libdir)/%.o : $(srcdir)/datastructures/%.f90
	$(FC) $(F90OPTS) -c $< -o $@
	$(AR) cr $(archive_flibs) $@

$(libdir)/%.o : $(srcdir)/filedir/%.f90
	$(FC) $(F90OPTS) -c $< -o $@
	$(AR) cr $(archive_flibs) $@

$(libdir)/%.o : $(srcdir)/funit/%.f90
	$(FC) $(F90OPTS) -c $< -o $@
	$(AR) cr $(archive_flibs) $@

$(libdir)/%.o : $(srcdir)/sqlite/%.f90
	$(FC) $(F90OPTS) -c $< -o $@
	$(AR) cr $(archive_flibs) $@

$(libdir)/%.o : $(srcdir)/streams/%.f90
	$(FC) $(F90OPTS) -c $< -o $@
	$(AR) cr $(archive_flibs) $@

$(libdir)/%.o : $(srcdir)/strings/%.f90
	$(FC) $(F90OPTS) -c $< -o $@
	$(AR) cr $(archive_flibs) $@

$(libdir)/%.o : $(srcdir)/platform/%.f90
	$(FC) $(F90OPTS) -c $< -o $@
	$(AR) cr $(archive_flibs) $@

$(libdir)/%.o : $(srcdir)/reporting/%.f90
	$(FC) $(F90OPTS) -c $< -o $@
	$(AR) cr $(archive_flibs) $@
#
# Rules for tests
#
$(libdir)/%.o : $(testdir)/computing/%.f90
	$(FC) $(F90OPTS) -c $< -o $@

$(libdir)/%.o : $(testdir)/datastructures/%.f90
	$(FC) $(F90OPTS) -c $< -o $@

$(libdir)/%.o : $(testdir)/filedir/%.f90
	$(FC) $(F90OPTS) -c $< -o $@

$(libdir)/%.o : $(testdir)/funit/%.f90
	$(FC) $(F90OPTS) -c $< -o $@

$(libdir)/%.o : $(testdir)/sqlite/%.f90
	$(FC) $(F90OPTS) -c $< -o $@

$(libdir)/%.o : $(testdir)/streams/%.f90
	$(FC) $(F90OPTS) -c $< -o $@

$(libdir)/%.o : $(testdir)/strings/%.f90
	$(FC) $(F90OPTS) -c $< -o $@

$(libdir)/%.o : $(testdir)/platform/%.f90
	$(FC) $(F90OPTS) -c $< -o $@

$(libdir)/%.o : $(testdir)/reporting/%.f90
	$(FC) $(F90OPTS) -c $< -o $@

include Makefile-flibs-dependencies

