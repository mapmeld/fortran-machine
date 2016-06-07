# Makefile for "computing" tests and examples
#
# $Id: makefile,v 1.4 2008/09/28 11:03:11 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_diff$(EXEEXT) \
		test_decimal$(EXEEXT) \
		test_m_coordinates$(EXEEXT) \
		test_libdate$(EXEEXT) \
		test_intervals$(EXEEXT) \
		test_annealing$(EXEEXT) \
		test_genalg$(EXEEXT) \

all:	$(PROGRAMS)

test_diff$(OBJEXT):	test_diff.f90 automdiff$(OBJEXT) select_precision$(OBJEXT)
	$(FC) $(FFLAGS) test_diff.f90

automdiff$(OBJEXT):	../../src/computing/automdiff.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/automdiff.f90

select_precision$(OBJEXT):	../../src/computing/select_precision.f90
	$(FC) $(FFLAGS) ../../src/computing/select_precision.f90

test_diff$(EXEEXT):	test_diff$(OBJEXT) automdiff$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_diff$(OBJEXT) automdiff$(OBJEXT) select_precision$(OBJEXT)


test_decimal$(OBJEXT):	test_decimal.f90 decimal$(OBJEXT)
	$(FC) $(FFLAGS) test_decimal.f90

decimal$(OBJEXT):	../../src/computing/decimal.f90
	$(FC) $(FFLAGS) ../../src/computing/decimal.f90

test_decimal$(EXEEXT):	test_decimal$(OBJEXT) decimal$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_decimal$(OBJEXT) decimal$(OBJEXT)


test_m_coordinates$(OBJEXT):	test_m_coordinates.f90 m_coordinates$(OBJEXT)
	$(FC) $(FFLAGS) test_m_coordinates.f90

m_coordinates$(OBJEXT):	../../src/computing/m_coordinates.f90
	$(FC) $(FFLAGS) ../../src/computing/m_coordinates.f90

test_m_coordinates$(EXEEXT):	test_m_coordinates$(OBJEXT) m_coordinates$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_coordinates$(OBJEXT) m_coordinates$(OBJEXT)


libdate$(OBJEXT):	../../src/computing/libdate.f90
	$(FC) $(FFLAGS) ../../src/computing/libdate.f90

test_libdate$(OBJEXT):	test_libdate.f90 libdate$(OBJEXT)
	$(FC) $(FFLAGS) test_libdate.f90

test_libdate$(EXEEXT):	test_libdate$(OBJEXT) libdate$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_libdate$(OBJEXT) libdate$(OBJEXT)


interval_simple$(OBJEXT):	../../src/computing/interval_simple.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/interval_simple.f90

test_intervals$(OBJEXT):	test_intervals.f90 interval_simple$(OBJEXT)
	$(FC) $(FFLAGS) test_intervals.f90

test_intervals$(EXEEXT):	test_intervals$(OBJEXT) interval_simple$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_intervals$(OBJEXT) interval_simple$(OBJEXT)


annealing$(OBJEXT):	../../src/computing/annealing.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/annealing.f90

test_annealing$(OBJEXT):	test_annealing.f90 annealing$(OBJEXT)
	$(FC) $(FFLAGS) test_annealing.f90

test_annealing$(EXEEXT):	test_annealing$(OBJEXT) annealing$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_annealing$(OBJEXT) annealing$(OBJEXT)


genetic_algorithms$(OBJEXT):	../../src/computing/genetic_algorithms.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/genetic_algorithms.f90

test_genalg$(OBJEXT):	test_genalg.f90 genetic_algorithms$(OBJEXT)
	$(FC) $(FFLAGS) test_genalg.f90

test_genalg$(EXEEXT):	test_genalg$(OBJEXT) genetic_algorithms$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_genalg$(OBJEXT) genetic_algorithms$(OBJEXT)

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

