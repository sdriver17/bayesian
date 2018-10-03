# COMP 212 Homework 0:
#
# N. Danner

# Dependency lists.

TARGETS=tests driver
DEPS=hw8.sml hw8.cm driver.sml driver.cm tests.sml tests.cm unit_test.sml

# ##########
# It should not be necessary to modify anything below this line.
# ##########

# SML/NJ programs
SML_BIN=
SML=$(SML_BIN)sml
ML_BUILD=$(SML_BIN)ml-build
H2E=/usr/local/bin/heap2exec32

# Options and additional CM files for ml-build.
ML_BUILD_OPTS=-Ctdp.instrument=true
ML_BUILD_CMS=\$$smlnj-tdp/back-trace.cm

# Compute the heap suffix.
HEAP_SUFFIX=$(shell $(SML) @SMLsuffix)

tests : $(DEPS)
	$(ML_BUILD) $(ML_BUILD_OPS) $(ML_BUILD_CMS) tests.cm Tests.main tests
	$(H2E) tests.$(HEAP_SUFFIX) tests
	rm tests.$(HEAP_SUFFIX)

driver : $(DEPS)
	$(ML_BUILD) $(ML_BUILD_OPS) $(ML_BUILD_CMS) driver.cm Driver.main driver
	$(H2E) driver.$(HEAP_SUFFIX) driver
	rm driver.$(HEAP_SUFFIX)

# Cleanup targets.
clean :
	rm -rf .cm
	rm -f *.lex.sml *.grm.sml
	rm -f $(TARGETS)
	rm -f $(addsuffix .$(HEAP_SUFFIX), $(TARGETS))


