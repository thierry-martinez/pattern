DUNE := dune
DUNE_PREFIX := _build/default

TESTS_EXE := tests/tests.exe

# All targets are phony targets since we want to rely on dune for
# dependency management.

.PHONY : all
all :
	$(DUNE) build runtime/pattern_runtime.cmxa
	$(DUNE) build ppx/pattern.cmxa

.PHONY : clean
clean :
	dune clean

.PHONY : tests
tests :
	$(DUNE) build tests/tests.exe
	$(DUNE_PREFIX)/tests/tests.exe

.PHONY : install
install :
	$(DUNE) build @install
	$(DUNE) install

