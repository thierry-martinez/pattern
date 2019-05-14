DUNE_PREFIX := _build/default
PATTERN_RUNTIME_CMXA := runtime/pattern_runtime.cmxa
PATTERN_CMXA := ppx/pattern.cmxa
TESTS_EXE := tests/tests.exe

.PHONY : all
all : $(DUNE_PREFIX)/$(PATTERN_CMXA).cmxa

.PHONY : clean
clean :
	dune clean

.PHONY : install
install : $(DUNE_PREFIX)/pattern.install
	dune install

$(DUNE_PREFIX)/$(PATTERN_RUNTIME_CMXA) : \
		 runtime/types.ml \
		 runtime/pattern_runtime.ml
	dune build $(PATTERN_RUNTIME_CMXA)

$(DUNE_PREFIX)/$(PATTERN_CMXA) : \
		$(DUNE_PREFIX)/$(PATTERN_RUNTIME_CMXA) \
		ppx/pattern.ml
	dune build $(PATTERN_CMXA)

.PHONY : tests
tests : $(DUNE_PREFIX)/$(TESTS_EXE)
	$(DUNE_PREFIX)/$(TESTS_EXE)

$(DUNE_PREFIX)/$(TESTS_EXE) : \
		tests/tests.ml \
		$(DUNE_PREFIX)/$(PATTERN_RUNTIME_CMXA) \
		$(DUNE_PREFIX)/$(PATTERN_CMXA)
	dune build $(TESTS_EXE)

$(DUNE_PREFIX)/pattern.install : \
		$(DUNE_PREFIX)/$(PATTERN_RUNTIME_CMXA) \
		$(DUNE_PREFIX)/$(PATTERN_CMXA) \
		pattern.opam
	dune build @install
