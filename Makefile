# List of day directories
DAYS := $(addprefix day_, $(shell seq 1 12))

# Each directory has puzzles.ml; executables will be named "puzzles"
APPS := $(DAYS:%=%/puzzles)

.PHONY: all clean $(DAYS)

all: $(APPS)

# Compile each puzzles.ml into its directory
%/puzzles: %/puzzles.ml
	@echo "Compiling $<..."
	ocamlopt -o $@ $<

# Allow "make day_5" to build only that day's executable
$(DAYS):
	$(MAKE) $@/puzzles

clean:
	@echo "Cleaning..."
	for d in $(DAYS); do \
		rm -f $$d/*.o $$d/*.cmi $$d/*.cmx $$d/puzzles; \
	done
