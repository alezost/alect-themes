# This is not a full-featured Makefile and it is not intended to be used
# to install "alect-themes" package to your system.  Its purposes are:
#
# - to byte-compile *.el files (using 'make'): to make sure that there
#   are no compilation warnings;
#
# - to run the tests (using 'make check').

EMACS = emacs

TOP := $(dir $(lastword $(MAKEFILE_LIST)))
LOAD_PATH = -L $(TOP)
EMACS_BATCH = $(EMACS) -batch -Q $(LOAD_PATH)

ELS =						\
  alect-themes.el				\
  alect-light-theme.el				\
  alect-light-alt-theme.el			\
  alect-dark-theme.el				\
  alect-dark-alt-theme.el			\
  alect-black-theme.el				\
  alect-black-alt-theme.el			\

ELCS = $(ELS:.el=.elc)

all: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --eval "\
	  (when (file-exists-p \"$@\")\
	    (delete-file \"$@\"))" \
	-f batch-byte-compile $<

check:
	@$(EMACS_BATCH) --eval "(progn\
	(load-file \"tests/alect-tests.el\")\
	(ert-run-tests-batch-and-exit))"

clean:
	@printf "Removing *.elc...\n"
	@$(RM) $(ELCS)
