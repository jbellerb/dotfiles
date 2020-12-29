.POSIX:

MODULES := xmonad

.PHONY: all
all: $(MODULES)

.include <${MODULES:%=%/module.mk}>

.PHONY: clean
clean: ${MODULES:%=clean_%}
