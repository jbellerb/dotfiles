.POSIX:

MODULES := dunst fish pywal vim xmonad

.PHONY: all
all: $(MODULES)

.for MODULE in ${MODULES}
.include <${MODULE}/module.mk>
.endfor

.PHONY: clean
clean: ${MODULES:%=clean_%}
