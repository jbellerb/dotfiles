.POSIX:

XMONAD_FILES != find xmonad -type f ! -name "module.mk"

.PHONY: xmonad
xmonad: \
  ${XMONAD_FILES:xmonad/%=${HOME}/.xmonad/%} \
  ${HOME}/.xmonad/lib/Colors.hs

.for FILE in ${XMONAD_FILES:xmonad/%=%}
${HOME}/.xmonad/${FILE}: xmonad/${FILE}
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/xmonad/${FILE} ${@}
.endfor

${HOME}/.xmonad/lib/Colors.hs:
	@mkdir -p ${@D}
	ln -s ${HOME}/.cache/wal/colors.hs ${@}

.PHONY: clean_xmonad
clean_xmonad:
	rm -rf ${HOME}/.xmonad
