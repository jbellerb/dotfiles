.POSIX:

XMONAD_FILES = build cabal.project xmonad-conf xmonad-bar

.PHONY: xmonad
xmonad: ${XMONAD_FILES:%=${HOME}/.config/xmonad/%}

.for FILE in ${XMONAD_FILES}
${HOME}/.config/xmonad/${FILE}: xmonad/${FILE}
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/xmonad/${FILE} ${@}
.endfor

.PHONY: clean_xmonad
clean_xmonad:
	rm -rf ${HOME}/.config/xmonad
