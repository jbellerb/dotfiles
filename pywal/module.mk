.POSIX:

.PHONY: pywal
pywal: \
  ${HOME}/.config/wal/colorschemes \
  ${HOME}/.config/wal/templates

${HOME}/.config/wal/colorschemes: pywal/colorschemes
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/pywal/colorschemes $@

${HOME}/.config/wal/templates: pywal/templates
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/pywal/templates $@

.PHONY: clean_pywal
clean_pywal:
	rm -rf ${HOME}/.config/wal
