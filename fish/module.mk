.POSIX:

FENV_FUNCTIONS != find fish/plugins/fenv/functions -type f

.PHONY: fish
fish: \
  ${HOME}/.config/fish/config.fish \
  ${FENV_FUNCTIONS:fish/plugins/fenv/functions%=${HOME}/.config/fish/functions/%} \
  ${HOME}/.config/fish/completions/pass.fish

${HOME}/.config/fish/config.fish: fish/config.fish
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/fish/config.fish $@

.for FILE in ${FENV_FUNCTIONS:fish/plugins/fenv/functions/%=%}
${HOME}/.config/fish/functions/${FILE}: fish/plugins/fenv/functions/${FILE}
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/fish/plugins/fenv/functions/${FILE} $@
.endfor

${HOME}/.config/fish/completions/pass.fish: /usr/local/share/examples/password-store/completion/pass.fish-completion
	@mkdir -p ${@D}
	ln -s /usr/local/share/examples/password-store/completion/pass.fish-completion $@

.PHONY: clean_fish
clean_fish:
	rm -rf ${HOME}/.config/fish
