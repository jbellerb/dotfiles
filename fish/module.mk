.POSIX:

.PHONY: fish
fish: ${HOME}/.config/fish/config.fish

${HOME}/.config/fish/config.fish: fish/config.fish
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/fish/config.fish ${@}

.PHONY: clean_fish
clean_fish:
	rm -rf ${HOME}/.config/fish
