.POSIX:

.PHONY: vim
vim: \
  ${HOME}/.config/vim/vimrc \
  ${HOME}/.config/vim/init.vim \
  ${HOME}/.config/vim/pack/plugins/start

${HOME}/.config/vim/vimrc: vim/vimrc
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/vim/vimrc ${@}

${HOME}/.config/vim/init.vim: vim/init.vim
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/vim/init.vim ${@}

${HOME}/.config/vim/pack/plugins/start:
	@mkdir -p ${@}
	ln -s ${.CURDIR}/vim/packages/* ${@}/

.PHONY: clean_vim
clean_vim:
	rm -rf ${HOME}/.config/vim
