.POSIX:

VIM_PACKAGES := everforest lightline nord

.PHONY: vim
vim: \
  ${HOME}/.config/vim/vimrc \
  ${HOME}/.config/vim/init.vim \
  ${VIM_PACKAGES:%=${HOME}/.config/vim/pack/plugins/start/%}

${HOME}/.config/vim/vimrc: vim/vimrc
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/vim/vimrc ${@}

${HOME}/.config/vim/init.vim: vim/init.vim
	@mkdir -p ${@D}
	ln -s ${.CURDIR}/vim/init.vim ${@}

.for PACKAGE in ${VIM_PACKAGES}
${HOME}/.config/vim/pack/plugins/start/${PACKAGE}: vim/packages/${PACKAGE}
	@mkdir -p ${@}
	ln -s ${.CURDIR}/vim/packages/${PACKAGE} ${@}
.endfor

.PHONY: clean_vim
clean_vim:
	rm -rf ${HOME}/.config/vim
