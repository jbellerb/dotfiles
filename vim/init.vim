if empty($XDG_CACHE_HOME) | let $XDG_CACHE_HOME = $HOME.'/.cache' | endif
if empty($XDG_CONFIG_HOME) | let $XDG_CONFIG_HOME = $HOME.'/.config' | endif
if empty($XDG_DATA_HOME) | let $XDG_DATA_HOME = $HOME.'/.local/share' | endif

set backupdir=$XDG_CACHE_HOME/vim/backup | call mkdir(&backupdir, 'p')
set directory=$XDG_CACHE_HOME/vim/swap | call mkdir(&directory, 'p')
set undodir=$XDG_CACHE_HOME/vim/undo | call mkdir(&undodir, 'p')

set viminfofile=$XDG_CACHE_HOME/vim/viminfo

set runtimepath^=$XDG_CONFIG_HOME/vim
set runtimepath+=$XDG_DATA_HOME/vim
set runtimepath+=$XDG_CONFIG_HOME/vim/after

set packpath^=$XDG_CONFIG_HOME/vim
set packpath+=$XDG_DATA_HOME/vim
set packpath+=$XDG_CONFIG_HOME/vim/after

source $XDG_CONFIG_HOME/vim/vimrc
