set fish_greeting

# source ~/.cache/wal/colors.fish

set fish_color_autosuggestion 4c566a
set fish_color_command 81a1c1
set fish_color_comment 434c5e
set fish_color_end 88c0d0
set fish_color_error ebcb8b
set fish_color_param eceff4
set fish_color_quote a3be8c
set fish_color_redirection b48ead

function fish_user_key_bindings
  fish_vi_key_bindings
end

set -x XDG_CACHE_HOME $HOME/.cache
set -x XDG_CONFIG_HOME $HOME/.config
set -x XDG_DATA_HOME $HOME/.local/share

set -x VIMINIT 'source $XDG_CONFIG_HOME/vim/init.vim'
