. ../helpers.sh

# TODO: install pynvim neovim-remote automagically

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOT_NVIM="$CURRENT_DIR"
NVIM_CONFIG_DIR="$XDG_CONFIG_HOME/nvim"

if [ ! -L "$NVIM_CONFIG_DIR/init.vim" ]; then
	echo_info "Linking init.vim"
    mkdir -p "$NVIM_CONFIG_DIR"
	ln -sf "$DOT_NVIM/init.vim" "$NVIM_CONFIG_DIR/init.vim"
fi

