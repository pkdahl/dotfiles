. ../helpers.sh

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOT_NVIM="$CURRENT_DIR"
NVIM_CONFIG_DIR="$XDG_CONFIG_HOME/nvim"

if [ ! -L "$NVIM_CONFIG_DIR" ]; then
	echo_info "Linking $NVIM_CONFIG_DIR"
	ln -sf "$DOT_NVIM" "$NVIM_CONFIG_DIR"
fi

