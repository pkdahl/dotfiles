dotfiles
========

# Install

Clone the repository, install the submodule(s) and install the symlinks.

```bash
git clone https://github.com/pkdahl/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
git submodule update --init --recursive
./install
```

# Update

```bash
git pull
git submodule update --init --recursive
./install
```

# Future

- [Dotbot brew plugin](https://github.com/d12frosted/dotbot-brew)
- [Multiple target](https://github.com/anishathalye/dotbot/pull/11#issuecomment-73082152)
