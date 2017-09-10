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

# Private settings

`gpg -d private.tar.gpg -o private.tar`
`tar -xvf private.tar`
`rm private.tar`
