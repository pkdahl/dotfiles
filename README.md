dotfiles
========

# Install

To install some settings, e.g. git, then do
```bash
make git_install
```

To later remove the settings
``` bash
make git_clean
```

# Private settings

```bash
gpg -d private.tar.gpg -o private.tar
tar -xvf private.tar
rm private.tar
```
