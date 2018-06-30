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

## Install

```bash
gpg -d private.tar.gpg -o private.tar
tar -xvf private.tar
rm private.tar
```

## Add new private settings

Update git repository in `private/`

``` bash
tar -cvf private.tar private
gpg -o private.tar --symmetric private.tar
rm private.tar
```
