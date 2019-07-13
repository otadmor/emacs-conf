# emacs-conf
My personal Emacs configuration
See also https://github.com/aviramc/.emacs.d
## Installation
```shell
mv ~/.emacs.d{,.orig} || true
ln -s emacs.d ~/.emacs.d
```
### gdb completion
Put the content of .emacs.d/lisp/completion_epc.gdbinit in ~/.gdbinit.
## Keys
### Global Keys
- *Ctrl-x c*: leave emacs (the default Ctrl-x Ctrl-c is disabled on purpose)
- *Ctrl-Alt-k*: kill current buffer (or Ctrl-Alt-k)
- *Ctrl-Tab*: switch to other window (or Alt-<arrow>)
- *Ctrl-z*: undo last edit
- *Ctrl-y*: redo last edit
- *f5*: maximize window to frame
## Emacs compilation
A wild tale tells lucid works better than gtk3 when using emacs daemon. These are the suggested compilation flags:
```
wget ftp://ftp.gnu.org/gnu/emacs/emacs-26.2.tar.xz
tar -xf emacs-26.2.tar.xz
cd emacs-26.2
./configure --with-x-toolkit=lucid --with-kerberos --with-kerberos5 --with-wide-int --with-mailutils
git apply xterm.c.patch
make
sudo make install
systemctl --user enable emacs
```
