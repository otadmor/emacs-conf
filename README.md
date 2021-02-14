# emacs-conf
My personal Emacs configuration.
See also https://github.com/aviramc/.emacs.d and https://github.com/borfig/emacs-conf
## Installation
```shell
mv ~/.emacs.d{,.orig} || true
ln -s emacs.d ~/.emacs.d
cp emacsclient.desktop ~/.local/share/applications/
chmod +x  ~/.local/share/applications/emacsclient.desktop
```
### gdb completion
Put the content of .gdbinit in ~/.gdbinit.
## Keys
### Global Keys
- *Ctrl-x c*: Leave emacs (the default Ctrl-x Ctrl-c is disabled on purpose)
- *M-d*: Focus in/out minibuffer
- *Ctrl-Alt-k*: Kill current buffer
- *Ctrl-Tab*: Switch to other window (or Ctrl-x arrow)
- *Ctrl-Shift-t*: New shell in current folder
- *Meta-h*: Magit
- *Meta-r*: Switch search method in swiper (ivy/regexp/fixed)
- *Meta-c*: Restore last minibuffer interaction
- *Meta-1 to Meta-9*: Switch to other perspective (on emacs client only)
### Editing
- *Ctrl-z*: Undo
- *Meta-f*: Grep in current project
- *Ctrl-x p*: Search file in project (current repository)
- *Ctrl-x a*: Locate file
- *Meta-.*: Goto Definition
- *Ctrl-Meta-p*: Previous location in file
- *Ctrl-Meta-n*: Next location in file
- *Ctrl-/*: Comment current line / region
- *Ctrl-k*: Delete whole line
- *Ctrl->*: Multiple-Cursors mark next
- *Ctrl-<*: Multiple-Cursors mark previous
- *Ctrl-"*: Multiple-Cursors mark all
- *Ctrl-l*: Truncate lines on/off
### Shell Keys
- *Ctrl-up*: Previous history
- *Ctrl-down*: Next history
- *Meta-r*: Bash history
- *Shift-return*: Insert new-lines in the shell
- *Tab*: Expand / Shrink long lines
## Emacs compilation
A wild tale tells lucid works better than gtk3 when using emacs daemon. These are the suggested compilation flags:
```
sudo apt-get install build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libncurses5-dev libxaw7-dev libgnutls28-dev libxft-dev libharfbuzz-dev libxfixes-dev libxrandr-dev libxinerama-dev ttf-mscorefonts-installer npm libvterm-bin libvterm-dev libvterm0 libtool libtool-bin cmake libsystemd-dev libsystemd0 libotf-dev
wget https://ftp.gnu.org/gnu/emacs/emacs-27.1.tar.xz
tar -xf emacs-27.1.tar.xz
cd emacs-27.1
git apply xterm.c.27.1.patch
./configure --with-x-toolkit=lucid --with-kerberos --with-kerberos5 --with-wide-int --with-mailutils --with-modules --with-libotf --with-libsystemd --enable-checking=yes,glyphs --with-file-notification=yes 'CFLAGS=-O2'
make
sudo make install
systemctl --user enable emacs
sudo npm -g install js-beautify

git clone https://github.com/akermu/emacs-libvterm.git
cd emacs-libvterm
mkdir -p build
cd build
cmake ..
make
cd ..
cp vterm-module.so ~/.emacs.d/lisp/vterm-module.so
cp vterm.el ~/.emacs.d/lisp/vterm.el
cp etc/emacs-vterm-bash.sh ~/.emacs.d/lisp/emacs-vterm-bash.sh
```
## Bash epc completion
```
wget https://ftp.gnu.org/gnu/bash/bash-5.1.tar.gz
tar -xf bash-5.1.tar.gz
cd bash-5.1
git apply bash_epc_completion.patch
./configure
make pathnames.h
cd examples/loadables
make epc_completion
cp epc_completion ~/.emacs.d/lisp/bash_epc_completion
# enable -f ./epc_completion epc_completion
# enable -d epc_completion
```

## GDB completion / context
The context command is provided by GEF and then sent to emacs using epc.
![](gdb.gif)
## Python completion
Support following python and completion from python interpreter.
![](python.gif)
## GDB / IDA decompiler integration
Have your ida on one machine with epc server running:
```
cd sysroot-of-binaries
python -m epc.server --address 0.0.0.0 --port 19999 builtins
```
The have your gdb init configure this machine ip:
```
gef config ida-interact.host "192.168.99.1"
```
And then you will be able to see one window with the decompiled code from ida and for each decompiled code you can see its assembly (press TAB on a source code line to see the assembly).
![](ida-source.gif)
