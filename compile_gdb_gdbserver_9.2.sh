#!/bin/bash
# sudo apt-get install guile-2.2 guile-2.2-dev guile-2.2-libs
# GDB_VERSION=10.1
GDB_VERSION=9.2
PYTHON_VERSION=3.8
# GUILE_VERSION=3.0
GUILE_VERSION=2.0
sudo apt-get install libipt-dev gcc-arm-linux-gnueabi g++-arm-linux-gnueabi gcc-multilib guile-$GUILE_VERSION guile-$GUILE_VERSION-dev guile-$GUILE_VERSION-libs python$PYTHON_VERSION-dev
# to check --enable-gdbtk --enable-tui, --enable-gtk
rm gdb-$GDB_VERSION.tar gdb-$GDB_VERSION.tar.xz
wget https://ftp.gnu.org/gnu/gdb/gdb-$GDB_VERSION.tar.xz
# wget ftp://sourceware.org/pub/gdb/releases/gdb-$GDB_VERSION.tar.xz
xz -dk gdb-$GDB_VERSION.tar.xz
tar -xf gdb-$GDB_VERSION.tar
gdb_prefix=/opt/gdb

pushd gdb-$GDB_VERSION

mkdir build-multiarch
pushd build-multiarch
../configure --enable-tui --with-guile=guile-$GUILE_VERSION --with-mpfr --with-expat --with-zlib --with-lzma --with-python=python$PYTHON_VERSION --prefix=$gdb_prefix --enable-targets=all || exit 1
make || exit 1
sudo make install || exit 1
popd


GDB=$(which gdb)
update-alternatives --query gdb
if [[ "$?" != "0" ]] ; then
    if [[ "$GDB" != "" ]]; then
        GDB_NEW_NAME=$(dirname $GDB)/gdb-$(gdb --version | sed -rn 's/.*gdb.*\) (.*?)/\1/p')
        if [[ ! -f $GDB_NEW_NAME ]]; then
            sudo mv $GDB $GDB_NEW_NAME
        fi
        sudo update-alternatives --install $GDB gdb $GDB_NEW_NAME 10
    fi
fi
if [[ "$GDB" == "" ]]; then
    GDB=/usr/local/bin/gdb
fi
sudo update-alternatives --install $GDB gdb $gdb_prefix/bin/gdb 20


mkdir build-gdbserver-arm-linux-gnueabi
pushd build-gdbserver-arm-linux-gnueabi
../gdb/gdbserver/configure LDFLAGS=-static --host=arm-linux-gnueabi || exit 1
make || exit 1
popd


NDK=/opt/android-ndk-r21d

mkdir build-gdbserver-aarch64-linux-android
pushd build-gdbserver-aarch64-linux-android
PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH ../gdb/gdbserver/configure --host=aarch64-linux-android --program-prefix=aarch64-linux-android- LDFLAGS="-static" CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ || exit 1
sed -i 's/HAVE_MKOSTEMP = 1/HAVE_MKOSTEMP = 0/g' build-gnulib-gdbserver/import/Makefile || exit 1
sed -i 's/HAVE_STRCHRNUL = 1/HAVE_STRCHRNUL = 0/g' build-gnulib-gdbserver/import/Makefile || exit 1
sed -i 's/-Wl,--dynamic-list=$(srcdir)\/proc-service.list//g' Makefile || exit 1
PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH make gdbserver || exit 1
PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH aarch64-linux-android-strip gdbserver || exit 1
popd


mkdir build-gdbserver-armv7a-linux-androideabi
pushd build-gdbserver-armv7a-linux-androideabi
PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH ../gdb/gdbserver/configure --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- LDFLAGS="-static" CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ || exit 1
sed -i 's/HAVE_MKOSTEMP = 1/HAVE_MKOSTEMP = 0/g' build-gnulib-gdbserver/import/Makefile || exit 1
sed -i 's/HAVE_STRCHRNUL = 1/HAVE_STRCHRNUL = 0/g' build-gnulib-gdbserver/import/Makefile || exit 1
sed -i 's/-Wl,--dynamic-list=$(srcdir)\/proc-service.list//g' Makefile || exit 1
PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH make gdbserver || exit 1
PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH arm-linux-androideabi-strip gdbserver || exit 1
popd


popd
