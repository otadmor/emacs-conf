#!/bin/bash
NDK=/opt/android-ndk-r21d
if [[ ! -d $NDK ]]; then
    echo Missing ndk $NDK
    exit 1
fi
GDB_VERSION=10.1
PYTHON_VERSION=3.8
GUILE_VERSION=2.0
sudo apt-get install libipt-dev gcc-multilib guile-${GUILE_VERSION} guile-${GUILE_VERSION}-dev guile-${GUILE_VERSION}-libs python${PYTHON_VERSION}-dev libmpfr-dev texi2html texinfo || exit 1
sudo apt-get install gcc-arm-linux-gnueabi g++-arm-linux-gnueabi || exit 1

# to check --enable-gdbtk --enable-tui, --enable-gtk
rm gdb-$GDB_VERSION.tar gdb-$GDB_VERSION.tar.xz
wget https://ftp.gnu.org/gnu/gdb/gdb-$GDB_VERSION.tar.xz
# wget ftp://sourceware.org/pub/gdb/releases/gdb-$GDB_VERSION.tar.xz
xz -dk gdb-$GDB_VERSION.tar.xz
tar -xf gdb-$GDB_VERSION.tar
gdb_prefix=/opt/gdb

cd gdb-${GDB_VERSION}


mkdir build-multiarch
cd build-multiarch && ../configure \
                          --enable-tui --with-guile=guile-${GUILE_VERSION} --with-mpfr --with-expat \
                          --with-zlib --with-lzma --with-python=python${PYTHON_VERSION} \
                          --prefix=${gdb_prefix} --enable-targets=all && \
    make && sudo make install && cd .. || exit 1


GDB=$(which gdb)
update-alternatives --query gdb
if [[ "$?" != "0" ]] ; then
    if [[ "$GDB" != "" ]]; then
        GDB_NEW_NAME=$(dirname $GDB)/gdb-$(gdb --version | sed -rn 's/.*gdb.*\) (.*?)/\1/p')
        if [[ ! -f $GDB_NEW_NAME ]]; then
            sudo mv $GDB ${GDB_NEW_NAME}
        fi
        sudo update-alternatives --install $GDB gdb ${GDB_NEW_NAME} 10
    fi
fi
if [[ "$GDB" == "" ]]; then
    GDB=/usr/local/bin/gdb
fi
sudo update-alternatives --install $GDB gdb ${gdb_prefix}/bin/gdb 20

mkdir build-gdbserver-arm-linux-gnueabi && cd build-gdbserver-arm-linux-gnueabi || exit 1

mkdir zlib && cd zlib && ../../zlib/configure  \
        LDFLAGS=-static --host=arm-linux-gnueabi && \
    make && cd .. || exit 1
mkdir bfd && cd bfd && ../../bfd/configure  \
        LDFLAGS=-static --host=arm-linux-gnueabi && \
    make && cd .. || exit 1
chmod +x ../gnulib/configure && mkdir gnulib && cd gnulib && ../../gnulib/configure  \
        LDFLAGS=-static --host=arm-linux-gnueabi && \
    make && cd .. || exit 1
mkdir gdbsupport && cd gdbsupport && ../../gdbsupport/configure  \
        LDFLAGS=-static --host=arm-linux-gnueabi && \
    make && cd .. || exit 1
mkdir libiberty && cd libiberty && ../../libiberty/configure \
        LDFLAGS=-static --host=arm-linux-gnueabi && \
    make && cd .. || exit 1
../gdbserver/configure CPPFLAGS="-include gnulib/import/stdio.h"  \
        LDFLAGS=-static --host=arm-linux-gnueabi && \
    sed -i 's/GNULIB_BUILDDIR = ..\/gnulib/GNULIB_BUILDDIR = gnulib/g' Makefile && \
    sed -i 's/LIBIBERTY_BUILDDIR = ..\/libiberty/LIBIBERTY_BUILDDIR = libiberty/g' Makefile && \
    sed -i 's/GDBSUPPORT_BUILDDIR = ..\/gdbsupport/GDBSUPPORT_BUILDDIR = gdbsupport/g' Makefile && \
    make gdbserver && cd .. || exit 1


mkdir build-gdbserver-aarch64-linux-android && cd build-gdbserver-aarch64-linux-android || exit 1
export PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH
mkdir zlib && cd zlib && ../../zlib/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS=-static CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    make && cd .. || exit 1
mkdir bfd && cd bfd && ../../bfd/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS=-static CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    make && cd .. || exit 1
chmod +x ../gnulib/configure && mkdir gnulib && cd gnulib && ../../gnulib/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS=-static CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    sed -i 's/HAVE_MKOSTEMP = 1/HAVE_MKOSTEMP = 0/g' import/Makefile && \
    sed -i 's/HAVE_STRCHRNUL = 1/HAVE_STRCHRNUL = 0/g' import/Makefile && \
    make && cd .. || exit 1
mkdir gdbsupport && cd gdbsupport && ../../gdbsupport/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS=-static CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    make && cd .. || exit 1
mkdir libiberty && cd libiberty && ../../libiberty/configure \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS=-static CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    make && cd .. || exit 1
PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH ../gdbserver/configure CPPFLAGS="-include gnulib/import/stdio.h"  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS=-static CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    sed -i 's/-Wl,--dynamic-list=$(srcdir)\/proc-service.list//g' Makefile && \
    sed -i 's/GNULIB_BUILDDIR = ..\/gnulib/GNULIB_BUILDDIR = gnulib/g' Makefile && \
    sed -i 's/LIBIBERTY_BUILDDIR = ..\/libiberty/LIBIBERTY_BUILDDIR = libiberty/g' Makefile && \
    sed -i 's/GDBSUPPORT_BUILDDIR = ..\/gdbsupport/GDBSUPPORT_BUILDDIR = gdbsupport/g' Makefile && \
    make gdbserver && aarch64-linux-android-strip gdbserver && cd .. || exit 1


mkdir build-gdbserver-armv7a-linux-androideabi && cd build-gdbserver-armv7a-linux-androideabi || exit 1
export PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH
mkdir zlib && cd zlib && ../../zlib/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static" CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    make && cd .. || exit 1
mkdir bfd && cd bfd && ../../bfd/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static" CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    make && cd .. || exit 1
chmod +x ../gnulib/configure && mkdir gnulib && cd gnulib && ../../gnulib/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static" CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    sed -i 's/HAVE_MKOSTEMP = 1/HAVE_MKOSTEMP = 0/g' import/Makefile && \
    sed -i 's/HAVE_STRCHRNUL = 1/HAVE_STRCHRNUL = 0/g' import/Makefile && \
    make && cd .. || exit 1
mkdir gdbsupport && cd gdbsupport && ../../gdbsupport/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static" CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    make && cd .. || exit 1
mkdir libiberty && cd libiberty && ../../libiberty/configure \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static" CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    make && cd .. || exit 1
PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH ../gdbserver/configure CPPFLAGS="-include gnulib/import/stdio.h"  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static" CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    sed -i 's/-Wl,--dynamic-list=$(srcdir)\/proc-service.list//g' Makefile && \
    sed -i 's/GNULIB_BUILDDIR = ..\/gnulib/GNULIB_BUILDDIR = gnulib/g' Makefile && \
    sed -i 's/LIBIBERTY_BUILDDIR = ..\/libiberty/LIBIBERTY_BUILDDIR = libiberty/g' Makefile && \
    sed -i 's/GDBSUPPORT_BUILDDIR = ..\/gdbsupport/GDBSUPPORT_BUILDDIR = gdbsupport/g' Makefile && \
    make gdbserver && arm-linux-androideabi-strip gdbserver && cd .. || exit 1
