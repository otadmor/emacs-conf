#!/bin/bash
# https://dl.google.com/android/repository/android-ndk-r25b-linux.zip
NDK=/opt/android-ndk-r25b/
if [[ ! -d $NDK ]]; then
    echo Missing ndk $NDK
    exit 1
fi
if ! grep -F "Pkg.Revision = 25." source.properties; then
    echo NDK version should be r25b
fi
gdb_prefix=/opt/gdb
GDB_VERSION=12.1
PYTHON_VERSION=3.10
GUILE_VERSION=2.2
# gcc-multilib g++-multilib
sudo apt-get install build-essential libipt-dev guile-${GUILE_VERSION} guile-${GUILE_VERSION}-dev guile-${GUILE_VERSION}-libs python${PYTHON_VERSION}-dev libmpfr-dev texi2html texinfo || exit 1


if [[ ! -f gnu-keyring.gpg ]]; then
    wget ftp://ftp.gnu.org/gnu/gnu-keyring.gpg && gpg --import gnu-keyring.gpg || exit 1
fi

if [[ ! -f gdb-$GDB_VERSION.tar.xz.sig ]]; then
    wget https://ftp.gnu.org/gnu/gdb/gdb-$GDB_VERSION.tar.xz.sig || exit 1
fi

if [[ ! -f gdb-$GDB_VERSION.tar.xz ]]; then
    wget https://ftp.gnu.org/gnu/gdb/gdb-$GDB_VERSION.tar.xz || exit 1
fi

if ! gpg --verify gdb-$GDB_VERSION.tar.xz.sig gdb-$GDB_VERSION.tar.xz; then
    echo redownload
    rm gdb-$GDB_VERSION.tar.xz || exit 1
    wget https://ftp.gnu.org/gnu/gdb/gdb-$GDB_VERSION.tar.xz || exit 1
    if ! gpg --verify gdb-$GDB_VERSION.tar.xz.sig gdb-$GDB_VERSION.tar.xz; then
        echo CANNOT DOWNLOAD GDB WITH THE CORRECT SIGNATURE
        exit 1
    fi
fi

# to check --enable-gdbtk --enable-tui, --enable-gt
if [[ -d gdb-$GDB_VERSION ]]; then
    rm -rf gdb-$GDB_VERSION
fi
tar -xf gdb-$GDB_VERSION.tar.xz

cd gdb-${GDB_VERSION}

sed -i 's/::kill (-signal_pid, SIGINT);/::kill (-signal_pid, SIGINT) \&\& ::kill (signal_pid, SIGINT);/g' gdbserver/linux-low.cc || exit 1

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

mkdir build-gdbserver-aarch64-linux-android && cd build-gdbserver-aarch64-linux-android || exit 1
export PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH
mkdir zlib && cd zlib && ../../zlib/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    make && cd .. || exit 1
mkdir bfd && cd bfd && ../../bfd/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS="-static" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    make && cd .. || exit 1
chmod +x ../gnulib/configure && mkdir gnulib && cd gnulib && ../../gnulib/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    sed -i 's/$(srcdir)\/$(GNULIB_PARENT_DIR)\//..\/$(GNULIB_PARENT_DIR)\//g' Makefile.gnulib.inc && \
    sed -i 's/HAVE_MKOSTEMP = 1/HAVE_MKOSTEMP = 0/g' import/Makefile && \
    sed -i 's/HAVE_STRCHRNUL = 1/HAVE_STRCHRNUL = 0/g' import/Makefile && \
    make && cd .. || exit 1
mkdir gdbsupport && cd gdbsupport && ../../gdbsupport/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    make && cd .. || exit 1
mkdir libiberty && cd libiberty && ../../libiberty/configure \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    make && cd .. || exit 1
PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH ../gdbserver/configure CPPFLAGS="-include gnulib/import/stdio.h"  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=aarch64-linux-android --program-prefix=aarch64-linux-android- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=aarch64-linux-android21-clang CXX=aarch64-linux-android21-clang++ && \
    sed -i 's/-Wl,--dynamic-list=$(srcdir)\/proc-service.list//g' Makefile && \
    sed -i 's/GNULIB_BUILDDIR = ..\/gnulib/GNULIB_BUILDDIR = gnulib/g' Makefile && \
    sed -i 's/GNULIB_PARENT_DIR = ../GNULIB_PARENT_DIR = ./g' Makefile && \
    sed -i 's/LIBIBERTY_BUILDDIR = ..\/libiberty/LIBIBERTY_BUILDDIR = libiberty/g' Makefile && \
    sed -i 's/GDBSUPPORT_BUILDDIR = ..\/gdbsupport/GDBSUPPORT_BUILDDIR = gdbsupport/g' Makefile && \
    make gdbserver && llvm-strip gdbserver && cd .. || exit 1


mkdir build-gdbserver-armv7a-linux-androideabi && cd build-gdbserver-armv7a-linux-androideabi || exit 1
export PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH
mkdir zlib && cd zlib && ../../zlib/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    make && cd .. || exit 1
mkdir bfd && cd bfd && ../../bfd/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    make && cd .. || exit 1
chmod +x ../gnulib/configure && mkdir gnulib && cd gnulib && ../../gnulib/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    sed -i 's/$(srcdir)\/$(GNULIB_PARENT_DIR)\//..\/$(GNULIB_PARENT_DIR)\//g' Makefile.gnulib.inc && \
    sed -i 's/HAVE_MKOSTEMP = 1/HAVE_MKOSTEMP = 0/g' import/Makefile && \
    sed -i 's/HAVE_STRCHRNUL = 1/HAVE_STRCHRNUL = 0/g' import/Makefile && \
    make && cd .. || exit 1
mkdir gdbsupport && cd gdbsupport && ../../gdbsupport/configure  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    make && cd .. || exit 1
mkdir libiberty && cd libiberty && ../../libiberty/configure \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    make && cd .. || exit 1
PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH ../gdbserver/configure CPPFLAGS="-include gnulib/import/stdio.h"  \
        PATH=$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/:$PATH \
        --host=arm-linux-androideabi --program-prefix=arm-linux-androideabi- \
        LDFLAGS="-static -ffunction-sections -fdata-sections -Wl,--gc-sections" \
        CPPFLAGS="-DHAVE_ELF32_AUXV_T -DHAVE_ELF64_AUXV_T" \
        CC=armv7a-linux-androideabi21-clang CXX=armv7a-linux-androideabi21-clang++ && \
    sed -i 's/-Wl,--dynamic-list=$(srcdir)\/proc-service.list//g' Makefile && \
    sed -i 's/GNULIB_BUILDDIR = ..\/gnulib/GNULIB_BUILDDIR = gnulib/g' Makefile && \
    sed -i 's/GNULIB_PARENT_DIR = ../GNULIB_PARENT_DIR = ./g' Makefile && \
    sed -i 's/LIBIBERTY_BUILDDIR = ..\/libiberty/LIBIBERTY_BUILDDIR = libiberty/g' Makefile && \
    sed -i 's/GDBSUPPORT_BUILDDIR = ..\/gdbsupport/GDBSUPPORT_BUILDDIR = gdbsupport/g' Makefile && \
    make gdbserver && llvm-strip gdbserver && cd .. || exit 1

sudo apt-get install gcc-arm-linux-gnueabi g++-arm-linux-gnueabi g++-11-arm-linux-gnueabi gcc-11-arm-linux-gnueabi || exit 1

mkdir build-gdbserver-arm-linux-gnueabi && cd build-gdbserver-arm-linux-gnueabi || exit 1
mkdir zlib && cd zlib && ../../zlib/configure  \
        LDFLAGS=-static --host=arm-linux-gnueabi \
        CC=arm-linux-gnueabi-gcc CXX=arm-linux-gnueabi-g++ AR=arm-linux-gnueabi-ar && \
    make && cd .. || exit 1
mkdir bfd && cd bfd && ../../bfd/configure  \
        --host=arm-linux-gnueabi \
        CC=arm-linux-gnueabi-gcc CXX=arm-linux-gnueabi-g++ AR=arm-linux-gnueabi-ar && \
    make && cd .. || exit 1
chmod +x ../gnulib/configure && mkdir gnulib && cd gnulib && ../../gnulib/configure  \
        LDFLAGS=-static --host=arm-linux-gnueabi \
        CC=arm-linux-gnueabi-gcc CXX=arm-linux-gnueabi-g++ AR=arm-linux-gnueabi-ar && \
    sed -i 's/$(srcdir)\/$(GNULIB_PARENT_DIR)\//..\/$(GNULIB_PARENT_DIR)\//g' Makefile.gnulib.inc && \
    make && cd .. || exit 1
mkdir gdbsupport && cd gdbsupport && ../../gdbsupport/configure  \
        LDFLAGS=-static --host=arm-linux-gnueabi \
        CC=arm-linux-gnueabi-gcc CXX=arm-linux-gnueabi-g++ AR=arm-linux-gnueabi-ar && \
    make && cd .. || exit 1
mkdir libiberty && cd libiberty && ../../libiberty/configure \
        LDFLAGS=-static --host=arm-linux-gnueabi \
        CC=arm-linux-gnueabi-gcc CXX=arm-linux-gnueabi-g++ AR=arm-linux-gnueabi-ar && \
    make && cd .. || exit 1
../gdbserver/configure CPPFLAGS="-include gnulib/import/stdio.h"  \
        LDFLAGS=-static --host=arm-linux-gnueabi \
        CC=arm-linux-gnueabi-gcc CXX=arm-linux-gnueabi-g++ AR=arm-linux-gnueabi-ar && \
    sed -i 's/GNULIB_BUILDDIR = ..\/gnulib/GNULIB_BUILDDIR = gnulib/g' Makefile && \
    sed -i 's/GNULIB_PARENT_DIR = ../GNULIB_PARENT_DIR = ./g' Makefile && \
    sed -i 's/LIBIBERTY_BUILDDIR = ..\/libiberty/LIBIBERTY_BUILDDIR = libiberty/g' Makefile && \
    sed -i 's/GDBSUPPORT_BUILDDIR = ..\/gdbsupport/GDBSUPPORT_BUILDDIR = gdbsupport/g' Makefile && \
    make gdbserver && arm-linux-gnueabi-strip gdbserver && cd .. || exit 1
