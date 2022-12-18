if ! [ -e "$1" ]; then
    LAPACK_VERSION=3.10.1
else
    LAPACK_VERSION=$1
fi

lapackDirName="$(basename -s .gz lapack-$LAPACK_VERSION)"
if [ -d "$lapackDirName" ]; then
    echo "LAPACK is already downloaded."
else
    wget http://www.netlib.org/lapack/lapack-$LAPACK_VERSION.tgz
    tar -xvzf lapack-$LAPACK_VERSION.tgz
fi

currentPath="$( cd "$( dirname "$0" )" && pwd )"
lapackPath=$currentPath/$lapackDirName
installPath=$lapackPath/lib
{
    cd $lapackPath
    cmake -S. -Bbuild -DCMAKE_INSTALL_LIBDIR=$installPath
    cmake --build build -j --target install
    cd --
} 2>&1 | tee lapack.log
