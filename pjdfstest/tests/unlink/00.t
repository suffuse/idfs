#!/bin/sh
# $FreeBSD: head/tools/regression/pjdfstest/tests/unlink/00.t 211352 2010-08-15 21:24:17Z pjd $

desc="unlink removes regular files, symbolic links, fifos and sockets"

dir=`dirname $0`
. ${dir}/../misc.sh

echo "1..20"

n0=`namegen`
n1=`namegen`
n2=`namegen`

expect 0 mkdir ${n2} 0755
cdir=`pwd`
cd ${n2}

expect 0 create ${n0} 0644
expect regular lstat ${n0} type
expect 0 unlink ${n0}
expect ENOENT lstat ${n0} type

expect 0 symlink ${n1} ${n0}
expect symlink lstat ${n0} type
expect 0 unlink ${n0}
expect ENOENT lstat ${n0} type

expect 0 mkdir ${n0} 0755
expect 0 create ${n0}/${n1} 0644
time=`${fstest} stat ${n0} ctime`
sleep 1
expect 0 unlink ${n0}/${n1}
mtime=`${fstest} stat ${n0} mtime`
test_check $time -lt $mtime
expect 0 rmdir ${n0}

expect 0 mkdir ${n0} 0755
expect 0 symlink test ${n0}/${n1}
time=`${fstest} stat ${n0} ctime`
sleep 1
expect 0 unlink ${n0}/${n1}
mtime=`${fstest} stat ${n0} mtime`
test_check $time -lt $mtime
expect 0 rmdir ${n0}

cd ${cdir}
expect 0 rmdir ${n2}
