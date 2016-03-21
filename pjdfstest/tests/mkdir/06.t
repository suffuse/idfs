#!/bin/sh
# $FreeBSD: head/tools/regression/pjdfstest/tests/mkdir/06.t 211352 2010-08-15 21:24:17Z pjd $

desc="mkdir returns EACCES when write permission is denied on the parent directory of the directory to be created"

dir=`dirname $0`
. ${dir}/../misc.sh

echo "1..6"

n0=`namegen`
n1=`namegen`
n2=`namegen`

expect 0 mkdir ${n0} 0755
cdir=`pwd`
cd ${n0}
expect 0 mkdir ${n1} 0755
expect 0 chmod ${n1} 0555
expect 0 chmod ${n1} 0755
expect 0 rmdir ${n1}
cd ${cdir}
expect 0 rmdir ${n0}
