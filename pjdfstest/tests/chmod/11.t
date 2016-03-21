#!/bin/sh
# $FreeBSD: head/tools/regression/pjdfstest/tests/chmod/11.t 211352 2010-08-15 21:24:17Z pjd $

desc="chmod returns EFTYPE if the effective user ID is not the super-user, the mode includes the sticky bit (S_ISVTX), and path does not refer to a directory"

dir=`dirname $0`
. ${dir}/../misc.sh

echo "1..14"

n0=`namegen`
n1=`namegen`
n2=`namegen`

expect 0 mkdir ${n0} 0755
cdir=`pwd`
cd ${n0}

for type in regular dir symlink; do
	if [ "${type}" != "symlink" ]; then
		create_file ${type} ${n1}
		expect 0 symlink ${n1} ${n2}
		expect 0 unlink ${n2}
		if [ "${type}" = "dir" ]; then
			expect 0 rmdir ${n1}
		else
			expect 0 unlink ${n1}
		fi
	fi

done

expect 0 mkdir ${n1} 0755
expect 0 symlink ${n1} ${n2}
expect 0 unlink ${n2}
expect 0 rmdir ${n1}

cd ${cdir}
expect 0 rmdir ${n0}
