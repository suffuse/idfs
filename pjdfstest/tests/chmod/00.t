#!/bin/sh
# $FreeBSD: head/tools/regression/pjdfstest/tests/chmod/00.t 211352 2010-08-15 21:24:17Z pjd $

desc="chmod changes permission"

dir=`dirname $0`
. ${dir}/../misc.sh

echo "1..22"

n0=`namegen`
n1=`namegen`
n2=`namegen`

expect 0 mkdir ${n2} 0755
cdir=`pwd`
cd ${n2}

for type in regular dir symlink; do
	if [ "${type}" != "symlink" ]; then
		create_file ${type} ${n0}
		expect 0 chmod ${n0} 0111
		expect 0111 stat ${n0} mode

		expect 0 symlink ${n0} ${n1}
		mode=`${fstest} lstat ${n1} mode`
		expect 0 chmod ${n1} 0222
		expect 0222 stat ${n1} mode
		expect 0222 stat ${n0} mode
		expect ${mode} lstat ${n1} mode
		expect 0 unlink ${n1}

		if [ "${type}" = "dir" ]; then
			expect 0 rmdir ${n0}
		else
			expect 0 unlink ${n0}
		fi
	fi
done

cd ${cdir}
expect 0 rmdir ${n2}
