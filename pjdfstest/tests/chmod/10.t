#!/bin/sh
# $FreeBSD: head/tools/regression/pjdfstest/tests/chmod/10.t 211352 2010-08-15 21:24:17Z pjd $

desc="chmod returns EFAULT if the path argument points outside the process's allocated address space"

dir=`dirname $0`
. ${dir}/../misc.sh

echo "1..2"

expect EFAULT chmod NULL 0644
expect EFAULT chmod DEADCODE 0644
