#!/bin/bash -xv

dir=$(dirname $0)/..
cd $dir

com=./COMMANDS/nameread

ERROR_CHECK(){
	[ $(plus ${PIPESTATUS[@]} ) -eq 0 ] && return
	rm -f $tmp-*
	exit 1
}

tmp=/tmp/$$

cat << FIN > $tmp-name
A01 1
A02 2
A03 3
A10 5
FIN

###########################################
#TEST1: ordinaly use

ANS=$( ${com} A01 $tmp-name )
[ "${ANS}" = "1" ] ; ERROR_CHECK

###########################################
#TEST2: regular expression

${com} -e '^A0[0-9]$' $tmp-name > $tmp-out

cat << FIN > $tmp-ans
1
2
3
FIN

diff $tmp-ans $tmp-out ; ERROR_CHECK

###########################################
#TEST2: regular expression with output of names

${com} -el '^A0[0-9]$' $tmp-name > $tmp-out

cat << FIN > $tmp-ans
A01 1
A02 2
A03 3
FIN

diff $tmp-ans $tmp-out ; ERROR_CHECK

rm $tmp-*
echo OK
exit 0
