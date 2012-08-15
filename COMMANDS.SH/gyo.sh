#! /bin/sh


# print the usage and exit
print_usage_and_exit () {
	cat <<____USAGE > /dev/stderr
Usage   : ${0##*/} [-f] <file> ...
Version : Sun Aug 14 17:45:00 JST 2012
____USAGE
	exit 1
}


# initialize
foption=0
file=''
use_stdin=0


# parse arguments and count lines
for arg in "$@"; do
	case "$arg" in
		-f)
			foption=1
			continue
			;;
		-)
			[ $use_stdin -ne 0 ] && print_usage_and_exit
			use_stdin=1
			file='/dev/stdin'
			;;
		*)
			if [ \( "_$arg" = '_/dev/stdin' \) -o  \( "_$arg" = '_/dev/fd/0' \) ]; then
				[ $use_stdin -ne 0 ] && print_usage_and_exit
				use_stdin=1
			elif [ \( -f "$arg" \) -o \( -c "$arg" \) ]; then
				file=$arg
			else
				print_usage_and_exit
			fi
			;;
	esac
	if [ $foption -ne 0 ]; then
		printf '%s %s\n' "$arg" $(cat "$file" | wc -l)
	else
		cat "$file" | wc -l | awk '{print $1}'
	fi
done
if [ \( -z "$file" \) -a \( $foption -ne 0 \) ]; then
	printf '\055 %s\n' $(cat /dev/stdin | wc -l)
elif [ \( -z "$file" \) -a \( $foption -eq 0 \) ]; then
	cat /dev/stdin | wc -l | awk '{print $1}'
fi