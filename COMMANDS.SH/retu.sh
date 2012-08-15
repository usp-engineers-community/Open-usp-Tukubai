#! /bin/sh


# print the usage and exit
print_usage_and_exit () {
	cat <<____USAGE > /dev/stderr
Usage   : ${0##*/} [-f] <file> ...
Version : Sun Aug 14 19:06:00 JST 2012
____USAGE
	exit 1
}


# initialize
foption=0
file=''
use_stdin=0



awkcode_main='
BEGIN {
	last_nf = 0;
}

{
	if (NF != last_nf) {
		if (length(filename) > 0) {
			printf("%s %d\n", filename, NF);
		} else {
			print NF;
		}
	}
	last_nf = NF;
}
'

# parse arguments and count lines
for arg in "$@"; do
	case "$arg" in
		-f)
			foption=1
			continue
			;;
		-)
			[ $use_stdin -ne 0 ] && print_usage_and_exit
			file='/dev/stdin'
			use_stdin=1
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
		filename=$arg
	else
		filename=''
	fi
	awk -v "filename=$filename" "$awkcode_main" "$file"
done
if [ \( -z "$file" \) -a \( $foption -ne 0 \) ]; then
	awk -v 'filename=-' "$awkcode_main" /dev/stdin
elif [ \( -z "$file" \) -a \( $foption -eq 0 \) ]; then
	awk -v 'filename=""' "$awkcode_main" /dev/stdin
fi