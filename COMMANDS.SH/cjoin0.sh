#! /bin/sh


# print the usage and exit
print_usage_and_exit () {
	cat <<____USAGE > /dev/stderr
Usage   : ${0##*/} [+ng[<fd>]] key=<n> <master> <tran>
Version : Sun Aug 12 17:34:00 JST 2012
____USAGE
	exit 1
}


# initialize
ng=''
key_1=''
key_2=''
masterfile=''
tranfile=''


# validate the arguments
for arg in "$@"; do
	case "$arg" in
		key=*)
			key=${arg#key=}
			key_1=''
			key_2=''
			$(echo "_$key" | grep '^_[0-9]\{1,\}$' > /dev/null)
			if [ $? -eq 0 ]; then
				key_1=$key
				key_2=$key
			fi
			$(echo "_$key" | grep '^_[0-9]\{1,\}/[0-9]\{1,\}$' > /dev/null)
			if [ $? -eq 0 ]; then
				key_1="${key%/*}"
				key_2="${key#*/}"
			fi
			if [ \( -n $key_1 \) -a \( -n $key_2 \) -a \( $key_2 -ge $key_1 \) ]; then
				continue
			else
				print_usage_and_exit
			fi
			;;
		+ng*)
			ng=${arg#+ng}
			$(echo "_$ng" | grep '^_[0-9]\{1,\}$' > /dev/null)
			if [ $? -ne 0 ]; then
				print_usage_and_exit
			fi
			;;
		*)
			if [ -z "$masterfile" ]; then
				if [ "_$arg" = '_-' ]; then
					masterfile='/dev/stdin'
				elif [ \( -f "$arg" \) -o \( -c "$arg" \) ]; then
					masterfile=$arg
				fi
			elif [ -z "$tranfile" ]; then
				if [ "_$arg" = '_-' ]; then
					tranfile='/dev/stdin'
				elif [ \( -f "$arg" \) -o \( -c "$arg" \) ]; then
					tranfile=$arg
				fi
			else
				print_usage_and_exit
			fi
			;;
	esac
done
if [ -z $key_1 ]; then
	print_usage_and_exit
fi
if [ -z "$masterfile" ]; then
	print_usage_and_exit
fi
if [ -z "$tranfile" ]; then
	tranfile='/dev/stdin'
fi
if [ \( \( "_$masterfile" = '_/dev/stdin' \) -o \( "_$masterfile" = '_/dev/fd/0' \) \) -a \( \( "_$tranfile" = '_/dev/stdin' \) -o \( "_$tranfile" = '_/dev/fd/0' \) \) ]; then
	print_usage_and_exit
fi


# join
exec awk -v key_1=$key_1 -v key_2=$key_2 -v ng=$ng \
'
	# prepare
	BEGIN {
		fd = (ng != "") ? "/dev/fd/"ng : "";
	}

	# print the records in tran which have the same key
	FNR < NR {
		if (key_1 == key_2) {
			str = $key_1
		} else {
			str = "";
			for (i = key_1; i <= key_2; i++) {
				str = str $i;
			}
		}
		if (str in masterkeys) {
			print;
		} else if (fd != "") {
			print > fd;
		}
		next;
	}

	# load the keys in master
	{
		if (key_1 == key_2) {
			str = $key_1
		} else {
			str = "";
			for (i = key_1; i <= key_2; i++) {
				str = str $i;
			}
		}
		masterkeys[str] = 1;
	}
' "$masterfile" "$tranfile"