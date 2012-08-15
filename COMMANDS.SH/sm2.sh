#! /bin/sh


# print the usage and exit
print_usage_and_exit () {
	cat <<____USAGE > /dev/stderr
Usage   : ${0##*/} [+count] <k1> <k2> <s1> <s2> <file>
Version : Sun Aug 12 21:10:12 JST 2012
____USAGE
	exit 1
}


# initialize
count=0


# parse the arguments
if [ "_$1" = "_+count" ]; then
	count=1
	shift
fi

[ $# -lt 4 ] && print_usage_and_exit

[ "$(echo "_$1$2$3$4" | tr -d '0-9')" = '_' ] || print_usage_and_exit
[ \( $1 -gt $2 \) ] && print_usage_and_exit
[ \( $3 -gt $4 \) -o \( $3 -le $2 \) ] && print_usage_and_exit
if [ $1 -gt 0 ]; then
	keyfileds1=$(awk -v s=$1 -v e=$2 'BEGIN{for(i=s;i<=e;i++){printf("$%d",i)}}')
	keyfileds2=$(echo $keyfileds1 | sed 's/\([0-9]\)\$/\1,$/g')
elif [ $2 -eq 0 ]; then
	keyfileds1='""'
	keyfileds2='""'
else
	print_usage_and_exit
fi

file=/dev/stdin
if [ $# -gt 4 ]; then
	if [ \( -f "$5" \) -o \( -c "$5" \) ]; then
		file=$5
	else
		print_usage_and_exit
	fi
fi


# sum-up
exec awk -F ' ' -v s=$3 -v e=$4 -v addcountfield=$count '
BEGIN {
	ORS = "";
	key0 = "";
	samekey = 0;
	notprintedyet = 0;
	_assert_exit = 0;
}

{
	if (NF < e) {
		printf("There is a record which does not have enough number of fields!\n") > "/dev/stderr";
		_assert_exit = 1;
		exit _assert_exit;
	}
	key = '$keyfileds1';
	samekey = (key == key0) ? 1 : 0;
	notprintedyet = 1;
	if (samekey) {
		for (i = s; i <= e; i++) {
			sum[i] += $i;
		}
		samekeylines++;
	} else {
		if (NR > 1) {
			print_smrecord();
			notprintedyet=0;
		}
		for (i = s; i <= e; i++) {
			sum[i] = $i;
		}
		samekeylines = 1;
	}
	key0 = key;
}

END {
	if (_assert_exit) {
		exit _assert_exit;
	}
	if (notprintedyet) {
		print_smrecord();
	}
}

function print_smrecord() {
	print '$keyfileds2';
	space = (key == "") ? "" : " ";
	if (addcountfield) {
		printf("%s%d", space, samekeylines);
		space = " ";
	}
	for (i = s; i <= e; i++) {
		printf("%s%d", space, sum[i]);
		space = " ";
	}
	printf("\n");
}' "$file"