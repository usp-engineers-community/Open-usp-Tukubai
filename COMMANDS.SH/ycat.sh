#! /bin/sh


# print the usage and exit
print_usage_and_exit () {
	cat <<____USAGE > /dev/stderr
Usage   : ${0##*/} [-<n>] <file1> <file2> ...
Version : Sun Aug 14 17:45:00 JST 2012
____USAGE
	exit 1
}


# initialize
numoption=1
octed_filenames=''
use_stdin=0
tmpfiles=''
hexed_filelist=' '
LF=$(printf '\012_'); LF=${LF%_}


# parse arguments
mktemp_instead_of_stdin () {
	tmpfile=$(mktemp -t "${0##*/}")
	if [ $? -eq 0 ]; then
		tmpfiles="$tmpfiles $tmpfile"
		trap "rm -f $tmpfiles" EXIT HUP INT QUIT ALRM SEGV TERM
		cat /dev/stdin > "$tmpfile"
	else
		echo "${0##*/} : Cannot create a temporary file" > /dev/stderr
		exit 1
	fi
}
for arg in "$@"; do
	case "$arg" in
		-[0-9]*)
			numoption=$(echo "${arg#-}" | grep '^[0-9]\+$')
			if [ $? -ne 0 ]; then
				print_usage_and_exit
			fi
			continue
			;;
		-)
			[ $use_stdin -ne 0 ] && print_usage_and_exit
			use_stdin=1
			mktemp_instead_of_stdin
			file=$tmpfile
			;;
		*)
			if [ \( "_$arg" = '_/dev/stdin' \) -o \( "_$arg" = '_/dev/fd/0' \) ]; then
				[ $use_stdin -ne 0 ] && print_usage_and_exit
				use_stdin=1
				mktemp_instead_of_stdin
				file=$tmpfile
			elif [ \( -f "$arg" \) -o \( -c "$arg" \) ]; then
				str=$(echo -n "_$arg" | sed 's/^_//' | od -A n -t x1 | tr -d ' \n')
				echo "$hexed_filelist" | grep -F " $str " > /dev/null
				if [ $? -ne 0 ]; then
					hexed_filelist="$hexed_filelist$str "
					file=$arg
				else
					retry=100
					while [ $retry -gt 0 ]; do
						retry=$((retry-1))
						tmpfile=$(mktemp -u -t "${0##*/}.XXXXXXXX")
						if [ $? -eq 0 ]; then
							ln -s "$arg" "$tmpfile" > /dev/null 2>&1
							[ $? -ne 0 ] && continue
							tmpfiles="$tmpfiles $tmpfile"
							trap "rm -f $tmpfiles" EXIT HUP INT QUIT ALRM SEGV TERM
							break
						else
							echo "${0##*/} : Cannot create a temporary file" > /dev/stderr
							exit 1
						fi
					done
					file=$tmpfile
				fi
			else
				print_usage_and_exit
			fi
			;;
	esac
	# transfer the characters to octet escaped strings to import to AWK safely
	octed_filenames="$octed_filenames $(echo -n "$file" | od -A n -t o1 | tr -d '\n' | sed -e 's/[[:blank:]]*$//;' | sed -e 's/[[:blank:]]\{1,\}/\\/g' )"
done
octed_filenames=${octed_filenames# }


# awkcode : get the length of the UTF-8 string (it is not a byte number)
awkcode_func_utf8strlen='
# strlen for UTF-8 (preparation : you must call this before using utf8strlen)
function utf8strlen_prep() {
	utf8strlen_80_1 = sprintf("\200");
	utf8strlen_C0_X = sprintf("\300");
	utf8strlen_E0_2 = sprintf("\340");
	utf8strlen_F0_3 = sprintf("\360");
	utf8strlen_F8_4 = sprintf("\370");
	utf8strlen_FC_5 = sprintf("\374");
	utf8strlen_FE_6 = sprintf("\376");
}

# strlen for UTF-8 (main)
function utf8strlen(str) {
	utf8strlen_ = 0;
	for (utf8strlen_i = 1; utf8strlen_i <= length(str); utf8strlen_i++) {
		utf8strlen_++;
		utf8strlen_letter=substr(str, utf8strlen_i, 1);
		if (utf8strlen_letter < utf8strlen_80_1) {
			continue;
		} else if (utf8strlen_letter < utf8strlen_C0_X) {
			utf8strlen_--;
			continue;
		} else if (utf8strlen_letter < utf8strlen_E0_2) {
			utf8strlen_i++;
		} else if (utf8strlen_letter < utf8strlen_F0_3) {
			utf8strlen_i += 2;
		} else if (utf8strlen_letter < utf8strlen_F8_4) {
			utf8strlen_i += 3;
		} else if (utf8strlen_letter < utf8strlen_FC_5) {
			utf8strlen_i += 4;
		} else if (utf8strlen_letter < utf8strlen_FE_6) {
			utf8strlen_i += 5;
		} else {
			utf8strlen_--;
			continue;
		}
		utf8strlen_++;
	}
	return utf8strlen_;
}
'


# get the maximal widthes of the files
awkcode_main1_get_maxwidthes='
BEGIN {
	utf8strlen_prep();

	split(octed_filenames, octed_filename);
	for (i = 1; i <= length(octed_filename); i++) {
		filename[i] = sprintf(octed_filename[i]);
	}

	for (i = 1; i <= length(filename); i++) {
		maxwidth[i] = 0;
		while (getline str < filename[i]) {
			width = utf8strlen(str);
			if (width > maxwidth[i]) {
				maxwidth[i] = width;
			}
		}
		close(filename[i]);
	}

	str = "";
	for (i = 1; i <= length(maxwidth); i++) {
		str = str " " maxwidth[i];
	}
	print substr(str, 2, length(str) - 1);
}
'
max_widthes=$(awk -v "octed_filenames=$octed_filenames" "$awkcode_main1_get_maxwidthes$awkcode_func_utf8strlen")


# ycat
awkcode_main2_ycat='
BEGIN {
	utf8strlen_prep();

	split(octed_filenames, octed_filename);
	for (i = 1; i <= length(octed_filename); i++) {
		filename[i] = sprintf(octed_filename[i]);
	}

	split(max_widthes, max_width);

	margin="";
	for (i = 0; i < numoption; i++) {
		margin = margin FS;
	}
	while (1) {
		line = "";
		validlines = 0;
		for (i = 1; i <= length(filename); i++) {
			if (length(filename[i])) {
				if (getline str < filename[i]) {
					validlines++;
				} else {
					close(filename[i]);
					filename[i] = "";
					str = "";
				}
			} else {
				str = "";
			}
			for (j = utf8strlen(str); j < max_width[i]; j++) {
				str = str " ";
			}
			line = (i == 1) ? str : line margin str;
		}
		if (validlines) {
			print line;
			continue;
		}
		else {
			break;
		}
	}
}
'
if [ -z "$tmpfiles" ]; then
	exec awk -v numoption=$numoption -v "octed_filenames=$octed_filenames" -v "max_widthes=$max_widthes" "$awkcode_main2_ycat$awkcode_func_utf8strlen"
else
	awk -v numoption=$numoption -v "octed_filenames=$octed_filenames" -v "max_widthes=$max_widthes" "$awkcode_main2_ycat$awkcode_func_utf8strlen"
fi