#! /bin/sh

# $Id$

# Extract RCS identifier information from files (or standard input) and
# write Fortran block data subprogram 'id_data' to standard output with
# the identifiers data loaded in a common block 'id_common'.

cat $* | ident -q | egrep '\$(Id|Header):' | sort | uniq | sed 's/^ *//' | \
awk '
BEGIN {
    print "      block data id_data";
    i = 0;
    quote = 39;
}
{
    line = $0;
    if (length(line) > 0) {
	i = i + 1;
	printf "      character id_%d*%d\n", i, length(line);
	printf "      common /id_common/ id_%d\n", i
	line = sprintf("data id_%d /%c%s%c/", i, quote, line, quote);
	cont = " ";
	while (length(line) > 0) {
	    printf "     %s%s\n", cont, substr(line,1,66);
	    line = substr(line,67);
	    cont = "+";
	}
    }
}
END {
    print "      end"
}
'
