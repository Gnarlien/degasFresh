#! /bin/sh
# Do fweb style makedepend on args

# Looks for lines of the form
#    
#   @I  filename
#and
#   @i  filename
#
# Only treats bare file names (not ones in <> or ").  Doesn't treat include
# directories.  A comment can follow the file name but this must be separated
# from the file name by whitespace.
#
# Assumes Makefile is called Makefile.  Uses conventional "# DO NOT DELETE"
# to separate off depends.  Makefile lines are not broken.
#
# This is NOT fast!

temp=/tmp/makedepend_$$ # basename for temporary files

# $temp.a holds includes for intermediate files
# $temp.b holds accumulated includes for current file
# $temp.c a temporary file
# $temp.d holds accumulated includes for all files

# $temp.e holds F90 stuff

cp /dev/null $temp.d

for file do
    echo $file > $temp.a # Start things off by assuming "@i $file" is given.
    cp /dev/null $temp.b # No accumulated includes yet
    while true; do
	newfiles="`comm -13 $temp.b $temp.a`" # New files we need to consider
	cat $temp.a $temp.b | sort | uniq > $temp.c 
	mv $temp.c $temp.b # Merge new files into list for current file
	cp /dev/null $temp.a
	test "$newfiles" || break # Consider next file if no new files
	for f in $newfiles; do # Look in each new file
	    grep '^[ 	]*@[iI][ 	]' $f |
		sed -e 's/^[ 	]*@[iI][ 	]*//' -e 's/[ 	].*$//' |
		sort | uniq >> $temp.a
	done
	sort $temp.a | uniq > $temp.c # Prune out redundancies
	mv $temp.c $temp.a
    done
    grep -v "^$file\$" $temp.b > $temp.c # Remove head file.
    mv $temp.c $temp.b
    # Create a Makefile dependency line
    case $file in
        *.web ) test -s $temp.b && echo `echo $file | sed 's/\.[^.]*$/.f/'`: `cat $temp.b` >>$temp.d ;;
	*.hweb ) test -s $temp.b && grep '^[ 	]*package_init' $file > /dev/null && echo `echo $file | sed 's/\.[^.]*$/_mod.f/'`: `cat $temp.b` >>$temp.d ;;
    esac
done

# Do FORTRAN 90 depends
cp /dev/null $temp.e
for file do
    case $file in 
     *.hweb )
	mods=`grep "^[ 	]*package_init\([a-z0-9]*\)" $file | grep -v '@m' | tr -s '; 	/*!' '\012' | grep package_init | sort -u | sed -e 's/)/_suffix/' -e 's/^package_init(/prefix_/'`
	if test "$mods"; then
#	echo `echo $mods | sed -e 's/prefix_//' -e 's/_suffix/.mod/'`: `echo $file | sed 's/\.[^.]*$/_mod.f/'` >> $temp.e
	for mod in $mods;do
	    echo `echo $mod | sed -e 's/prefix_//' -e 's/_suffix/_MOD/'`:=`echo $file | sed 's/\.[^.]*$/_mod.$O/'` >> $temp.e
	 done
	fi
       ;;
    esac
done

for file do
    case $file in 
    *.web )
       mods=`grep "^[ 	]*[a-z0-9]*_common" $file | tr -s '; 	/*!' '\012' | grep _common | sort -u | sed -e 's/_common/_suffix/' -e 's/^/prefix_/'`
       if test "$mods"; then
	echo `echo $file | sed 's/\.[^.]*$/_mods/'`:= `echo $mods | sed -e 's/prefix_/$(/g' -e 's/_suffix/_MOD)/g'` >> $temp.e
#	echo `echo $file | sed 's/\.[^.]*$/.o/'`: `echo $mods | sed -e 's/prefix_/$(/g' -e 's/_suffix/_MOD)/'` >> $temp.e
	echo `echo $file | sed 's/\.[^.]*$/.o/'`: '$('`echo $file | sed 's/\.[^.]*$/_mods/'`')' >> $temp.e
       fi
       ;;
    esac
done

(
echo 'ifeq ($(FORTRAN90),yes)'
cat $temp.e
echo endif
echo 'ifeq ($(STANDALONE),yes)'
cat $temp.d
echo 'endif'
) > Makefile.depends

rm -rf $temp.*

###########################################


# Interprets Makefile.depends to assign the relevant _mod.f files to variables named xx_MOD
cat Makefile.depends |grep "MOD:="|sed 's/^\(..\)_MOD:=\(.*\)\.\$O/set(\1_MOD \2.f)/g' > cmakecommands

# Interprets Makefile.depends to get module dependencies of each .f source file listed.
cat Makefile.depends |grep "_mods:=" | sed 's/(/{/g' | sed 's/)/}/g' |sed 's/\(.*\)_mods:=\(.*\)$/list(APPEND \1_mods \2)/g' >>cmakecommands

# Interprets *Makefile* to create variables xxx_deps for .f files manually specified in Makefile
grep "^.* = " Makefile|grep "\.\$O" | sed 's/\$(DG2D)//g' | sed 's/\$(DETECTORFILE)/${DETECTORFILE}/g' | sed 's/\$(PLASMAFILE)/${PLASMAFILE}/g' | sed 's/aladdin_subr\.o\ //g' |sed 's/^\(.*\)\ =\ \(.*\)$/list(APPEND \1_deps \2)/g' |sed 's/\.\$O/\.f/g' | sed 's/bspline.mod\ //g' |sed 's/bspline90_22\.o\ //g'  >> cmakecommands

# Generates list of targets from Makefile
grep "^.* = " Makefile|grep "\.\$O" | sed 's/\(.*\)\ \=\ .*$/\1/g' > targets

# Now take these targets and generate corresponding commands in CMakeLists.txt
for target in `cat targets`
do
   echo "foreach(str \${${target}_deps})"  >> cmakecommands
   echo "  string(REPLACE .f _mods modstr \"\${str}\")" >> cmakecommands
   echo "  list(APPEND ${target}_mods \"\${\${modstr}}\")" >> cmakecommands
   echo "endforeach()" >> cmakecommands
   echo "add_executable($target $target.f string.f sysdep.f \${${target}_mods} \${${target}_deps})" >> cmakecommands
   echo "target_link_libraries($target \${NETCDF_LIBRARIES})" >> cmakecommands
   echo " " >> cmakecommands
done

rm targets


