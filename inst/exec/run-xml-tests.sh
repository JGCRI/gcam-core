#!/bin/zsh

## Run the xml tests on all of the files in the newxml directory,
## using files in the oldxml directory as a comparison.

newdir=$1
olddir=$2

cmdstatus=0

xml_cmp=`Rscript -e 'cat(system.file("exec/xml_verify.py", package="gcamdata"))'`

cd $newdir

for file1 in ./**/*.xml; do
    fn=`basename $file1`
    ## We can't be sure there is only one match, so test against all
    for file2 in $olddir/**/$fn; do
	## Compare the old on the left to the new on the right
	echo "*** oldfile: $file2    newfile: $file1"
	$xml_cmp $file2 $file1
	rslt=$?
	if [ rslt != "0" ]; then
	   cmdstatus=$rslt
	fi
	   
    done
done

if [ cmdstatus != "0" ]; then
    echo "One or more comparison failures."
fi

exit $cmdstatus

