#!/bin/sh
echo "$0 mappings_file target_file" 1>&2
while read WORD HASH
do
	sed -i "s/$HASH/$WORD/g" $2
done < $1
