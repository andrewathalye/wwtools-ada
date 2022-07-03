#!/bin/sh
echo "extract_unmapped.sh INPUT_FILE" 1>&2
grep -oE "[0-9]{5,}" $1
