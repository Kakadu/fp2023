#!/usr/bin/env bash
if [ "$#" -eq "0" ]; then
    echo "No file specified"
    exit 1
fi

FILENAME=$1
LC=$(wc -l $FILENAME | cut -f1 -d' ')
# echo $LC

if [ "$LC" -gt 5 ]; then
    echo "TOOMANY"
elif [ "$LC" -gt 1 ]; then
    echo "FEW"
else
    echo "OK"
fi