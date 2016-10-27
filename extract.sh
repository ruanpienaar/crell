#!/bin/bash
# $1 - DIR containing src/

# TODO: first get grapherl....
# grapherl, has been removed, getopt seems to cause an issue goanna.

mkdir png
for DIR in `ls $1`; do
    echo $DIR
    ./deps/grapherl/grapherl -m "$1"/"$DIR" "png/$DIR.png"
done