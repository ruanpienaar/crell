#!/bin/bash
# $1 - DIR containing src/
mkdir png
for DIR in `ls $1`; do
    echo $DIR
    ./deps/grapherl/grapherl -m "$1"/"$DIR" "png/$DIR.png"
done