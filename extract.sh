#!/bin/bash

# $1 - DIR containing src/
# ~/code/project/apps/

for DIR in `ls $1`; do
    echo $DIR
    ./deps/grapherl/grapherl -m "$1"/"$DIR" $DIR.png
done