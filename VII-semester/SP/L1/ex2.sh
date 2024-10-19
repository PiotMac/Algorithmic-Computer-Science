#!/bin/bash

declare -A occurences

function count_words() {
    local folder="$1"
    folder=${folder%/}

    for file in $folder/*; do
        if [ -f "$file" ]; then
            for word in $(cat $file); do
                if [ ${occurences["$word"]+exists} ]; then
                    ((occurences["$word"]++))
                else
                    ((occurences["$word"]=1))
                fi
            done
        elif [ -d "$file" ]; then
            count_words "$file"
        fi
    done
}

count_words $1

# Iterating over keys of the array
for word in ${!occurences[@]}; do
    printf "$word (${occurences[$word]})\n"
done
