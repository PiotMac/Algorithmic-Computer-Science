#!/bin/bash

declare -A occurences

function count_words_in_unique_files() {
    local folder="$1"
    folder=${folder%/}

    for file in $folder/*; do
        if [ -f "$file" ]; then
            # Using 'tr' command to position one word per one line
            # Using 'sort' command to sort all the words alphabetically
            # Using 'uniq' to remove duplicates in consecutive lines
            for word in $(cat $file | tr ' ' '\n' | sort | uniq); do
                if [ ${occurences["$word"]+exists} ]; then
                    ((occurences["$word"]++))
                else
                    ((occurences["$word"]=1))
                fi
            done
        elif [ -d "$file" ]; then
            count_words_in_unique_files "$file"
        fi
    done
}

count_words_in_unique_files $1

# Iterating over keys of the array
for word in ${!occurences[@]}; do
    printf "$word (${occurences[$word]})\n"
done
