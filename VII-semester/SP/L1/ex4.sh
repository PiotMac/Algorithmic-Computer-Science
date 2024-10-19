#!/bin/bash

declare -A occurrences

function find_word_occurrences() {
    local folder="$1"
    folder=${folder%/}

    for file in "$folder"/*; do
        if [ -f "$file" ]; then
            declare -A current_file
            for word in $(cat $file); do
                if [ ! ${current_file["$word"]+exists} ]; then
                    occurrences["$word"]+=" $file"
                    current_file["$word"]="sth"
                fi
            done
            unset current_file
        elif [ -d "$file" ]; then
            find_word_occurrences "$file"
        fi
    done
}

find_word_occurrences $1

for word in "${!occurrences[@]}"; do
    printf "$word:\n"
    for file in ${occurrences["$word"]}; do
        grep -w -n -- "$word" "$file" | while IFS= read -r line; do
            printf "    $file:$line\n"
        done
    done
    printf "\n"
done
