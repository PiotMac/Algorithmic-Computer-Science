#!/bin/bash

declare -A lines_for_word

function find_duplicate_word_occurrences() {
    local folder="$1"
    folder=${folder%/}

    for file in "$folder"/*; do
        if [ -f "$file" ]; then
            counter=0
            while IFS= read -r line; do
                ((counter++))
                declare -A word_count
                for word in $line; do
                    ((word_count["$word"]++))
                done
                
                for word in "${!word_count[@]}"; do
                    if [ ${word_count["$word"]} -gt 1 ]; then
                        lines_for_word["$word"]+="$file:$counter:$line"$'\n'
                    fi
                done

                unset word_count
            done < "$file"
        elif [ -d "$file" ]; then
            find_duplicate_word_occurrences "$file"
        fi
    done
}

find_duplicate_word_occurrences $1

for word in "${!lines_for_word[@]}"; do
    printf "$word:\n"
    IFS=$'\n'
    for line in ${lines_for_word["$word"]}; do
        printf "    $line\n"
    done
    printf "\n"
    
    # Reset IFS to default after use
    unset IFS
done
