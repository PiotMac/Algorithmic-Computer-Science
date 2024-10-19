#!/bin/bash

function replace_letter() {
    local folder="$1"
    folder=${folder%/}

    for file in $folder/*; do
        if [ -f "$file" ]; then
            # Replacing in-place all the occurences of 'a' to 'A'
            # s - substitute, g - in the whole file, not just the first occurence
            sed -i 's/a/A/g' $file
        elif [ -d "$file" ]; then
            replace_letter "$file"
        fi
    done
}

replace_letter $1
