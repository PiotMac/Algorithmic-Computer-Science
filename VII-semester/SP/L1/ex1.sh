#!/bin/bash

function show_regular_files() {
    local folder="$1"
    folder=${folder%/}

    for file in $folder/*; do
        if [ -f "$file" ]; then
            printf "$file\n"
        elif [ -d "$file" ]; then
            show_regular_files "$file"
        fi
    done
}

show_regular_files $1
