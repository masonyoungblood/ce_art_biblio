#!/bin/bash

#create output file
OUTPUT="data/references.json"
echo "[" > "$OUTPUT"

#iterate through files
FIRST=1
for file in sources/*.pdf; do
    #run anystyle
    RESULT=$(anystyle find "$file")
    #remove leading/trailing whitespace
    RESULT=$(echo "$RESULT" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    #remove outer brackets if present (anystyle outputs arrays)
    RESULT=$(echo "$RESULT" | sed 's/^\[\(.*\)\]$/\1/')
    if [ -n "$RESULT" ]; then
        if [ $FIRST -eq 0 ]; then
            echo "," >> "$OUTPUT"
        fi
        echo "$RESULT" >> "$OUTPUT"
        FIRST=0
    fi
done

#send to output file
echo "]" >> "$OUTPUT"

#correct tabbing
jq . "$OUTPUT" > "${OUTPUT}.pretty" && mv "${OUTPUT}.pretty" "$OUTPUT"
