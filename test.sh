#!/bin/bash

#Must be executed at the project dir root
TARGET=compilou.exe
TEST_FILE=tests/tmp
make

if [ ! $? ]; then
    printf "La compilation a echoue !\n"
    exit 1
fi

for file in tests/*.c; do
    ./$TARGET "$file"
    /bin/spim -f "${file%.c}.s" > "$TEST_FILE"
    /bin/tail -n +6 "$TEST_FILE" > "$TEST_FILE.tmp"
    /bin/cat "$TEST_FILE.tmp" > "$TEST_FILE"
    diff "$TEST_FILE" "${file%.c}.test"
    if [ ! $? ]; then
        rm "$TEST_FILE" "$TEST_FILE.tmp" tests/*.s
        printf "Le test ${test/#file} a echoue !\n"
        exit 1
    fi
done

rm "$TEST_FILE" "$TEST_FILE.tmp" tests/*.s
printf "Tous les tests sont passes !\n"
exit 0

