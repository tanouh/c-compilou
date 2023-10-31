#!/bin/bash

#Must be executed at the project dir root
TARGET=compilou.exe
TEST_FILE=tests/tmp
FAILED_TEST=""
RED='\x1B[31m'
GREEN='\x1B[32m'
CLEAR='\x1B[m'

make

if [ $? -ne 0 ]; then
    printf "La compilation a echoue !\n"
    exit 1
fi

function check_error {
    /bin/grep "ERROR" "$1" >/dev/null
    if [ $? -ne 0 ]; then
        file="$1"
        printf "$RED%s$CLEAR\n" "[FAILED]"
        FAILED_TEST=$file,$FAILED_TEST
    else
        printf "$GREEN%s$CLEAR\n" "[DONE]"
    fi
    return 0
}

for file in tests/*.c; do
    printf "[TEST] $file..."
    ./$TARGET "$file" &>/dev/null
    if [ $? -ne 0 ]; then
        check_error "${file%.c}.test"
        continue
    fi
    /bin/spim -f "${file%.c}.s" >"$TEST_FILE"
    /bin/tail -n +6 "$TEST_FILE" >"$TEST_FILE.tmp"
    /bin/cat "$TEST_FILE.tmp" >"$TEST_FILE"
    diff "$TEST_FILE" "${file%.c}.test" >"${file%.c}.log"
    if [ $? -ne 0 ]; then
        /bin/cat "$TEST_FILE" >>"${file%.c}.log"
        /bin/cat "${file%.c}.s" >>"${file%.c}.log"
        rm "$TEST_FILE" "$TEST_FILE.tmp" tests/*.s
        printf "$RED%s$CLEAR\n" "[FAILED]"
        FAILED_TEST=$file,$FAILED_TEST
    else
        printf "$GREEN%s$CLEAR\n" "[DONE]"
    fi

done

printf "\n"
if [ $FAILED_TEST ]; then
    IFS=,
    for file in $FAILED_TEST; do
        printf "Le test $file a echoue \n"
    done
    rm "$TEST_FILE" "$TEST_FILE.tmp" tests/*.s 2>/dev/null
    exit 1
fi
rm "$TEST_FILE" "$TEST_FILE.tmp" tests/*.s 2>/dev/null
printf "Tous les tests sont passes !\n"
exit 0
