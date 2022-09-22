#!/usr/bin/env bash


DIR="$( cd "$( dirname "$0" )" && pwd )"
cd ${DIR}

GREEN='\033[0;32m'
NC='\033[0m'

TOTAL_TESTS=1

pass_count=0

# Test 1, ~misc/nand2tetris/projects/11/Seven/Main.jack
file1="./SevenMain.vm"

cargo run ~/misc/nand2tetris/projects/11/Seven/Main.jack
mv ./Main.vm ./SevenMainGen.vm
file2="./SevenMainGen.vm"

if cmp -s "$file1" "$file2"; then
    let pass_count=(pass_count+1);
    printf "${GREEN}Test1 - Seven.jack passed.${NC}\n"
fi

echo "----------------------------------------"
printf "Test Summary: [${pass_count}/${TOTAL_TESTS}] passed."
