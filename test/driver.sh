#!/bin/bash
compiler=${1:-./target/debug/rust-cc}
compiler_name=${2:-rust-cc}
tmp=`mktemp -d /tmp/rust-cc-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT
echo > $tmp/empty.c

check() {
    if [ $? -eq 0 ]; then
        echo "testing $1 ... passed"
    else
        echo "testing $1 ... failed"
        exit 1
    fi
}

# -o
rm -f $tmp/out
"$compiler" -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

# --help
"$compiler" --help 2>&1 | grep -q "$compiler_name"
check --help

echo OK
