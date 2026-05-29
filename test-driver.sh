#!/bin/sh
rust_cc=./target/debug/rust-cc
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
$rust_cc -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

# --help
$rust_cc --help 2>&1 | grep -q rust-cc
check --help

echo OK
