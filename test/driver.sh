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
"$compiler" -c -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

# --help
"$compiler" --help 2>&1 | grep -q "$compiler_name"
check --help

# -S
echo 'int main() {}' | "$compiler" -S -o - - | grep -q 'main:'
check -S

# Default output file
rm -f $tmp/out.o $tmp/out.s
echo 'int main() {}' > $tmp/out.c
(cd $tmp; "$OLDPWD/$compiler" -c out.c)
[ -f $tmp/out.o ]
check 'default object file'

rm -f $tmp/out.s
(cd $tmp; "$OLDPWD/$compiler" -S out.c)
[ -f $tmp/out.s ]
check 'default assembly file'

# Multiple input files
rm -f $tmp/foo.o $tmp/bar.o
echo 'int x;' > $tmp/foo.c
echo 'int y;' > $tmp/bar.c
(cd $tmp; "$OLDPWD/$compiler" -c $tmp/foo.c $tmp/bar.c)
[ -f $tmp/foo.o ] && [ -f $tmp/bar.o ]
check 'multiple object files'

rm -f $tmp/foo.s $tmp/bar.s
(cd $tmp; "$OLDPWD/$compiler" -S $tmp/foo.c $tmp/bar.c)
[ -f $tmp/foo.s ] && [ -f $tmp/bar.s ]
check 'multiple assembly files'

# Link one source from stdin.
rm -f $tmp/foo
echo 'int main() { return 0; }' | "$compiler" -o $tmp/foo -
$tmp/foo
check 'link stdin'

# Compile and link multiple source files.
rm -f $tmp/foo
echo 'int bar(); int main() { return bar(); }' > $tmp/foo.c
echo 'int bar() { return 42; }' > $tmp/bar.c
"$compiler" -o $tmp/foo $tmp/foo.c $tmp/bar.c
$tmp/foo
[ "$?" = 42 ]
check 'link multiple files'

# Default linked output is a.out.
rm -f $tmp/a.out
echo 'int main() {}' > $tmp/foo.c
(cd $tmp; "$OLDPWD/$compiler" foo.c)
[ -f $tmp/a.out ]
check a.out

echo OK
