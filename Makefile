# ============================================================================
# rust-cc test harness — mirrors chibicc's .c-file test suite.
#
# For each test:  test/<name>.c --(preprocess)--> rust-cc --(asm)--> gcc link
#                 --> test/<name>.exe --> run it; a non-zero exit = failure.
# ============================================================================

# Path to the compiler binary that `cargo build` produces.
RUST_CC=./target/debug/rust-cc

# Every C file under test/ is a test program. $(wildcard) expands the glob to:
#   test/arith.c test/control.c ... test/variable.c
TEST_SRCS=$(wildcard test/*.c)

# The executables we want, derived from the sources by rewriting .c -> .exe
# (test/arith.c -> test/arith.exe). Pure string substitution; no files renamed.
TESTS=$(TEST_SRCS:.c=.exe)

# `make build` / `make rebuild`: compile the rust-cc binary.
build:
	cargo build

rebuild:
	cargo clean
	cargo build

# Pattern rule: how to turn any test/<name>.c into a runnable test/<name>.exe.
# `%` is the stem (e.g. "arith"); in the recipe $* = stem, $@ = the target.
# Depends on `build` (so the compiler is always fresh) and the matching .c.
#
# Step 1 (line below): preprocess the .c with the system compiler ($(CC)) and
#   pipe the result into rust-cc to produce assembly.
#     -E preprocess only    -P no "# line" markers
#     -C keep comments      -o- write to stdout
#   This expands #include "test.h" and ASSERT(x,y) into assert(x, y, "y").
#   rust-cc reads source from stdin ("-") and writes asm to test/<name>.s.
# Step 2: assemble the .s and link it with test/common (which defines
#   assert()). -xc forces gcc to treat the extension-less `common` file as C.
test/%.exe: build test/%.c
	$(CC) -o- -E -P -C test/$*.c | $(RUST_CC) -o test/$*.s -
	$(CC) -o $@ test/$*.s -xc test/common

# For testing a single file:
# test/struct.exe: build test/struct.c
# 	$(CC) -o- -E -P -C test/struct.c | $(RUST_CC) -o test/struct.s -
# 	$(CC) -o test/struct.exe test/struct.s -xc test/common

# `make struct`: build and run just the struct.c test.
struct_test:
	$(CC) -o- -E -P -C test/struct.c | $(RUST_CC) -o test/struct.s -
	$(CC) -o struct.exe test/struct.s -xc test/common
	echo struct.exe; ./struct.exe || exit 1; echo;

local_test:
	cargo build
	$(RUST_CC) -o test.s test.c
	gcc -o test.exe test.s
	./test.exe

# `make test`: build every .exe (via the $(TESTS) prerequisites), run each,
# then run the CLI-level driver.
# $^ = all prerequisites (the .exe list). `|| exit 1` stops at the first
# failing test. `$$i` escapes `$i` so the shell expands it, not make.
test: $(TESTS)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh

# `make clean`: remove cargo artifacts, generated .s/.exe, temp + backup files.
clean:
	cargo clean
	rm -rf tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

# These are command names, not files to produce, so always run them even if a
# file of the same name happens to exist in the directory.
.PHONY: build rebuild test clean
