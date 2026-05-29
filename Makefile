build:
	cargo build

rebuild:
	cargo clean
	cargo build

test:
	cargo build
	./test.sh
	./test-driver.sh

clean:
	cargo clean
	rm -f  *.o *~ tmp*

.PHONY: test clean
