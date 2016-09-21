bin:
	mkdir bin
bin/erlangen-port-mapper: bin build/port-mapper.lisp
	ccl -Q -b -n -l build/port-mapper.lisp
bin/erlangen-kernel: bin build/kernel.lisp
	ccl -Q -b -n -l build/kernel.lisp
clean:
	rm -rf bin
.PHONY=clean
