bin/erlangen-port-mapper: build/port-mapper.lisp
	ccl -Q -b -n -l build/port-mapper.lisp
bin/erlangen-kernel: build/kernel.lisp
	ccl -Q -b -n -l build/kernel.lisp
