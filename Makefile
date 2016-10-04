bin:
	mkdir bin
bin/erlangen-port-mapper: bin build/port-mapper.lisp
	ccl -Q -b -n -l build/port-mapper.lisp
bin/erlangen-kernel: bin build/kernel.lisp
	ccl -Q -b -n -l build/kernel.lisp
test-agent: bin/erlangen-kernel
	bin/erlangen-kernel -Q -b -n \
	-e "(erlangen.agent-test:run-tests)" \
	-e "(ccl:quit)"
test-node: bin/erlangen-kernel
	bin/erlangen-kernel -Q -b -n \
	-e "(erlangen.distribution.protocol.node-test:run-tests)" \
	-e "(ccl:quit)"
test-port-mapper: bin/erlangen-kernel
	bin/erlangen-kernel -Q -b -n \
	-e "(erlangen.distribution.protocol.port-mapper-test:run-tests)" \
	-e "(ccl:quit)"
test: test-agent test-node test-port-mapper
clean:
	rm -rf bin
.PHONY: test-agent test-node test-port-mapper test clean
