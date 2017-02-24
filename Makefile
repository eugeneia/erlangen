SOURCE_OBJECTS = $(shell find . -regex '[^\#]*\.lisp' -printf '%P ' \
			&& find . -regex '[^\#]*\.asd' -printf '%P ')

all: bin/erlangen-port-mapper bin/erlangen-kernel \
	bin/erlangen-message-benchmark
bin:
	mkdir -p bin
bin/erlangen-port-mapper: build/port-mapper.lisp $(SOURCE_OBJECTS) | bin
	ccl -Q -b -n \
		-l build/load-quicklisp.lisp \
		-l build/port-mapper.lisp
bin/erlangen-kernel: build/kernel.lisp $(SOURCE_OBJECTS) | bin
	ccl -Q -b -n \
		-l build/load-quicklisp.lisp \
		-l build/kernel.lisp
bin/erlangen-message-benchmark: build/message-benchmark.lisp \
				$(SOURCE_OBJECTS) | bin
	ccl -Q -b -n \
		-l build/load-quicklisp.lisp \
		-l build/message-benchmark.lisp
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
.PHONY: all test-agent test-node test-port-mapper test clean
