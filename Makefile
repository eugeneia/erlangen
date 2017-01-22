SOURCE_OBJECTS = $(shell find . -regex '[^\#]*\.lisp' -printf '%P ' \
			&& find . -regex '[^\#]*\.asd' -printf '%P ')

bin:
	mkdir bin
bin/erlangen-port-mapper: bin build/port-mapper.lisp $(SOURCE_OBJECTS)
	ccl -Q -b -n -l build/port-mapper.lisp
bin/erlangen-kernel: bin build/kernel.lisp $(SOURCE_OBJECTS)
	ccl -Q -b -n -l build/kernel.lisp
bin/erlangen-message-benchmark: bin build/message-benchmark.lisp $(SOURCE_OBJECTS)
	ccl -Q -b -n -l build/message-benchmark.lisp
bin/erlangen-ring-benchmark: bin build/ring-benchmark.lisp $(SOURCE_OBJECTS)
	ccl -Q -b -n -l build/ring-benchmark.lisp
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
