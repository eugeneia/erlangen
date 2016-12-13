#!/bin/bash
# Integration tests for distribution layer

# run instrumentation node in backround and remember PID
bin/erlangen-kernel -b -n -l test/instrumentation-node.lisp &
inode_pid=$!

# run the test
bin/erlangen-kernel -b -n -l test/dist-integration.lisp

# clean up
kill $inode_pid
