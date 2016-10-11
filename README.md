# Erlangen

Distributed, asynchronous message passing system for Clozure Common Lisp.

_Warning: Erlangen is immature, experimental software, subject to bugs and
changes._

## Documentation

 * [Manual and API Documentation](http://mr.gy/software/erlangen/api.html)


## Dependencies

 * [trivia](https://github.com/guicho271828/trivia)
 * [jpl-queues](http://www.thoughtcrime.us/software/jpl-queues/)
 * [fast-io](https://github.com/rpav/fast-io)
 * [cl-conspack](https://github.com/conspack/cl-conspack)
 * [trivial-utf-8](https://common-lisp.net/project/trivial-utf-8/)
 * [split-sequence](https://github.com/sharplispers/split-sequence)

## Getting Started

First, make sure you have [Clozure Common Lisp](http://ccl.clozure.com/)
installed. Then clone the Erlangen repository to a place where
[ASDF](https://common-lisp.net/project/asdf/) can find it. Also, make sure ASDF
can find Erlangen’s dependencies.

If you use [Quicklisp](https://www.quicklisp.org/) you can place Erlangen into
the `quicklisp/local-projects` directory and have it fetch the dependencies
automatically.

You can now start Clozure Common Lisp and load Erlangen:

```
(require :asdf) ; Not necessary if you use Quicklisp
(asdf:load-system :erlangen)
(use-package :erlangen)
```

### Programming a Parallelized Map with Asynchronous Agents and Message Passing

Let’s jump straight into a practical example. The Erlangen repository contains
the `erlangen.example` package which implements `parallel-map`, a parallelized
*map* function. Its like Common Lisp’s `map` over a single vector, only that it
spreads the work across multiple concurrent *agents* (fancy processes/native
threads).

`parallel-map` returns the results of calling a *function* on the elements in a
*vector* in a new vector. It uses up to *level* agents to parallelize the
workload. Optionally, a *result-type* can be specified as the element type of
the result vector. Here is roughly how `parallel-map` works:

  1. it spawns some worker agents and attaches to them in *monitor* mode, so
     that it will receive an exit notification for each worker
  2. it sends each worker agent a message with a chunk of work
  3. it waits for and receives the exit notification of each worker, which
     contains the chunk’s result, and inserts them into the final result vector

The worker agents initially do nothing at all. They each just wait to receive a
function to execute, and quit when they are done.

```
(defun worker ()
  (ematch (receive)
    ((and (type function) function)
     (funcall function))))
```

Eventually, each worker will receive a `map-thunk` closure, which maps the
mapping *function* over a chunk of the *vector* bounded by *start* and *end*.

```
(defun map-chunk (function vector start end result-type)
  (lambda ()
    (let ((results (make-array (- end start) :element-type result-type)))
      (loop for i from start below end do
           (setf (aref results (- i start))
                 (funcall function (aref vector i))))
      (values start end results))))
```

Now let’s look at `parallel-map`. To distribute the work, it computes

 - *length*—the length of our input *vector*
 - *n-chunks*—the number of chunks we will divide the work up into
 - *chunk-size*—the minimum length (in elements) of a chunk

and spawns a worker agent for each chunk.

```
(defun parallel-map (function vector &key (level 2) (result-type t))
  (let* ((length (length vector))
         (n-chunks (min level length))
         (chunk-size (floor length n-chunks))
         (workers (loop for i from 1 to n-chunks collect
                       (spawn 'worker :attach :monitor))))
```

Next it sends each worker a closure for the chunk it should process. It
divides the work into *n-chunks* intervals of at least *chunk-size* length,
that fully cover the *vector*.

```
    (loop for worker in workers
          for chunk from 1
          for start from 0 by chunk-size
          for end = (if (< chunk n-chunks)
                        (+ start chunk-size)
                        length)
       do (send (map-chunk function vector start end result-type) worker))
```

Finally it allocates a vector to store the results in, and waits to receive
each chunk result. If any worker exits unexpectedly, `parallel-map` exits with
that workers exit reason. Again, because we attached to the worker agents in
*monitor* mode, all remaining workers will also receive the exit signal and
shut down.

```
    (loop with results = (make-array length :element-type result-type)
          for worker in workers do
         (ematch (receive)
           ((list (type agent) :ok start end chunk-result)
            (replace results chunk-result :start1 start :end1 end))
           ((list (type agent) :exit reason)
            (exit reason)))
       finally (return results))))
```

Now we can spawn `parallel-map` agents like this

```
(spawn '(parallel-map 1+ #(2 4 6 8 10 12 14) :level 3) :attach :monitor)
(receive)
→ (#<AGENT #x302002A191ED> :OK #(3 5 7 9 11 13 15))
```

### Getting Distributed

What fun are agents if they aren’t distributed over a network? Erlangen comes
with support for distribution via TCP/IP built in. Each instance of Erlangen
can act as a *node*, and talk to other Erlangen nodes. But first, there needs
to be a *port mapper* on the network for nodes to find each other. To build and
run the Erlangen port mapper, execute the commands

```
make bin/erlangen-port-mapper
bin/erlangen-port-mapper localhost &
```

in a shell in the root of the Erlangen repository. Now build the Erlangen kernel
in the same way to help quickly start additional Erlangen instances, and use it
to start a node named *map-node*.

```
make bin/erlangen-kernel
bin/erlangen-kernel -n -e '(node :name "map-node")'
```

Hint: if you use Emacs, you can start a new Erlangen instance with Slime with
*C-u M-x slime RET /path/to/erlangen-kernel*.

For the following example to work your hostname as reported by
`machine-instance` must resolve to the local host. You might need to edit your
`/etc/hosts` file and add a line like this one (your mileage may vary):

```
127.0.0.1 <hostname>
```

With that out of the way, we can also make our initial Erlangen instance a
node, and offload some work to *map-node*:

```
(spawn '(node))
(spawn '(erlangen.examples:parallel-map 1+ #(2 4 6 8 10 12 14))
       :attach :monitor
       :node "map-node")
(receive)
→ ("perth/map-node/0" :OK #(3 5 7 9 11 13 15))
```

What happened? We spawned an agent on the remote *map-node* instance to run
`parallel-map`, and received its exit notification transparently over the
network.

