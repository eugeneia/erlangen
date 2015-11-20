;;;; Erlangen packages.

(defpackage erlangen.mailbox
  (:documentation "Agent mailbox implementation.")
  (:use :cl
        :jpl-queues
        :bordeaux-threads)
  (:export :mailbox
           :make-mailbox
           :enqueue-message
           :empty-p
           :dequeue-message
           :close-mailbox
           :mailbox-full
           :mailbox-closed))

(defpackage erlangen.conditions
  (:documentation "Generic conditions for Erlangen.")
  (:use :cl)
  (:export :send-error
           :timeout))

(defpackage erlangen.algorithms
  (:documentation "Reusable algorithms.")
  (:use :cl)
  (:export :with-poll-timeout
           :with-poll-select))

(defpackage erlangen.agent
  (:documentation
   "Node-local implementation of asynchronous _agents_ that can
  communicate via message passing. The interface is mostly equivalent to
  Erlang's processes except (1) without asynchronous exceptions and (2)
  without a blocking version of {send}.")
  (:use :cl
        :erlangen.mailbox
        :erlangen.conditions
        :erlangen.algorithms
        :bordeaux-threads)
  (:shadowing-import-from :erlangen.conditions :timeout)
  (:export :*agent*
           :agent
           :spawn
           :link
           :unlink
           :send
           :receive
           :exit
           :*default-mailbox-size*
           :*agent-debug*))

(defpackage erlangen.registry
  (:documentation
   "Node-local agent name registry.")
  (:use :cl
        :erlangen.agent
        :bordeaux-threads)
  (:export :register
           :unregister
           :registered
           :agent-by-name))

(defpackage erlangen.macros
    (:documentation "Erlangen core macros.")
    (:use :cl
          :erlangen.agent
          :erlangen.algorithms)
    (:export :select))

(defpackage erlangen.distribution.call
  (:documentation
   "Portable function call objects.")
  (:use :cl)
  (:export :call
           :make-function))

(defpackage erlangen.distribution.protocol.buffers
  (:documentation
   "DSL for defining wire formats for protocol messages.")
  (:use :cl
        :trivial-utf-8
        :fast-io
        :conspack)
  (:export :define-message
           :read-message
           :read-message*
           :value))

(defpackage erlangen.distribution.protocol.common
  (:documentation "Common distribution protocol subset.")
  (:use :cl
        :ccl
        :erlangen.distribution.protocol.buffers)
  (:export :*i/o-timeout*
           :with-connect
           :with-listen
           :connect
           :find-listening-socket
           :send-hello
           :assert-protocol-version
           :write-error-reply
           :read-error-reply
           :write-ack-reply
           :read-ack-reply))

(defpackage erlangen.distribution.protocol.port-mapper
  (:documentation "Port mapper portion of the distribution protocol.")
  (:use :cl
        :ccl
        :erlangen.agent
        :erlangen.macros
        :erlangen.distribution.protocol.buffers
        :erlangen.distribution.protocol.common)
  (:export :port-mapper
           :register-node
           :query-node-port
           :query-nodes))

(defpackage erlangen
  (:documentation
   "Distributed asychronous message passing system for Common Lisp.")
  (:use :cl
        :erlangen.agent
        :erlangen.registry
        :erlangen.conditions
        :erlangen.macros
        :erlangen.distribution.call)
  ;; Some symbols with generic definitions (for local/registered/remote
  ;; agents) are redefined in this package and thus shadowed.
  (:shadow :send
           :exit
           :link
           :unlink
           :spawn)
  (:export :agent
           :spawn
           :call
           :link
           :unlink
           :send
           :send-error
           :receive
           :timeout
           :exit
           :register
           :unregister
           :registered
           :select
           :*default-mailbox-size*
           :*agent-debug*))
