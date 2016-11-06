;;;; Erlangen packages.

(defpackage erlangen.conditions
  (:documentation "Generic conditions for Erlangen.")
  (:use :cl)
  (:export :timeout))

(defpackage erlangen.mailbox
  (:documentation "Agent mailbox implementation.")
  (:use :cl
        :ccl
        :jpl-queues
        :erlangen.conditions)
  (:export :mailbox
           :make-mailbox
           :enqueue-message
           :enqueue-priority
           :empty-p
           :dequeue-message
           :close-mailbox
           :mailbox-messages-dequeued
           :mailbox-messages-dropped))

(defpackage erlangen.agent
  (:documentation
   "Node-local implementation of asynchronous _agents_ that can
  communicate via message passing. The interface is mostly equivalent to
  Erlang's processes except (1) without asynchronous exceptions and (2)
  without a blocking version of {send}.")
  (:use :cl
        :ccl
        :erlangen.mailbox)
  (:export :agent
           :agent-stats
           :agent-links
           :agent-monitors
           :spawn
           :add-link
           :remove-link
           :link
           :unlink
           :send
           :receive
           :exit
           :notify
           :*default-mailbox-size*
           :*agent-debug*))

(defpackage erlangen.registry
  (:documentation
   "Node-local agent name registry.")
  (:use :cl
        :ccl
        :erlangen.agent)
  (:export :register
           :unregister
           :registered
           :agent-by-name))

(defpackage erlangen.macros
    (:documentation "Erlangen core macros.")
    (:use :cl
          :ccl
          :erlangen.agent)
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
  (:export :send-hello
           :assert-protocol-version
           :write-error-reply
           :read-error-reply
           :write-ack-reply
           :read-ack-reply
           :protocol-error
           :make-socket*
           :with-socket
           :do-request))

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

(defpackage erlangen.distribution.id
  (:documentation "Universal agent identifiers.")
  (:use :cl
        :ccl
        :erlangen.agent
        :erlangen.registry
        :split-sequence
        :conspack)
  (:export :host-name
           :node-name
           :decode-id
           :agent-id
           :find-agent
           :reserve-id
           :claim-id
           :bind-id))

(defpackage erlangen.distribution.protocol.node
  (:documentation "Node portion of the distribution protocol.")
  (:use :cl
        :ccl
        :erlangen.agent
        :erlangen.macros
        :erlangen.distribution.call
        :erlangen.distribution.id
        :erlangen.distribution.protocol.buffers
        :erlangen.distribution.protocol.common
        :erlangen.distribution.protocol.port-mapper)
  (:export :make-node-server
           :clear-connections
           :remote-spawn
           :remote-send
           :remote-link
           :remote-unlink
           :remote-exit
           :remote-notify))

(defpackage erlangen
  (:documentation
   "Distributed, asychronous message passing system for Clozure Common Lisp.")
  (:use :cl
        :erlangen.agent
        :erlangen.registry
        :erlangen.conditions
        :erlangen.macros
        :erlangen.distribution.call
        :erlangen.distribution.id
        :erlangen.distribution.protocol.node
        :erlangen.distribution.protocol.port-mapper)
  ;; Some symbols with generic definitions (for local/registered/remote
  ;; agents) are redefined in this package and thus shadowed.
  (:shadow :send
           :exit
           :notify
           :link
           :unlink
           :spawn)
  (:export :agent
           :spawn
           :call
           :link
           :unlink
           :send
           :receive
           :timeout
           :exit
           :register
           :unregister
           :registered
           :select
           :node
           :*default-mailbox-size*
           :*agent-debug*))

(defpackage erlangen.management
  (:use :cl
        :ccl
        :erlangen.agent
        :erlangen.mailbox)
  (:export :agent-stats
           :agent-tree
           :root
           :linked
           :monitored
           :process-agent
           :flush-messages))
