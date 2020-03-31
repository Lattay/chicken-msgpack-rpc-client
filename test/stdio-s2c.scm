(import scheme
        chicken.base
        chicken.process
        chicken.random)
(import test)

(import srfi-18)
(import (prefix msgpack-rpc-client mrpc:))

(define tell
  (let ((out (open-output-file "test.log")))
    (lambda (msg)
      (display msg out)
      (newline out))))

(define client (mrpc:make-client 'stdio))
(tell "start")
(mrpc:bind! client "hello" (lambda () (+ 5 4)))
(tell "bind")

(mrpc:connect! client)
(tell "connect")

(let loop ()
  (tell "listen")
  (when (mrpc:listen! client #f 1)
    (thread-sleep! 0.2))
  (loop))
