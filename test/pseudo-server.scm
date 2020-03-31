(import msgpack-rpc-client)

(define client (make-client 'tcp "127.0.0.1" 8000))
(connect! client)
(let-values (((res stat) (call! client "hello")))
  (write res)
  (newline)
  (write stat)
  (newline))
