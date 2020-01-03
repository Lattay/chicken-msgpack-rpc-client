(import test)
(import chicken.process
        chicken.random)

(load "msgpack-rpc-client.so")
(import msgpack-rpc-client)

(define tcp-port (+ 9000 (pseudo-random-integer 999)))

(define (pythoncmd . args)
  (process-run (foldl (lambda (l r) (string-append l " " r)) "python3" args)))

(define (start-external-server mode . args)
  (case mode
    ((tcp)
     (pythoncmd "./tcp-mpack-server.py" tcp-port))
    (else
      (error "Not implemented yet."))))

(test-group "Integration tests"
  (test-group "initialization"
    (test "tcp params" #t
          (mrpc-client?  (make-mrpc-client 'tcp "localhost" tcp-port)))
    (test "unix port params" #t
          (mrpc-client? (make-mrpc-client 'unix "/tmp/mrpc-port")))
    (test "file IO params" #t
          (mrpc-client? (make-mrpc-client 'file "/tmp/mrpc-io-file"))))

  (test-group "tcp usage"
    (define stop-srv (start-external-server 'tcp "localhost" tcp-port))
    (define client (make-mrpc-client 'tcp "localhost" tcp-port))
    (test "connect" #t (mrpc-connect client))
    (test "call" 46 (mrpc-call client "prod" 6 7))
    (test-group "async call with promise"
      (let ((promise (mrpc-async-call client "prod" 6 7 #f)))
        (test "promise" (mrpc-promise? promise))
        (test "result" (mrpc-wait promise))))
    (test-group "async call with callback"
      (let ((param (let ((val #f) (lambda args (if (null? args) val (set! val (car args))))))))
        (test "call" (mrpc-async-call client "prod" 6 7 (lambda (res) 

                (stop-srv)

     
