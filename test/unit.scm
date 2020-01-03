(import test)

(load "msgpackrpc-client.so")
(import msgpackrpc-client)

(test-group "Unit tests"
  (test "TODO" #t #f)
  (test-group "initialization"
    (test "tcp params" #t
          (mrpc-client?  (make-mrpc-client 'tcp "localhost" 8888)))
    (test "unix port params" #t
          (mrpc-client? (make-mrpc-client 'unix "/tmp/mrpc-port")))
    (test "file IO params" #t
          (mrpc-client? (make-mrpc-client 'file "/tmp/mrpc-io-file"))))

  (test-group "tcp usage"
    (define stop-srv (start-external-server 'tcp "localhost" 8888))
    (define client (make-mrpc-client 'tcp "localhost" 8888))
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

     
