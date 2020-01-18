(import test)
(import chicken.process
        chicken.random)

(import srfi-18)

(load "msgpack-rpc-client.so")
(import msgpack-rpc-client)

(define tcp-port (+ 9000 (pseudo-random-integer 999)))
(display "port used ")
(display tcp-port)
(newline)

(define (pythoncmd . args)
  (process-run (foldl (lambda (l r) (string-append l " " r)) "python3" args)))

(define (start-external-server mode . args)
  (case mode
    ((tcp)
     (let ((pid (pythoncmd "test/tcp-mpack-server.py" (number->string tcp-port))))
       (lambda () (process-run (string-append "kill -15 " (number->string pid))))))
    (else
      (error "Not implemented yet."))))

(test-group "Integration tests"
  (test-group "initialization"
    (test "tcp params" #t
          (mrpc-client?  (make-mrpc-client 'tcp "localhost" tcp-port)))
    ; (test "file IO params" #t
    ;       (mrpc-client? (make-mrpc-client 'file "/tmp/mrpc-io-file")))
    )

  (test-group "tcp usage"
    (define stop-srv (start-external-server 'tcp "localhost" tcp-port))
    (sleep 1)
    (define client (make-mrpc-client 'tcp "localhost" tcp-port))
    (test "connect" #t (mrpc-connect! client))
    (test "call" 46 (mrpc-call! client "sum" 36 10))
    (test-group "async call with promise"
      (let ((promise (mrpc-async-call! client "prod" 6 7 #f)))
        (test "result" 42 (mrpc-wait! promise))))
    (test-group "async call with callback"
      (test "single thread call" 55
            (let ((val #f))
              (let ((prom (mrpc-async-call! client "prod" 11 5
                                            (lambda (res err)
                                              (if err
                                                  (set! val err)
                                                  (set! val res))))))
                (mrpc-wait! prom)
                val)))
      (*multi-thread* #t)
      (test "multiple thread call" 55
            (let ((val #f))
              (let ((prom (mrpc-async-call! client "prod" 11 5
                                            (lambda (res err)
                                              (if err
                                                  (set! val err)
                                                  (set! val res))))))
                (thread-sleep! 0.5)
                val)))
      )
    (test-group "edge cases"
      (test "empty arg list" 42 (mrpc-call! client "answer"))
      (test "null result" '() (mrpc-call! client "i_dont_know" 1 2 3))
      (test "call raise error" 'error
            (let-values (((result status) (mrpc-call! client "dont_call_me" 1 2 3)))
              status))
      (test "method does not exist" 'error
            (let-values (((result status) (mrpc-call! client "does_not_exists" 3)))
              status))
      )

    (stop-srv))
  )
