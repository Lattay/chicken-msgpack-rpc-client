(include "src/mrpc-protocol.scm")

(module msgpack-rpc-client (*wait-cycle-length*
                           *multi-thread*

                           make-mrpc-client
                           mrpc-client?
                           mrpc-connect!
                           mrpc-close!
                           mrpc-call!
                           mrpc-async-call!
                           mrpc-wait!
                           mrpc-notify!
                           mrpc-bind!
                           mrpc-client-listen!
                           )
 (import scheme
         chicken.base
         chicken.tcp
         chicken.condition)

 (include "src/thread-tools.scm")
 (include "src/list-tools.scm")

 (import (prefix mrpc-protocol mrpc:))

 (define *wait-cycle-length* (make-parameter 0.05))
 (define *multi-thread* (make-parameter #f))

 (define (serialize-exception exn)
   ; TODO
   (list "error" "error serialization not implemented yet"))

 (define (make-mrpc-client mode . args)
   (let ((in #f)
         (in-lock (make-mutex))
         (out #f)
         (out-lock (make-mutex))
         (req-table '())
         (req-table-lock (make-mutex))
         (method-table '())
         (method-call-stack '())
         (method-call-lock (make-mutex))
         (gen-id (let ((id-lock (make-mutex))
                       (id 0))
                   (lambda ()
                     (with-lock id-lock
                                (set! id (modulo (add1 id) 65536))
                                id)))))
     (let ((listen
             (lambda ()
               (if (and
                     (input-port-open? in)
                     (char-ready? in))
                   (let ((msg (with-lock in-lock (mrpc:read-message in))))
                     (when msg
                       (case (car msg)
                         ((request)
                          (let ((id (second msg))
                                (method-name (third msg))
                                (args (fourth msg)))
                            (with-lock method-call-lock
                                       (set! method-call-stack
                                         (cons
                                           (list
                                             method-name
                                             args
                                             (lambda (res err)
                                               (with-lock out-lock
                                                          (if err
                                                              (mrpc:write-response out id method-name err #f)
                                                              (mrpc:write-response out id method-name #f res)))))
                                           method-call-stack)))))
                         ((response)
                          (let ((id (second msg))
                                (err (third msg))
                                (result (fourth msg)))
                            (with-lock req-table-lock
                                       (alist-new! id
                                                   (if (null? err)
                                                       (cons 'success result)
                                                       (cons 'error err))
                                                   req-table))))
                         ((notification)
                          (let ((method-name (second msg))
                                (args (third msg)))
                            (with-lock method-call-lock
                                       (set! method-call-stack
                                         (cons
                                           (list method-name args #f)
                                           method-call-stack)))))
                         (else (error "unrecognized message type")))) ; TODO
                     #t)  ; a message was received
                   #f)))) ; no message was received
       (let ((connect #f)
             (close
               (lambda ()
                 (with-lock in-lock
                            (close-input-port in))
                 (with-lock out-lock
                            (close-output-port out))))
             (call
               (lambda (mode method args)
                 (with-lock out-lock
                            (case mode
                              ((request)
                               (let ((id (gen-id)))
                                 (mrpc:write-request out id method args)
                                 id))
                              ((notification)
                               (mrpc:write-notification out method args))))))
             (wait
               (lambda (key timeout)
                 (let ((start (timestamp)))
                   (let wait-for-it ()
                     (or (with-lock req-table-lock (alist-get-and-remove! key req-table))
                         (let ((t (timestamp)))
                           (if (>= (- t start) timeout 0)
                               #f
                               (begin
                                 (let repeat () ; read all available messages
                                   (when (listen)
                                     (repeat)))
                                 (sleep-til! (+ (*wait-cycle-length*) t))
                                 (wait-for-it)))))))))
             (bind
               (lambda (method-name method)
                 (alist-set! method-name method method-table)))
             (poll-calls
               (lambda ()
                 (mutex-lock! method-call-lock)
                 (if (null? method-call-stack)
                     (begin
                       (mutex-unlock! method-call-lock)
                       #f)
                     (let ((req (car method-call-stack)))
                       (set! method-call-stack (cdr method-call-stack))
                       (mutex-unlock! method-call-stack)
                       (let ((res #f) (err #f))
                         (condition-case
                           (set! res (apply (alist-ref (first req) method-table equal?)
                                            (second req)))
                           (e ()
                              (set! err (serialize-exception e))))
                         (when (third req)
                           ((third req) res err)))
                       #t)))))
         (case mode
           ((tcp)
            (assert (= (length args) 2))
            (let ((host (first args))
                  (port (second args)))
              (set! connect
                (lambda ()
                  (let-values (((l-in l-out) (tcp-connect host port)))
                    (set! in l-in)
                    (set! out l-out)
                    #t)))))
           ((file)
            )
           ((extend)
            (assert (= (length args) 1))
            (set! connect
              (lambda ()
                (let-values (((l-in l-out) ((car args))))
                  (set! in l-in)
                  (set! out l-out)))))
           (else
             (error "Unsupported transport mode.")))
         (lambda (method)
           (case method
             ((is-mrpc-client) #t)
             ((connect) connect)
             ((close) close)
             ((call) call)
             ((wait) wait)
             ((bind) bind)
             ((listen) listen)
             ((pooll-calls) poll-calls)
             ((debug)
              (list (cons 'req req-table)
                    (cons 'methods method-table)
                    (cons 'call-stack method-call-stack)))
             (else #f)))))))


 ; internal "read all incoming messages"
 (define (%mrpc-read-all! client)
   (when ((client 'listen))  ; messages incoming
     (%mrpc-read-all! client)))
 
 ; internal async call
 (define (%mrpc-call! client mode method args)
   (%mrpc-read-all! client)
   ((client 'call) mode method args))

 ; internal wait
 (define (%mrpc-wait! client key #!optional (timeout -1))
   ((client 'wait) key timeout))

 ; client predicate
 (define (mrpc-client? client)
   (and
     (procedure? client)
     (client 'is-mrpc-client)))

 ; connect to the server
 (define (mrpc-connect! client)
   (assert (mrpc-client? client))
   ((client 'connect)))

 ; close the connection
 (define (mrpc-close! client)
   (assert (mrpc-client? client))
   ((client 'close)))

 ; synchronously call a remote method and return the result
 (define (mrpc-call! client method . args)
   (assert (mrpc-client? client))
   (%mrpc-wait! client (%mrpc-call! client 'request method args)))

 ; Asyncronously call a remote method.
 ; The last argument can be a either a callback procedure or #f.
 ; It return an object to be waited with mrpc-wait.
 ; The callback must be a two argument procedure: the result and the error object
 ; if no error occured the error object is #f.
 ; In any case mrpc-wait will return #f for a callback based request.
 ; If #f is provided instead of a callback, mrpc-async-call return a Chicken promise.
 ; The promise have to be waited with mrpc-wait to retrieve the request result.
 ; If *multi-thread* is #t the server response is treated as soon as possible.
 ; If it is #f the server response is treated when mrpc-wait is called.
 (define (mrpc-async-call! client method . args)
   (assert (mrpc-client? client))
   (let* ((rargs (reverse args))
          (cb (car rargs))
          (args (reverse (cdr rargs)))
          (key (%mrpc-call! client 'request method args)))
     (assert (or (not cb) (procedure? cb)) "Last argument must be either #f or a procedure.")
     (cond
       ((and cb (*multi-thread*))
        (let ((thread
                (make-thread
                  (lambda ()
                    (let ((raw (%mrpc-wait! client key)))
                      (if (eq? (car raw) 'error)
                          (cb #f (cdr raw))
                          (cb (cdr raw) #f)))))))
          (thread-start! thread)
          thread))
       ((and cb (not (*multi-thread*)))
        (lambda ()
          (let ((raw (%mrpc-wait! client key)))
            (if (eq? (car raw) 'error)
                (cb #f (cdr raw))
                (cb (cdr raw) #f)))))
       ((and (not cb) (*multi-thread*))
        (let* ((res #f)
               (thread
                 (make-thread (lambda () (set! res (%mrpc-wait! client key))))))
          (thread-start! thread)
          (delay (begin (thread-join! thread) res))))
       ((and (not cb) (not (*multi-thread*)))
        (delay (%mrpc-wait! client key))))))

 ; Block until the completion of a request and eventually return the result
 ; argument is the return value of mrpc-async-call and can be:
 ; - a Chicken promise to be forced
 ; - a started SRFI-18 thread to be joined
 ; - a standard thunk to be called
 ; Anything else cause an error.
 ; Result is returned only from promise based requests.
 ; #f is returned for callback based requests.
 (define (mrpc-wait! to-wait)
   (cond
     ((promise? to-wait)
        (force to-wait))
     ((thread? to-wait)
      (thread-join! to-wait))
     ((procedure? to-wait)
      (to-wait))
     (else
       (error "Not a valid to-be-waited object."))))

 (define (mrpc-notify! client method . args)
   (assert (mrpc-client? client))
   (%mrpc-call! client 'notification method args))

 ; bind a procedure to a method-name to be called by server
 ; any uncaught error will be reported to the server
 (define (mrpc-bind! client method-name method)
   ((client 'bind) method-name method))

 ; block the current thread to treat server-to-client requests and return when
 ; no more server-to-client requests are waiting or when timeout second
 ; (optional, default 0) ; are elapsed
 ; If timeout is 0, treat one waiting request (if any) and return
 ; return #f if stopped from timeout and #f if stopped from exaustion
 (define (mrpc-client-listen! client #!optional (timeout 0))
   (let ((start (timestamp)))
     (%mrpc-read-all! client)
     (if ((client 'listen))
       (let ((elapsed (- (timestamp) start)))
         (if (and (> timeout 0)
                  (> elapsed timeout))
             #f
             (mrpc-client-listen! client (- timeout elapsed))))
       #t)))
 )
