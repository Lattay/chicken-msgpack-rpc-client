(include "src/mrpc-protocol.scm")

(module msgpack-rpc-client (*wait-cycle-length*
                           *multi-thread*

                           make-client
                           client?
                           connect!
                           close!
                           call!
                           async-call!
                           wait!
                           notify!
                           bind!
                           client-listen!

                           untangle-msg
                           )
 (import scheme
         chicken.base
         chicken.tcp
         chicken.condition)

 (import srfi-1
         srfi-69)

 (include "src/thread-tools.scm")

 (import (prefix mrpc-protocol mrpc:))

 (define-syntax alist-set!
   (syntax-rules ()
     ((_ alist key val)
      (set! alist (alist-cons key val alist)))))

 (define *wait-cycle-length* (make-parameter 0.05))
 (define *multi-thread* (make-parameter #f))

 (define (serialize-exception exn)
   ; TODO
   (list "error" "error serialization not implemented yet"))

 (define (make-client mode . args)
   (let ((in #f)
         (in-lock (make-mutex))
         (out #f)
         (out-lock (make-mutex))
         (req-table (make-hash-table))
         (req-table-lock (make-mutex))
         (method-table (make-hash-table))
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
                                       (hash-table-set! req-table id
                                                   (if (null? err)
                                                       (cons 'success result)
                                                       (cons 'error err))))))
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
                     (or (with-lock req-table-lock
                                    (let ((res (hash-table-ref/default req-table key #f)))
                                      (hash-table-delete! req-table key)
                                      res))
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
                           (set! res (apply (hash-table-ref method-table (first req))
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
              (list (cons 'req (hash-table->alist req-table))
                    (cons 'methods (hash-table->alist method-table))
                    (cons 'call-stack method-call-stack)))
             (else #f)))))))


 ;; internal "read all incoming messages"
 (define (%read-all! client)
   (when ((client 'listen))  ; messages incoming
     (%read-all! client)))
 
 ;; internal async call
 (define (%call! client mode method args)
   (%read-all! client)
   ((client 'call) mode method args))

 ;; internal wait
 (define (%wait! client key #!optional (timeout -1))
   (let ((res ((client 'wait) key timeout)))
     (values (cdr res) (car res))))

 ;; client predicate
 (define (client? client)
   (and
     (procedure? client)
     (client 'is-mrpc-client)))

 ;; connect to the server
 (define (connect! client)
   (assert (client? client))
   ((client 'connect)))

 ;; close the connection
 (define (close! client)
   (assert (client? client))
   ((client 'close)))

 ;; synchronously call a remote method and return the result
 (define (call! client method . args)
   (assert (client? client))
   (%wait! client (%call! client 'request method args)))

 ;; Asyncronously call a remote method.
 ;; The last argument can be a either a callback procedure or #f.
 ;; It return an object to be waited with wait.
 ;; The callback must be a two argument procedure: the result and the error object
 ;; if no error occured the error object is #f.
 ;; In any case wait will return #f for a callback based request.
 ;; If #f is provided instead of a callback, async-call return a Chicken promise.
 ;; The promise have to be waited with wait to retrieve the request result.
 ;; If *multi-thread* is #t the server response is treated as soon as possible.
 ;; If it is #f the server response is treated when wait is called.
 (define (async-call! client method . args)
   (assert (client? client))
   (let* ((rargs (reverse args))
          (cb (car rargs))
          (args (reverse (cdr rargs)))
          (key (%call! client 'request method args)))
     (assert (or (not cb) (procedure? cb)) "Last argument must be either #f or a procedure.")
     (cond
       ; callback + multithread
       ((and cb (*multi-thread*))
        (let ((thread
                (make-thread
                  (lambda ()
                    (let-values (((result status) (%wait! client key)))
                      (if (eq? status 'error)
                          (cb #f result)
                          (cb result #f)))))))
          (thread-start! thread)
          thread))
       ; callback + singlethread
       ((and cb (not (*multi-thread*)))
        (lambda ()
          (let-values (((result status) (%wait! client key)))
            (if (eq? status 'error)
                (cb #f result)
                (cb result #f)))))
       ; promise + multi-thread
       ((and (not cb) (*multi-thread*))
        (let* ((status #f)
               (result #f)
               (thread
                 (make-thread (lambda ()
                                (let-values (((lresult lstatus) (%wait! client key)))
                                  (set! status lstatus)
                                  (set! result lresult))))))
          (thread-start! thread)
          (delay (begin
                   (thread-join! thread)
                   (values result status)))))
       ; promise + singlethread
       ((and (not cb) (not (*multi-thread*)))
        (delay (%wait! client key))))))

 ;; Block until the completion of a request and eventually return the result
 ;; argument is the return value of async-call and can be:
 ;; - a Chicken promise to be forced (promise based call)
 ;; - a started SRFI-18 thread to be joined (callback based call)
 ;; - a standard thunk to be called (callback based call)
 ;; Anything else cause an error.
 ;; Result is returned only from promise based requests.
 ;; #f is returned for callback based requests.
 (define (wait! to-wait)
   (cond
     ((promise? to-wait) (force to-wait))
     ; callback based call:
     ((thread? to-wait) (thread-join! to-wait) (values #f #f))
     ((procedure? to-wait) (to-wait) (values #f #f))
     (else (error "Not a valid to-be-waited object."))))

 (define (notify! client method . args)
   (assert (client? client))
   (%call! client 'notification method args))

 ;; bind a procedure to a method-name to be called by server
 ;; any uncaught error will be reported to the server
 (define (bind! client method-name method)
   ((client 'bind) method-name method))

 ;; block the current thread to treat server-to-client requests and return when
 ;; no more server-to-client requests are waiting or when timeout second
 ;; (optional, default 0) ; are elapsed
 ;; If timeout is 0, treat one waiting request (if any) and return
 ;; return #f if stopped from timeout and #f if stopped from exaustion
 (define (client-listen! client #!optional (timeout 0))
   (let ((start (timestamp)))
     (%read-all! client)
     (if ((client 'listen))
       (let ((elapsed (- (timestamp) start)))
         (if (and (> timeout 0)
                  (> elapsed timeout))
             #f
             (client-listen! client (- timeout elapsed))))
       #t)))

 ;; Mostly unrelated but useful. Recursively convert all hash-tables to alist
 ;; and vectors to list so that message can be easily displayed and browsed.
 (define (untangle-msg thing)
  (cond
    ((hash-table? thing)
     (let loop ((rest (hash-table->alist thing)) (acc '()))
       (if (null? rest)
           (reverse acc)
           (loop (cdr rest) (cons (cons (caar rest) (untangle-msg (cdar rest))) acc)))))
    ((list? thing)
     (let loop ((rest thing) (acc '()))
       (if (null? rest)
           (reverse acc)
           (loop (cdr rest) (cons (untangle-msg (car rest)) acc)))))
    ((vector? thing)
     (untangle-msg (vector->list thing)))
    (else
      thing)))
 )
