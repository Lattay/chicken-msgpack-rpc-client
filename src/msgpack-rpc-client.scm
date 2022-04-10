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
                            listen!

                            untangle-msg)
 (import scheme
         chicken.base
         chicken.tcp
         chicken.condition
         chicken.format)

 (import srfi-1
         srfi-69
         mailbox
         socket)

 (include "src/thread-tools.scm")

 (import (prefix mrpc-protocol mrpc:))

 (define *wait-cycle-length* (make-parameter 0.01))
 (define *multi-thread* (make-parameter #f))

 (define (make-error type msg)
   (case type
     ((no-such-method)
      (list "no-such-method" msg))
     ((app)
      (list "app-error" msg))
     (else
       (list "error" msg))))

 (define get-message (condition-property-accessor 'exn 'message))
 (define get-location (condition-property-accessor 'exn 'location))
 (define (serialize-exception exn)
   ; TODO
   (make-error
     'app
     (format "~A: ~A" (get-location exn) (get-message exn))))


 (define (make-client mode . args)
   (let ((in #f)
         (in-lock (make-mutex))
         (out #f)
         (out-lock (make-mutex))
         (req-table (make-hash-table))
         (req-table-lock (make-mutex))
         (method-table (make-hash-table))
         (method-call-stack (make-mailbox))
         (gen-id (let ((id-lock (make-mutex))
                       (id 0))
                   (lambda ()
                     (with-lock id-lock
                                (set! id (modulo (add1 id) 65536))
                                id)))))
     (let ((listen  ; check for pending messages and dispatch *one* of them (if any)
             (lambda ()
               (if (and
                     (input-port-open? in)
                     (char-ready? in))
                   (let ((msg (with-lock in-lock (mrpc:read-message in))))
                     (if (not msg)
                         (close-input-port in) ; end of file
                         (case (car msg)
                           ((request)  ; requests are pushed onto method-call-stack
                            (let ((id (second msg))
                                  (method-name (third msg))
                                  (args (vector->list (fourth msg))))
                              (mailbox-send!
                                method-call-stack
                                (list method-name  ; method to be called
                                      args  ; parameters
                                      (lambda (res err)  ; response callback
                                        (with-lock out-lock
                                                   (if err
                                                       (mrpc:write-response
                                                         out id method-name err #f)
                                                       (mrpc:write-response
                                                         out id method-name #f res))))))))
                           ((notification)  ; notification are pushed onto method-call-stack
                            (let ((method-name (second msg))
                                  (args (vector->list (third msg))))
                              (mailbox-send! method-call-stack (list method-name args #f))))
                           ((response)  ; response are stored in req-table to be waited
                            (let ((id (second msg))
                                  (err (third msg))
                                  (result (fourth msg)))
                              (with-lock req-table-lock
                                         (hash-table-set! req-table id
                                                          (if (null? err)
                                                              (cons 'success result)
                                                              (cons 'error err))))))
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
             (call  ; send a request or notification and return the id
               (lambda (mode method args)
                 (with-lock out-lock
                            (case mode
                              ((request)
                               (let ((id (gen-id)))
                                 (mrpc:write-request out id method args)
                                 id))
                              ((notification)
                               (mrpc:write-notification out method args))))))
             (wait  ; wait for the response to a specific request
               (lambda (key timeout)
                 (let ((start (timestamp)))
                   (let wait-for-it ()
                     ; either get the result from the req-table
                     ; or listen for a while and recurse
                     (or (with-lock req-table-lock
                                    (let ((res (hash-table-ref/default req-table key #f)))
                                      (hash-table-delete! req-table key)
                                      res))
                         (let ((t (timestamp)))
                           (if (or (not timeout) (< (- t start) timeout))
                               (begin
                                 (let repeat () ; read all available messages
                                   (when (listen)
                                     (repeat)))
                                 (sleep-til! (+ (*wait-cycle-length*) t))
                                 (wait-for-it))
                               #f)))))))
             (bind  ; register a method
               (lambda (method-name method)
                 (hash-table-set! method-table method-name method)))
             (handle-s2c  ; take a method-call from method-call-stack (if any),
               ; run it and (eventually) send the response to server
               (lambda (#!optional (blocking #f))
                 (if (and (not blocking) (mailbox-empty? method-call-stack))
                     #f  ; no more s2c pending
                     (let ((req (mailbox-receive! method-call-stack)))  ; first method pending
                       (let ((func (first req))
                             (args (second req))
                             (respond (third req))
                             (res #f) (err #f))
                         (if (hash-table-exists? method-table func)
                             (condition-case  ; run the method in controlled env
                               (set! res (apply (hash-table-ref method-table func) args))
                               (e () (set! err (serialize-exception e))))
                             (set! err (make-error 'no-such-method func)))
                         (when respond  ; send the result to server
                           (respond res err)))
                       #t)))))  ; return #t when a method have been handled
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
           ((unix)
            (assert (= (length args) 2))
            (let ((path (first args)))
              (set! connect
                (lambda ()
                  (let ((sock (socket-connect
                                (socket af/unix
                                        sock/stream))))
                    (socket-connect sock (unix-address path))
                    (let-values (((s-in s-o) (socket-i/o-ports sock)))
                      (set! in s-in)
                      (set! out s-out)
                      ))))))
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
             ((handle-s2c) handle-s2c)
             ((debug)
              (list (cons 'req (hash-table->alist req-table))
                    (cons 'methods (hash-table->alist method-table))
                    (cons 'call-stack method-call-stack)))
             (else #f)))))))


 ;; internal "read all incoming messages"
 ;; if timeout is provided it will stop when timeout seconds
 ;; have been elapsed
 ;; return #t when it stop from exhaustion
 ;; return #f when it stop from timeout
 (define (%read-all! client #!optional (timeout #f))
   (if timeout
       (if (> timeout 0)
           (let ((start (timestamp)))
             (if ((client 'listen))
                 (let ((elapsed (- (timestamp) start)))
                   (%read-all! client (- timeout elapsed)))
                 #t))
           #f)
       (if ((client 'listen))  ; messages incoming
           (%read-all! client)
           #t)))

 ;; internal async call
 (define (%call! client mode method args)
   ((client 'call) mode method args))

 ;; internal wait
 (define (%wait! client key #!optional (timeout #f))
   (let ((res ((client 'wait) key timeout)))
     (if res
      (values (cdr res) (car res))
      (values #f 'timeout))))

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
                          (cb result #f))
                      #t)))))
          (thread-start! thread)
          (lambda (timeout)
            thread)))
       ; callback + singlethread
       ((and cb (not (*multi-thread*)))
        (lambda (timeout)
          (lambda ()
            (let-values (((result status) (%wait! client key timeout)))
              (when (not (eq? status 'timeout))
                (if (eq? status 'error)
                    (cb #f result)
                    (cb result #f)))
              (values result status)))))

       ; promise + multi-thread
       ((and (not cb) (*multi-thread*))
        (let* ((status #f)
               (result #f)
               (thread (make-thread (lambda ()
                                      (let-values (((lres lstat) (%wait! client key)))
                                        (set! status lstat)
                                        (set! result lres)
                                        #t)))))
          (thread-start! thread)
          (lambda (timeout)
            (delay (if (thread-join! thread timeout #f)
                       (values result status)
                       (values #f 'timeout))))))
       ; promise + singlethread
       ((and (not cb) (not (*multi-thread*)))
        (lambda (timeout)
          (delay (%wait! client key timeout)))))))

 ;; Block until the completion of a request or timeout and eventually return the result
 ;; argument is the return value of async-call and can be:
 ;; - a Chicken promise to be forced (promise based call)
 ;; - a started SRFI-18 thread to be joined (callback based call)
 ;; - a standard thunk to be called (callback based call)
 ;; A specific use of wait! can be to pass it the client instance just to
 ;; force reading pending messages.
 ;; Anything else cause an error.
 ;; Result is returned only from promise based requests.
 ;; #f is returned for callback based requests.
 ;; status may be #f for callback based request, 'timeout if timeout have been reached while
 ;; waiting, 'error if an error have been raised in the server or 'success.
 ;; If client is passed, return #t if all available messages have been fetched
 ;; and #f if timeout have been reached before.
 ;;
 ;; If timeout is reached waiting for a callback (multi-thread or not),
 ;; it can be waited again, whereas promise can be waited only once.
 ;; Hence avoid using promise style with finite timeout or you will lost results
 ;; and risk memory leak (filling the pending responses result storage).
 ;;
 ;; Do not specify timeout or set it to #f to have infinite blocking.
 (define (wait! to-wait #!optional (timeout #f))
   (let ((to-wait (to-wait timeout)))
     (cond ((promise? to-wait) (force to-wait))
           ; callback based call:
           ((thread? to-wait) (if (thread-join! to-wait timeout #f) (values #f #f) (values #f 'timeout)))
           ((procedure? to-wait) (to-wait) (values #f #f))
           ((client? to-wait) (%read-all! to-wait timeout))
           (else (error "Not a valid to-be-waited object.")))))

 ;; send a notification to the server and imediatly return
 (define (notify! client method . args)
   (assert (client? client))
   (%call! client 'notification method args))

 ;; bind a procedure to a method-name to be called by server
 ;; any uncaught error will be reported to the server
 (define (bind! client method-name method)
   (assert (client? client))
   ((client 'bind) method-name method))

 ;; Non-lazy or
 (define (or* . args)
   (foldl (lambda (l r) (or r l)) #f args))

 ;; block the current thread to fetch server-to-client messages and return when
 ;; no more server-to-client messages are waiting or when timeout second
 ;; (optional, default 0) ; are elapsed
 ;; If timeout is 0, treat one waiting message (if any) and return
 ;; return #f if stopped from timeout and #t if stopped from exaustion
 ;; If optional handle-call is #t (default is #f) also run methods upon server
 ;; requests and notifications.
 (define (listen! client #!optional (blocking #f) (timeout #f))
   (assert (client? client))
   (if (and timeout (< timeout 0))
       #f
       (let ((start (timestamp)))
         (%read-all! client (if timeout (/ timeout 2) #f))  ; fetch pending messages until exhaustion (or timeout)
         (if ((client 'handle-s2c) blocking)
             (listen! client (- timeout (- (timestamp) start)))
             #t))))

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
