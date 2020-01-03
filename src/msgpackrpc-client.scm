(include "mrpc-protocol.scm")

(module msgpackrpc-client (*wait-cycle-length*

                           make-mrpc-client
                           mrpc-client?
                           mrpc-connect
                           mrpc-close
                           mrpc-call
                           mrpc-async-call
                           mrpc-wait
                           mrpc-notify
                           mrpc-bind
                           mrpc-client-listen

                           mrpc-promise?
                           )

 (include "thread-tools.scm")
 (include "list-tools.scm")

 (import (prefix mrpc-protocol mrpc:))

 (define *wait-cycle-length* (make-parameter 0.05))

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
         (method-call-lock (make-mutex)))
     (let ((connect #f)
           (close #f)
           (call
             (lambda (method args)
               (with-lock req-table-lock
                 (alist-new!
                   (gen-key)
                   '()
                   req-table)))))

       (let ((wait
               (lambda (key)
                 (let wait-for ((v #f))
                   (with-lock req-table-lock
                     (set! v (alist-get-and-remove! key req-table)))
                   (if v
                       v
                       (let ((t (timestamp)))
                         (poll)
                         (sleep-til! (+ (*wait-cycle-length*) t))
                         (wait-for v))))))
             (bind
               (lambda (method-name method)
                 (alist-set! method-name method method-table)))
             (listen  ; TODO quand ça doit être appelé ??
               (lambda ()
                 (if (and
                       (input-port-open? in)
                       (char-ready? in))
                     (let ((msg (with-lock in-lock (mrpc:read in))))
                       (if msg
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
                                                         (mrpc:write-response out id method-name #f res))))))))))
                             ((response)
                              (let ((id (second msg))
                                    (method-name (third msg))
                                    (err (fourth msg))
                                    (result (fifth msg)))
                                (with-lock req-table-lock
                                  (alist-set!
                                    key
                                    (if err
                                        (cons 'error err)
                                        (cons 'success result))
                                    req-table))))
                             ((notification)
                              (let ((method-name (second msg))
                                    (args (third msg)))
                                (with-lock method-call-lock
                                  (set! method-call-stack
                                    (cons
                                      (list method-name args #f)
                                      method-call-stack)))))
                             'error) ; TODO
                           #f)))))
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
                       #t))))
                 )))


         (case mode
           ((tcp)
            )
           ((unix)
            )
           ((file)
            ))
         (list
           connect
           close
           call
           wait
           bind
           listen
           poll-calls)))))


 ; promise predicate
 (define (mrpc-promise? promise)
   (eq? (car promise) 'mrpc-promise))

 ; client predicate
 (define (mrpc-client? client)
   (eq? (first client 'mrpc-client)))

 ; connect to the server
 (define (mrpc-connect client)
   (assert (mrpc-client? client))
   ((second client)))

 ; close the connection
 (define (mrpc-close client)
   (assert (mrpc-client? client))
   ((third client)))

 ; synchronously call a remote method and return the result
 (define (mrpc-call client method . args)
   (assert (mrpc-client? client))
   (%mrpc-wait (%mrpc-call client method args)))

 ; internal async call
 (define (%mrpc-call client method args)
   ((fourth client) method args))

 ; internal wait
 (define (%mrpc-wait client key)
   ((fifth client) method args))

 ; asyncronously call a remote method. If a callback is provided as the last paramter
 ; it is call with the result as an argument as soon as the server respond.
 ; If #f is provided instead it return a promise. The call is
 (define (mrpc-async-call client method . args)
   (assert (mrpc-client? client))
   (let ((rargs (reverse args)))
     (let ((cb (car rargs))
           (args (reverse (cdr rargs))))
       (if cb
           ; mode callback
           (thread-start
             (lambda ()
               (let ((key (%mrpc-call client method args)))
                 (cb (%mrpc-wait key)))))
           ; mode promise
           (let ((key (%mrpc-call client method args)))
             (cons
               'mrpc-promise
               (lambda () (%mrpc-wait key))))))))


 (define (mrpc-wait promise)
   (assert (mrpc-promise? promise))
   ((cdr promise)))

 (define (mrpc-notify client method . args)
   (assert (mrpc-client? client))
   (%mrpc-call client method args))

 (define (mrpc-bind client method-name method)
   ((sixth client) method-name method))

 (define (mrpc-client-listen client #!optional (min-time 0))
   (let ((start (timestamp)))
     (when ((eighth client))
       (let ((elapsed (- (timestamp) start)))
         (if (and (> min-time 0)
                  (> elapsed min-time))
             #t
             (mrpc-client-listen client (- min-time elapsed)))))))
 )
