(module mrpc-protocol (write-request
                       write-response
                       write-notification
                       read-message)

  (import scheme
          chicken.base
          chicken.condition
          chicken.format)

  (import (prefix (only msgpack pack unpack) mpk:))

  (define (raise-corrupted content)
    (signal
      (condition 
        `(exn location msgpack message ,(format "Corrupted message ~A." content))
        '(rpc)
        '(invalid))))

  (define (make-message vec)
    (if (not (vector? vec))
        (raise-corrupted vec))
    (case (vector-ref vec 0)
      ((0) ; 0 id method-name params
       (if (= (vector-length vec) 4)
           (cons 'request (cdr (vector->list vec)))
           (raise-corrupted vec)))
      ((1) ; 1 id error-obj result
       (if (= (vector-length vec) 4)
           (cons 'response (cdr (vector->list vec)))
           (raise-corrupted vec)))
      ((2) ; 2 method-name params
       (if (= (vector-length vec) 3)
           (cons 'notification (cdr (vector->list vec)))
           (raise-corrupted vec)))
      (else #f)))

  ;;;;;;;;;;; Public interface

  ; writer
  (define (write-request port id method-name params)
    (assert (and 'write-request (integer? id)))
    (assert (and 'write-request (output-port? port) (output-port-open? port)))
    (mpk:pack port (list 0 id method-name (if (null? params) #() params))))

  (define (write-notification port method-name params)
    (assert (and 'write-notification (output-port? port) (output-port-open? port)))
    (mpk:pack port (list 2 method-name (if (null? params) #() params))))

  (define (write-response port id method-name error-obj result)
    (assert (and 'write-notification (integer? id)))
    (assert (and 'write-notification (output-port? port) (output-port-open? port)))
    (mpk:pack port (list 1 id method-name error-obj result)))

  ; reader
  (define (read-message port)
    (assert (and 'read-message (input-port? port) (input-port-open? port)))
    (make-message (mpk:unpack port)))
  )
