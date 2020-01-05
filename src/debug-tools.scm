(import chicken.format)

(define (tell-me prefix data)
  (display (format "~A: ~A" prefix data))
  (newline)
  data)

(define-syntax make-and-connect
  (syntax-rules ()
    ((_ port)
     (begin
       (define cl (make-mrpc-client 'tcp "localhost" port))
       (mrpc-connect! cl)))))
