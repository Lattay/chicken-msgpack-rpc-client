(import srfi-18)

(define (timestamp)
  (time->second (current-time)))
  
(define (sleep-til! time)
  (thread-sleep! (- time (timestamp))))

; allow locking a mutex for a given body but always unlocking it even in case of exception
; be careful not to use call/cc within the body, it may break everything
(define-syntax with-lock
  (syntax-rules ()
    ((with-lock lock body ...)
     (begin
       (mutex-lock! lock)
       (condition-case
         (let ((res (begin
                      body ...)))
           (mutex-unlock! lock)
           res)
         (e ()
            (begin
              (mutex-unlock! lock)
              (raise e))))))))

