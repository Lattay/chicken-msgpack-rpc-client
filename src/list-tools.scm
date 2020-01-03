(import srfi-1)

(define-syntax alist-new!
  (syntax-rules ()
    ((alist-new! key datum alist)
     (set! alist (alist-cons key datum alist)))))

(define-syntax alist-get-and-remove!
  (syntax-rules ()
    ((alist-get-and-remove! key alist)
     (let ((v #f)
           (alist-val alist))
       (let loop ((acc '()) (rest alist-val))
         (if (null? rest)
             (set! alist (reverse acc))
             (if (eq? (caar alist-val) key)
                 (begin
                   (set! v (cdar rest))
                   (loop acc (cdr rest)))
                 (loop (cons acc (car rest)) (cdr rest))))))
     v)))

(define-syntax alist-set!
  (syntax-rules ()
    ((alist-set! key val alist)
     (let ((alist-val alist)
           (vkey key))
       (call-with-current-continuation
         (lambda (k)
           (let loop ((rest alist-val))
             (unless (null? rest)
               (if (equal? (caar rest) vkey)
                   (begin
                     (set-cdr! (car rest) val)
                     (k #t))
                   (loop (cdr rest)))))
           (set! alist (alist-cons vkey val alist-val))
           (k #f)))))))
