(import srfi-1)

(define-syntax alist-new!
  (syntax-rules ()
    ((_ key datum alist)
     (set! alist (alist-cons key datum alist)))))


(define (alist-get-and-remove key alist)
  (let loop ((acc '()) (rest alist))
    (if (null? rest)
        (values #f alist)
        (if (equal? (caar rest) key)
            (values (cdar rest) (append (reverse acc) (cdr rest)))
            (loop (cons (car rest) acc) (cdr rest))))))

(define-syntax alist-get-and-remove!
  (syntax-rules ()
    ((_ key alist)
     (let-values (((val new-alist) (alist-get-and-remove key alist)))
       (set! alist new-alist)
       val))))

(define (alist-set key val alist)
  (let loop ((rest alist) (acc '()))
    (if (null? rest)
        (reverse acc)
        (if (equal? (caar rest) key)
            (loop (alist-cons key val acc) (cdr rest))
            (loop (cons (car rest) acc) (cdr rest))))))

(define-syntax alist-set!
  (syntax-rules ()
    ((_ key val alist)
     (set! alist (alist-set key val alist)))))
