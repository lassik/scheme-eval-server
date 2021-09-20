(define-library (eval-server)
  (import (srfi 1)
          (srfi 13)
          (srfi 98)
          (scheme base)
          (scheme write)
          (only (chicken blob) blob->string)
          (only (srfi 4) u8vector->blob)
          (spiffy)
          (intarweb)
          (uri-common)
          (tar))
  (begin

    (define implementations
      '(("gauche" '("gosh" "-r" "7"))))

    (define implementation-name first)

    (define (respond-with-error status string)
      (send-response status: status body: string))

    (define (bytevector->string bytes)
      (blob->string (u8vector->blob bytes)))

    (define (tar-for-each proc)
      (let loop ()
        (let ((entry (read-tar-entry)))
          (unless (eof-object? entry)
            (proc entry)
            (loop)))))

    (define (parse-null-terminated-strings bytes)
      (let loop ((a 0) (b 0) (strings '()))
        (if (= b (bytevector-length bytes))
            (if (= a b)
                (reverse strings)
                (error "Missing final null terminator"))
            (if (zero? (bytevector-u8-ref bytes b))
                (loop (+ b 1) (+ b 1) (cons (utf8->string
                                             (bytevector-copy bytes a b))
                                            strings))
                (loop a (+ b 1) strings)))))

    (define (handle-the-scheme-implementation impl)
      (let ((cmdline #f)
            (environ #f)
            (cwd (string->utf8 "/"))
            (stdin #f))
        (parameterize ((current-input-port (request-port (current-request))))
          (tar-for-each
           (lambda (entry)
             (let* ((name (tar-entry-name entry))
                    (bytes (read-tar-entry-bytes entry)))
               (write name)(newline)
               (cond ((or (equal? "proc/" name)
                          (equal? "proc/self/" name)
                          (equal? "proc/self/fd/" name))
                      #f)
                     ((equal? "proc/self/cmdline" name)
                      (set! cmdline (parse-null-terminated-strings bytes)))
                     ((equal? "proc/self/environ" name)
                      (set! stdin (parse-null-terminated-strings bytes)))
                     ((equal? "proc/self/cwd" name)
                      (set! stdin (utf8->string bytes)))
                     ((equal? "proc/self/fd/0" name)
                      (set! stdin bytes))
                     ((string-prefix? "proc/self/fd/" name)
                      (error "Bad proc/self/fd/ on input" name))
                     ((string-prefix? "proc/" name)
                      (error "Bad /proc entry on input")))))))
        (write `(cmdline ,cmdline))(newline)
        (write `(environ ,environ))(newline)
        (write `(cwd ,cwd))(newline)
        (write `(stdin ,stdin))(newline)
        (send-response
         status: 'ok
         headers: `((content-type ,tar-content-type))
         body: (bytevector->string
                (let ((bytes (string->utf8
                              (string-append "Hello "
                                             (implementation-name impl)))))
                  (bytevector-append
                   (bytevector-append
                    (make-tar-header-for-regular-file "proc/self/fd/1" bytes)
                    bytes
                    (make-tar-padding bytes))
                   (make-tar-eof)))))))

    (define (handle-scheme-implementation)
      (let ((path (uri-path (request-uri (current-request))))
            (head (request-headers (current-request))))
        (write (header-value 'content-type head))(newline)
        (write (header-value 'accept head))(newline)
        (if (not (and (tar-content-type? (header-value 'content-type head))
                      (tar-content-type? (header-value 'accept head))))
            (respond-with-error 'bad-request "Not tar files")
            (let* ((impl-name (second path))
                   (impl (assoc impl-name implementations)))
              (if (not impl)
                  (respond-with-error 'not-found
                                      "No such Scheme implementation")
                  (handle-the-scheme-implementation impl))))))

    (define (handle-request continue)
      (let ((path (uri-path (request-uri (current-request)))))
        (write path)(newline)
        (if (and (= 2 (length path))
                 (eq? '/ (first path))
                 (string? (second path)))
            (handle-scheme-implementation)
            (continue))))

    (define (main)
      (vhost-map `(("localhost" . ,handle-request)))
      (server-port
       (string->number (or (get-environment-variable "PORT")
                           (error "No PORT"))))
      (start-server))

    (main)))
