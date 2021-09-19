(import (srfi 1)
        (srfi 98)
        (scheme base)
        (scheme write)
        (only (chicken blob) blob->string)
        (only (srfi 4) u8vector->blob)
        (spiffy)
        (intarweb)
        (uri-common))

(define implementations
  '(("gauche" '("gosh" "-r" "7"))))

(define implementation-name first)

(define tar-content-type "application/x-tar")

(define (tar-content-type? symbol)
  (eq? symbol (string->symbol tar-content-type)))

(define (tar-limit-exceeded)
  (error "tar limit exceeded"))

(define (tar-string nbytes str)
  (let* ((bytes (string->utf8 str))
         (room (- nbytes (bytevector-length bytes))))
    (if (> room 0)
        (bytevector-append bytes (make-bytevector room 0))
        (tar-limit-exceeded))))

(define (tar-octal nbytes value)
  (let* ((bytes (string->utf8 (number->string value 8)))
         (room (- nbytes (bytevector-length bytes))))
    (if (> room 0)
        (bytevector-append (make-bytevector (- room 1) (char->integer #\0))
                           bytes (bytevector 0))
        (tar-limit-exceeded))))

(define (bytevector-fold merge state bytes)
  (let loop ((state state) (i 0))
    (if (= i (bytevector-length bytes)) state
        (loop (merge (bytevector-u8-ref bytes i) state) (+ i 1)))))

(define (make-tar-header-for-regular-file filename bytes)
  (let* ((before-checksum
          (bytevector-append
           (tar-string 100 filename)
           (tar-octal 8 #x444)
           (tar-octal 8 0)
           (tar-octal 8 0)
           (tar-octal 12 (bytevector-length bytes))
           (tar-octal 12 0)))
         (after-checksum
          (bytevector-append
           (bytevector (char->integer #\space))
           (bytevector (char->integer #\0))
           (tar-string 100 "")
           (tar-string 6 "ustar")
           (string->utf8 "00")
           (tar-string 32 "root")
           (tar-string 32 "root")
           (tar-octal 8 0)
           (tar-octal 8 0)
           (tar-string 155 "")
           (make-bytevector 12 0)))
         (blank-checksum
          (make-bytevector 7 (char->integer #\space)))
         (checksum
          (truncate-remainder (+ (bytevector-fold + 0 before-checksum)
                                 (bytevector-fold + 0 blank-checksum)
                                 (bytevector-fold + 0 after-checksum))
                              (expt 8 6))))
    (bytevector-append before-checksum
                       (tar-octal 7 checksum)
                       after-checksum)))

(define (align multiple value)
  (truncate-remainder (- multiple (truncate-remainder value multiple))
                      multiple))

(define (make-tar-padding bytes)
  (make-bytevector (align 512 (bytevector-length bytes)) 0))

(define (make-tar-eof)
  (make-bytevector (* 512 2) 0))

(define (respond-with-error status string)
  (send-response status: status body: string))

(define (bytevector->string bytes)
  (blob->string (u8vector->blob bytes)))

(define (handle-the-scheme-implementation impl)
  (send-response
   status: 'ok
   body: (bytevector->string
          (let ((bytes (string->utf8
                        (string-append "Hello " (implementation-name impl)))))
            (bytevector-append
             (bytevector-append
              (make-tar-header-for-regular-file "proc/fd/1" bytes)
              bytes
              (make-tar-padding bytes))
             (make-tar-eof))))))

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
              (respond-with-error 'not-found "No such Scheme implementation")
              (handle-the-scheme-implementation impl))))))

(define (handle-request continue)
  (let ((path (uri-path (request-uri (current-request)))))
    (write path)(newline)
    (if (and (= 2 (length path))
             (eq? '/ (first path))
             (string? (second path)))
        (handle-scheme-implementation)
        (continue))))

(vhost-map `(("localhost" . ,handle-request)))
(server-port
 (string->number (or (get-environment-variable "PORT") (error "No PORT"))))
(start-server)
