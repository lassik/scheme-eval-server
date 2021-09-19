(import (srfi 1)
        (srfi 98)
        (scheme base)
        (scheme write)
        (http-client)
        (intarweb)
        (uri-common))

(define tar-content-type "application/x-tar")

(define (read-exactly-n-bytes n)
  (let ((bytes (read-bytevector n)))
    (if (= n (bytevector-length bytes)) bytes (error "Short read"))))

(define (tar-entry-octal-ref entry offset len)
  (let loop ((offset offset) (len len) (value 0))
    (if (<= len 0) value
        (let ((dig0 (char->integer #\0))
              (dig7 (char->integer #\7))
              (byte (bytevector-u8-ref entry offset)))
          (loop (+ offset 1) (- len 1)
                (if (<= dig0 byte dig7)
                    (let ((digit (- byte dig0)))
                      (+ digit (* value 8)))
                    value))))))

(define (make-tar-eof)
  (make-bytevector (* 512 2) 0))

(define (tar-entry-size entry)
  (tar-entry-octal-ref entry 124 12))

(define (bytevector-every? f bytes)
  (let loop ((i 0))
    (or (= i (bytevector-length bytes))
        (and (f (bytevector-u8-ref bytes i))
             (loop (+ i 1))))))

(define (read-tar-entry)
  (let ((entry (read-exactly-n-bytes 512)))
    (if (bytevector-every? zero? entry) (eof-object) entry)))

(define (align multiple value)
  (truncate-remainder (- multiple (truncate-remainder value multiple))
                      multiple))

(define (read-tar-entry-bytes entry)
  (let* ((nbyte (tar-entry-size entry))
         (bytes (read-exactly-n-bytes nbyte))
         (nulls (read-exactly-n-bytes (align 512 nbyte))))
    bytes))

(with-input-from-request
 (make-request
  method: 'POST
  uri: (uri-reference "http://localhost:3000/gauche")
  headers: (headers `((content-type ,tar-content-type)
                      (accept ,tar-content-type))))
 (lambda ()
   (write-string (utf8->string (make-tar-eof))))
 (lambda ()
   (let loop ()
     (let ((entry (read-tar-entry)))
       (unless (eof-object? entry)
         (read-tar-entry-bytes entry)
         (loop))))
   (display "Success")
   (newline)))
