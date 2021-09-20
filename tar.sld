(define-library (tar)
  (export tar-content-type
          tar-content-type?
          tar-entry-name
          tar-entry-size
          read-tar-entry
          read-tar-entry-bytes
          make-tar-padding
          make-tar-eof
          make-tar-header-for-regular-file)
  (import (scheme base))
  (begin

    (define (align multiple value)
      (truncate-remainder (- multiple (truncate-remainder value multiple))
                          multiple))

    (define (bytevector-fold merge state bytes)
      (let loop ((state state) (i 0))
        (if (= i (bytevector-length bytes)) state
            (loop (merge (bytevector-u8-ref bytes i) state) (+ i 1)))))

    (define (bytevector-every? f bytes)
      (let loop ((i 0))
        (or (= i (bytevector-length bytes))
            (and (f (bytevector-u8-ref bytes i))
                 (loop (+ i 1))))))

    (define (read-exactly-n-bytes n)
      (let* ((bytex (read-bytevector n))
             (bytes (if (eof-object? bytex) (bytevector) bytex)))
        (if (< (bytevector-length bytes) n)
            (error "Short read / wanted / got" n (bytevector-length bytes))
            bytes)))

    ;;

    (define tar-content-type "application/x-tar")

    (define (tar-content-type? symbol)
      (eq? symbol (string->symbol tar-content-type)))

    ;;

    (define (tar-limit-exceeded)
      (error "tar limit exceeded"))

    (define (tar-string nbytes str)
      (let* ((bytes (string->utf8 str))
             (room (- nbytes (bytevector-length bytes))))
        (if (> room 0)
            (bytevector-append bytes (make-bytevector room 0))
            (tar-limit-exceeded))))

    (define (tar-entry-string-ref entry offset len)
      (let ((limit (let loop ((limit (+ offset len)))
                     (cond ((<= limit offset)
                            offset)
                           ((zero? (bytevector-u8-ref entry (- limit 1)))
                            (loop (- limit 1)))
                           (else
                            limit)))))
        (if (= limit (+ offset len))
            (error "tar: string is not null terminated")
            (utf8->string (bytevector-copy entry offset limit)))))

    (define (tar-octal nbytes value)
      (let* ((bytes (string->utf8 (number->string value 8)))
             (room (- nbytes (bytevector-length bytes))))
        (if (> room 0)
            (bytevector-append (make-bytevector (- room 1)
                                                (char->integer #\0))
                               bytes
                               (bytevector 0))
            (tar-limit-exceeded))))

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

    (define (tar-entry-name entry)
      (tar-entry-string-ref entry 0 100))

    (define (tar-entry-size entry)
      (tar-entry-octal-ref entry 124 12))

    (define (read-tar-entry)
      (let ((entry (read-exactly-n-bytes 512)))
        (if (bytevector-every? zero? entry) (eof-object) entry)))

    (define (read-tar-entry-bytes entry)
      (let* ((nbyte (tar-entry-size entry))
             (bytes (read-exactly-n-bytes nbyte))
             (nulls (read-exactly-n-bytes (align 512 nbyte))))
        bytes))

    (define (make-tar-padding bytes)
      (make-bytevector (align 512 (bytevector-length bytes)) 0))

    (define (make-tar-eof)
      (make-bytevector (* 512 2) 0))

    (define (make-tar-header-for-regular-file filename bytes)
      (let* ((before-checksum
              (bytevector-append
               (tar-string 100 filename)
               (tar-octal 8 #o444)
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
                           after-checksum)))))
