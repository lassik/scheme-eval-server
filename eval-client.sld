(define-library (eval-client)
  (import (srfi 1)
          (srfi 98)
          (scheme base)
          (scheme write)
          (uri-common)
          (only (intarweb) headers make-request)
          (http-client)
          (tar))
  (begin

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
             (write-string (utf8->string (read-tar-entry-bytes entry)))
             (newline)
             (loop))))))))
