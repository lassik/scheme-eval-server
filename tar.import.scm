;;;; tar.import.scm - GENERATED BY CHICKEN 5.2.1 -*- Scheme -*-

(##sys#with-environment
  (lambda ()
    (scheme#eval
      '(import-syntax
         (only r7rs
               begin
               cond-expand
               export
               import
               import-for-syntax
               include
               include-ci
               syntax-rules)
         scheme.base))
    (import
      (only r7rs
            begin
            cond-expand
            export
            import
            import-for-syntax
            include
            include-ci
            syntax-rules))
    (##sys#register-compiled-module
      'tar
      'tar
      (scheme#list
        '(tar-entry-octal-ref . tar#tar-entry-octal-ref)
        '(tar-octal . tar#tar-octal)
        '(tar-entry-string-ref . tar#tar-entry-string-ref)
        '(tar-string . tar#tar-string)
        '(tar-limit-exceeded . tar#tar-limit-exceeded)
        '(read-exactly-n-bytes . tar#read-exactly-n-bytes)
        '(bytevector-every? . tar#bytevector-every?)
        '(bytevector-fold . tar#bytevector-fold)
        '(align . tar#align))
      '((tar-content-type . tar#tar-content-type)
        (tar-content-type? . tar#tar-content-type?)
        (tar-entry-name . tar#tar-entry-name)
        (tar-entry-size . tar#tar-entry-size)
        (read-tar-entry . tar#read-tar-entry)
        (read-tar-entry-bytes . tar#read-tar-entry-bytes)
        (make-tar-padding . tar#make-tar-padding)
        (make-tar-eof . tar#make-tar-eof)
        (make-tar-header-for-regular-file
          .
          tar#make-tar-header-for-regular-file))
      (scheme#list
        (scheme#cons
          '|\x04r7rstar|
          (##sys#er-transformer (##core#lambda (x r c) (##core#undefined)))))
      (scheme#list))))

;; END OF FILE