;; SPDX-License-Identifier: Unlicense
(library
  (private install)
  (export
    (rename (copy-srfi! install-source!))
    compile-source!)
  (import
    (chezscheme)
    (private translate-name))

  (define srfi-name?
    (lambda (f)
      (char=? #\% (string-ref f 0))))

  (define copy-file
    (lambda (src dest)
      (let* ([inp (open-file-input-port src)]
             [outp (open-file-output-port dest)]
             [contents (get-bytevector-all inp)])
        (unless (eof-object? contents)
          ;; Write nothing if the file is empty.
          (put-bytevector outp contents))
        (for-each close-port `(,inp ,outp)))))

  (define copy-directory
    (lambda (src dest)
      (let* ([tdest (join-path dest (translate-name src))]
             [dir-list (directory-list src)]
             [src-contents
               (map
                 (lambda (f)
                   (join-path src f))
                 dir-list)])
        (unless (file-exists? tdest)
          (make-dirs tdest))
        (for-each
          (lambda (source-path segment)
            (cond
              [(and (file-regular? source-path) (not (file-symbolic-link? source-path)))
               (copy-file source-path (join-path tdest segment))]
              [(file-directory? source-path)
               (copy-directory source-path dest)]))
          src-contents (map translate-name dir-list)))))

  ;; Join all string parts together using separator.
  ;; Naive implementation that uses (potentially) multiple calls to string-append.
  (define join-string
    (lambda (sep . str-parts)
      (cond
        [(null? str-parts)
         ""]
        [else
          (let loop ([acc (car str-parts)] [rest (cdr str-parts)])
            (cond
              [(null? rest)
               acc]
              [else
                (loop (string-append acc sep (car rest)) (cdr rest))]))])))

  (define join-path
    (let ([sep (string (directory-separator))])
      (lambda parts
        (apply join-string sep parts))))

  (define collect-library
    (lambda (dir)
      (filter srfi-name? (directory-list dir))))

  (define make-dirs
    (lambda (dir)
      (cond
        [(file-directory? dir)
         #t]
        [(or (string=? "" dir) (string=? "/" dir))
         #t]
        [else
          (make-dirs (path-parent dir))
          (mkdir dir)])))

  ;; [proc] directory-list/with-path: list directory contents with leading path.
  (define directory-list/with-path
    (lambda (dir)
      (define returner
        (cond
          [(string=? "." dir)
           ;; do not return paths with ./ prefix as this becomes a problem case for the import script that
           ;; 'compile-all' needs to generate.
           values]
          [else
            (lambda (f)
              (join-path dir f))]))
      (map		; list directory contents.
        returner
        (directory-list dir))))

  ;; Copy the source portions of srfi to the dest dir.
  ;; Almost equivalent to sh commands (but with added decoding of percent encoded filenames for destination):
  ;;   $ cp -r %3a* private dest-dir
  ;; Test files are also copied if copy-tests? is true (default is true).
  ;; ie,
  ;;   $ cp -r test dest-dir
  (define copy-srfi!
    (case-lambda
      [(src-dir dest-dir)
       (copy-srfi! src-dir dest-dir #t)]
      [(src-dir dest-dir copy-tests?)
       (define srfi-dest-dir (join-path dest-dir "srfi"))
       ;; TODO should merge rather than abort here.
       (cond
         [(file-exists? srfi-dest-dir)
          (error #f "SRFI destination directory exists. Please remove before running again." srfi-dest-dir)]
         [else
           (make-dirs srfi-dest-dir)])
       (let ([src-paths (collect-library src-dir)])
         (for-each
           (lambda (src)
             (cond
               [(file-regular? src)
                (copy-file src (join-path srfi-dest-dir (translate-name src)))]
               [(file-directory? src)
                (copy-directory src srfi-dest-dir)]
               [else
                 (error 'copy-srfi! "unsupported copy file type. must be a file or directory" src)]))
           src-paths)
         (copy-directory (join-path src-dir "private") srfi-dest-dir)
         (when copy-tests?
           (copy-directory (join-path src-dir "tests") srfi-dest-dir)))]))

  ;; Allow override of official srfi library-name for historical srfi's.
  ;; ie, only (srfi :115 regex) is converted to (srfi :115 regexp).
  ;; New srfi's should follow the standard.
  (define srfi-library-name
    (lambda (srfi-def)
      (case (car srfi-def)
        [(115)
         ;; (library-name regex)
         'regexp]
        [else
          (cadr srfi-def)])))

  ;; Generate an import list such that all locally exported libs are captured.
  (define make-import-all
    (lambda (srfi-list srfi-dir)
      `(import
         (rnrs)
         ,@(fold-left
             (lambda (acc srfi-def)
               (let* ([colon-str (string-append ":" (number->string (car srfi-def)))]
                      [colon-num (string->symbol colon-str)])
                 (cons
                   (list 'srfi colon-num)
                   ;; Not all srfi's have a directory (or named) implementation.
                   ;; eg, (srfi :175 ascii) is only available as (srfi :175) in chez-srfi.
                   ;; SRFI-97 defines neumonics as optional so there's no harm if missing.
                   (if (file-directory? (join-path srfi-dir colon-str))
                     (cons
                       (list 'srfi colon-num (srfi-library-name srfi-def))
                       acc)
                     acc))))
             '()
             (sort
               (lambda (a b)
                 (fx> (car a) (car b)))
               srfi-list)))))

  ;; Generate an import statement containing all the public libs defined by chez-srfi.
  ;; That list is then imported inline via 'eval'.
  ;; Doing it this way lets Chez scheme handle dependancies correctly and compile libs only once
  ;; and in the correct order.
  (define compile-source!
    (lambda (srfi-dir)
      (parameterize ([library-directories srfi-dir]
                     [compile-imported-libraries #t]
                     [generate-wpo-files #t])
        ;; Import after library-directories is set. That way we are sure to import the set of
        ;; implemented libs at srfi-dir.
        (import (only (srfi private registry-names) SRFIs))
        (eval (make-import-all SRFIs srfi-dir)))))

  )
