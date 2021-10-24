#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; translate-name has the below Copyright Notice:
;;; Copyright (c) 2012 Aaron W. Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.

(library (srfi private include)
  (export 
    include/resolve)
  (import 
    (except (rnrs) read)
    (for (only (rnrs mutable-strings) string-set!) expand)	; translate-name
    (for (srfi private include compat) expand)
    (for (srfi private include read) expand))
  
  (define-syntax include/resolve
    (lambda (stx)
      (define (include/lexical-context ctxt filename)
        (with-exception-handler
          (lambda (ex)
            (raise
             (condition
              (make-error)
              (make-who-condition 'include/resolve)
              (make-message-condition "error while trying to include")
              (make-irritants-condition (list filename))
              (if (condition? ex) ex (make-irritants-condition (list ex))))))
          (lambda ()
            (call-with-input-file filename
              (lambda (fip)
                (let loop ((a '()))
                  (let ((x (read fip)))
                    (if (eof-object? x)
                      (cons #'begin (datum->syntax ctxt (reverse a)))
                      (loop (cons x a))))))))))
      ;; translate-name: Copyright (c) 2012 Aaron W. Hsu <arcfide@sacrideo.us>
      ;; Define translate-name inline. We cannot import the lib as that is
      ;; defined in the 'install' scope, ie: (import (private translate-name)).
      ;; This could be removed if all the srfi libs were within an srfi subdir.
      ;; ie, if install and srfi's used the same srfi namespace:
      ;;   (import (srfi private translate-name))
      (define (translate-name name)
        (let f ([i 0] [j 0])
          (if (fx=? i (string-length name))
            (make-string j)
            (let ([c (string-ref name i)])
              (cond
                [(and (char=? c #\%)
                      (let ([next-i (fx+ i 3)])
                        (and (fx<=? next-i (string-length name)) next-i))) =>
                 (lambda (next-i)
                   (let ([translated-name (f next-i (fx+ j 1))])
                     (string-set! translated-name j
                                  (integer->char
                                    (string->number
                                      (substring name (fx+ i 1) next-i) 16)))
                     translated-name))]
                [else
                  (let ([translated-name (f (fx+ i 1) (fx+ j 1))])
                    (string-set! translated-name j c)
                    translated-name)])))))
      (syntax-case stx ()
        ((ctxt (lib-path* ...) file-path)
         (for-all (lambda (s) (and (string? s) (positive? (string-length s)))) 
                  (syntax->datum #'(lib-path* ... file-path)))
         (let ((p (apply string-append 
                         (map (lambda (ps) (string-append "/" ps)) 
                              (syntax->datum #'(lib-path* ... file-path)))))
               (sp (search-paths)))
           (let loop ((search sp))
             (if (null? search)
               (error 'include/resolve "cannot find file in search paths"
                      (substring p 1 (string-length p)) sp)
               (let* ((full (string-append (car search) p))
                      (xname (translate-name full)))
                 (cond
                   [(file-exists? full)
                    (include/lexical-context #'ctxt full)]
                   [(file-exists? xname)
                    ;; Fallback to searching within pre-translated paths.
                    (include/lexical-context #'ctxt xname)]
                   [else
                     (loop (cdr search))])))))))))
)
