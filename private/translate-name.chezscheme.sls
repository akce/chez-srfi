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

(library (private translate-name)
  (export translate-name)
  (import (chezscheme))

  (define (translate-name name)
    (let f ([i 0] [j 0])
      (if (fx= i (string-length name))
        (make-string j)
        (let ([c (string-ref name i)])
          (cond
            [(and (char=? c #\%)
                  (let ([next-i (fx+ i 3)])
                    (and (fx<= next-i (string-length name)) next-i))) =>
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
  )
