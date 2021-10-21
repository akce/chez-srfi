#! /usr/bin/env scheme-script
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

(import
  (chezscheme)
  (private translate-name))

;;; Link all of the SRFIs to their normal directories like sane 
;;; people who use Chez Scheme prefer. :-)

(define (link-files!)
  (let file-loop ([ls (directory-list (current-directory))])
    (unless (null? ls)
      (let ([name (car ls)])
        (let ([translated-name (translate-name name)])
          (unless (or (string=? name translated-name)
                      (file-exists? translated-name))
            (system (format "ln -sf '~a' '~a'" name translated-name)))
          (when (file-directory? translated-name)
            (parameterize ([current-directory translated-name])
              (link-files!)))
          (file-loop (cdr ls)))))))

(link-files!)
