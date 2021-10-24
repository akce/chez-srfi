#! /bin/sh
#|
exec /usr/bin/env ${SCHEME:-scheme} --debug-on-exception --script "$0" "$@"
|#
;; Installs versions of srfi libs where calls to (include/resolve ...) have been inlined with requested scheme code.
;; These inlined libs are written to a separate install directory and compiled by Chez scheme.
;;
;; Inlining this way makes all referenced SRFI code compilable.
;;
;; Use from the top level dir of these srfi libs:
;; ./install.chezscheme.sps <dest-dir>
;;
;; The SRFI library will be installed under <dest-dir>. ie, <dest-dir>/srfi/...
;; <dest-dir> will be created if it does not exist.
;;
;; SPDX-License-Identifier: Unlicense

(import
  (chezscheme)
  (private translate-name)
  (private install))

(define usage
  (lambda ()
    (format #t "Usage:
  $ ~a <mode> [mode-args ...]

where <mode> is one of:

  links

  source <destination-dir>

      Where <destination-dir> is in the Chez scheme library search path, (library-directories).
      The SRFIs will be installed beneath <destination-dir>. ie, <destination-dir>/srfi
      The <destination-dir>/srfi directory must not exist.

  compile

" (car (command-line)))
    (exit 1)))

(define arg=? string=?)

(let ([argv (cdr (command-line))])
  (cond
    [(null? argv)
     (usage)]
    [(arg=? (car argv) "links")
     ;; create softlinks with decoded percent-encoded filenames.
     (link-files!)]
    [(arg=? (car argv) "source")
     ;; install source files to location
     (install-source! "." (cadr argv))]
    [(arg=? (car argv) "compile")
     ;; compile source at location
     (compile-source! (cadr argv) "./compile-all.chezscheme.sps")]
   [else
     (usage)]))

;; vi:ft=scheme:
