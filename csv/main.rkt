#lang racket

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco doc <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

(provide read-csv write-csv csv-debug)

;; Code here

;; debug
(define csv-debugging #f)
(define (csv-debug toggle)
  (set! csv-debugging toggle)
  )
(define (log msg . arguments)
  (when csv-debugging
        (begin
          (apply printf (cons msg arguments))
          (printf "~n")
          )))

(define (write-csv filename data
                   #:field_delimiter [fd #\,]
                   #:text_delimiter [td #\"])
  (define (csv-writer f)
    (define (row-writer row)
      (define size (length row))
      (for ([i size])
           (define data (list-ref row i))
           (define quote-string (string td))
           (define quote-escape (format "~a~a" td td))
           (define escaped (string-replace (format "~a" data)
                                           quote-string quote-escape
                                           #:all? #t))
           (fprintf f "~a~a~a" td escaped td)
           (unless (= i (- size 1))
                   (fprintf f "~a" fd)))
      (display "\n" f))
    (map row-writer data))
  (call-with-output-file filename #:exists 'truncate csv-writer))


(define (read-csv filename
                  #:field_delimiter [fd #\,]
                  #:text_delimiter [td #\"])
  (define (row-reader row)
    (define row-chars (list->vector (string->list row)))
    (define row-length (vector-length row-chars))
    (define (state i s k)
      (if [>= i row-length] ;; out of range
          (if (eq? k '()) '()
              (cons (list->string (reverse k)) '()))
          (let* ([c (vector-ref row-chars i)]
                 [is_delimiter (eq? c td)])
            (log "i:~a c:~a s:~a k:~a" i c s k)
            (case s
              ([out]
               (if is_delimiter
                   (state (add1 i) 'in '())
                   (state (add1 i) s k)))
              ([in]
               (if is_delimiter
                   (state (add1 i) 'pout k)
                   (state (add1 i) s (cons c k))))
              ([pout]
               (if is_delimiter
                   ;; "" is escape for "
                   (state (add1 i) 'in (cons td k))
                   ;; out
                   (cons (list->string (reverse k))
                         (state i 'out '()))))
              (else error "state error: ~a" s))
            )))
    (log "v:~a length:~a" row-chars row-length)
    (state 0 'out '()))
  (define (csv-reader f)
    (define row (read-line f))
    (if (eq? row eof) '()
        (cons (row-reader row) (csv-reader f))))
  (call-with-input-file filename csv-reader))


(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
