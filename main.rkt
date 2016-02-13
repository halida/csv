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

(provide read-csv write-csv csv-debug row-reader)

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

;; writer
(define (csv-writer f data fd td)
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

(define (write-csv file data
                   #:field-delimiter [fd #\,]
                   #:text-delimiter [td #\"])
  (define (writer f) (csv-writer f data fd td))
  (cond
   ([string? file]
    (call-with-output-file file #:exists 'truncate writer))
   ([output-port? file]
    (writer file))
   (else (error "file type unknown" file))
   ))

;; reader
(define (row-reader row fd td)
  (define row-chars (list->vector (string->list row)))
  (define row-length (vector-length row-chars))
  (define (state i s k)
    (if [>= i row-length] ;; out of range
        (if (eq? k '()) '()
            (cons (list->string (reverse k)) '()))
        (let* ([c (vector-ref row-chars i)])
          (log "i:~a c:~a s:~a k:~a" i c s k)
          (case s
            ([out] ;; out of each text
             (cond
               ([eq? c td] ;; in quoted text
                (state (add1 i) 'inq '()))
               ([member c (list fd #\space #\tab)] ;; seperates
                (state (add1 i) s k))
               (else ;; unquoted text
                (state i 'inn k))))
            ([inq] ;; in each text, quoted by text-delimiter
             (cond
               ([eq? c td] ;; pending out
                (state (add1 i) 'pout k))
               (else ;; append text
                (state (add1 i) s (cons c k)))))
            ([inn] ;; in each text, not quoted by text-delimiter
             (cond
               ([eq? c fd] ;; unquoted text out
                (cons (list->string (reverse k))
                      (state (add1 i) 'out '())))
               (else ;; append text
                (state (add1 i) s (cons c k)))))
            ([pout] ;; pending out, encounter text-delimiter, check if it is escpaed delimiter
             (cond
               ([eq? c td] ;; "" is escape for "
                (state (add1 i) 'inq (cons td k)))
               (else ;; out
                (cons (list->string (reverse k))
                      (state i 'out '())))))
            (else error "state error: ~a" s))
          )))
  (log "v:~a length:~a" row-chars row-length)
  (state 0 'out '()))

(define (csv-reader f fd td)
  (define row (read-line f))
  (if (eq? row eof) '()
      (cons (row-reader row fd td) (csv-reader f fd td))))

(define (read-csv file
                  #:field-delimiter [fd #\,]
                  #:text-delimiter [td #\"])
  (define (reader f) (csv-reader f fd td))
  (cond
   ([string? file]
    (call-with-input-file file reader))
   ([port? file]
    (reader file))
   (else (error "file type unknown" file))))

  

