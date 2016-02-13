#lang racket

(require "main.rkt")
(require 2htdp/batch-io)

(module+
 test
 (require rackunit)

 ;; (csv-debug #t)

 ;; Tests to be run with raco test
 (define data '(("name" "location")
                ("James" "Tokyo")
                ("Bob" "Shanghai")
                ("Steven" "Austin")))
 (define csv-raw
   #<<HERESTRING
'name','location'
'James','Tokyo'
'Bob','Shanghai'
'Steven','Austin'
HERESTRING
 )
 (define filename "xxx.csv")
 (write-csv filename data #:text-delimiter #\')
 (check-equal? (read-file filename) csv-raw)
 (check-equal? (read-csv filename #:text-delimiter #\') data)

 (define testcases-for-read-csv
   '(
     ;; normal quote
     (("name, value, 1")
      ("name" "value" "1"))
     (("'name', 'value', '1'")
      ("name" "value" "1"))
     (("name, 'value', 1")
      ("name" "value" "1"))
     ;; ;; escape
     (("name, 'val''ue', 1")
      ("name" "val'ue" "1"))
     (("name, 'va,lue', 1")
      ("name" "va,lue" "1"))
     (("name, val'ue, 1")
      ("name" "val'ue" "1"))
     ;; ;; error
     (("name, 'value'', 1")
      ("name" "value', 1"))
     ))
 (for ([item testcases-for-read-csv])
      (let ([raw (caar item)]
            [result (cadr item)])
        (check-equal? (row-reader raw #\, #\') result)))
 )
