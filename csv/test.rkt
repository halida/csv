#lang racket

(require "main.rkt")

(require 2htdp/batch-io)

(module+
 test
 (require rackunit)

 (csv-debug #t)

 ;; Tests to be run with raco test
 (define data '(("name" "location")
                ("James" "Tokyo")
                ("Bob" "Shanghai")
                ("Steven" "Austin")))
 (define csv-raw
   #<<HERESTRING
"name","location"
"James","Tokyo"
"Bob","Shanghai"
"Steven","Austin"
HERESTRING
 )
 (define filename "xxx.csv")
 (write-csv filename data)
 (check-equal? (read-file filename) csv-raw)
 (check-equal? (read-csv filename) data)
 )
