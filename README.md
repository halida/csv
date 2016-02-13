# csv

csv reader/writer for Racket language.

## install

`raco pkg install csv`

## Usage

```scheme
(require csv)
(define data '(("name" "location")
              ("James" "Tokyo")
              ("Bob" "Shanghai")
              ("Steven" "Austin")))
(define filename "xxx.csv")
(write-csv filename data)
(eq? (read-csv filename) data)
```

## todo

- document
- read-csv string
