# csv

csv reader/writer for Racket language.

## Usage

```scheme
(define data '(("name" "location")
              ("James" "Tokyo")
              ("Bob" "Shanghai")
              ("Steven" "Austin")))
define filename "xxx.csv")
(write-csv filename data)
(eq? (read-csv filename) data)
```

## todo

- document
- read string
