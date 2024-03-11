#lang racket
(provide true-for-all? struct->file file->struct file->contract)

(require racket/serialize)

; This procedure returns true if the predicate satisfies all members of the list
(define (true-for-all? pred list)
  (cond
    [(empty? list) #t] ; If the list is empty, return true
    [(pred (car list)) (true-for-all? pred (cdr list))] ; If the predicate is true for the first element, check the rest of the list
    [else #f]))

; Export a struct to a file
; The file will be overwritten if it already exists
; The file will be created if it does not exist
; input: object - the struct to be exported
(define (struct->file object file)
  (let ([out (open-output-file file #:exists 'replace)]) ; Open the file for writing
    (write (serialize object) out) ; Write the serialized struct to the file
    (close-output-port out))) ; Close the file

; Import struct contents from a file
; input: file - the file to be imported
; output: the struct that was imported
(define (file->struct file)
  (letrec ([in (open-input-file file)] ; Open the file for reading
           [result (read in)]) ; Read the contents of the file
    (close-input-port in) ; Close the file
    (deserialize result))) ; Deserialize the contents of the file

; Import contract contents from a file
(define (file->contract file)
  ; If the file does not exist, return an empty list
  (with-handlers ([exn:fail? (lambda (exn) '())])
    (read (open-input-file file)))) ; Read the contents of the file if it exists

