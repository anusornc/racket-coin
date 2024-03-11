#lang racket
(require (only-in sha sha256))
(require (only-in sha bytes->hex-string))
(require racket/serialize)

(provide (struct-out transaction-io)
         make-transaction-io valid-transaction-io?)
#|
The transaction structure will consist of a transaction-io structure (transaction input/output). The transaction input will represent the blockchain address from which the money was sent, and the transaction output will represent the blockchain address to which the money was sent.
|#
; A transaction-io is a record with a hash, value, owner, and timestamp
(struct transaction-io
  (hash value owner timestamp)
  #:prefab)

; Procedure for calculating the hash of a transaction-io object
; The hash is calculated by concatenating the value, owner, and timestamp
(define (calculate-transaction-io-hash value owner timestamp)
  (bytes->hex-string (sha256 (bytes-append
           (string->bytes/utf-8 (number->string value))
           (string->bytes/utf-8 (~a (serialize owner)))
           (string->bytes/utf-8 (number->string timestamp))))))

; Make a transaction-io object with calculated hash
; input: value, owner
; The transaction-io object will have a hash, value, owner, and timestamp
; The function returns a transaction-io object
(define (make-transaction-io value owner)
  (let ([timestamp (current-milliseconds)]) ; get the current time in milliseconds
    (transaction-io ; create a transaction-io object
     (calculate-transaction-io-hash value owner timestamp) ; calculate the hash
     value  ; set the value
     owner  ; set the owner
     timestamp))) ; set the timestamp

; A transaction-io is valid if...
(define (valid-transaction-io? t-in)
  ; the hash is correct
  (equal? (transaction-io-hash t-in)
          (calculate-transaction-io-hash
            (transaction-io-value t-in)
            (transaction-io-owner t-in)
            (transaction-io-timestamp t-in))))