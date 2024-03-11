#lang racket
(require (only-in file/sha1 hex-string->bytes))
(require (only-in sha sha256))
(require (only-in sha bytes->hex-string))
(require racket/serialize)

(provide (struct-out block) mine-block valid-block? mined-block?)
;block structure of Blockchain 
;includes hash, previous hash, transaction, timestamp, and nonce
;hash is calculated from the previous hash, timestamp, transaction, and nonce it the hashing of the block
(struct block
  (hash previous-hash transaction timestamp nonce)
  #:prefab)

;function to calculate the hash of the block
;this function takes the previous hash, timestamp, transaction, and nonce as input
(define (calculate-block-hash previous-hash timestamp transaction nonce)
  ;hash is calculated using the sha256 algorithm
  ;all inputs are converted to bytes and concatenated it to a single byte string
  ;finally it is converted to a hex string representation of the hash
  (bytes->hex-string (sha256 (bytes-append
           (string->bytes/utf-8 previous-hash)
           (string->bytes/utf-8 (number->string timestamp))
           (string->bytes/utf-8 (~a (serialize transaction)))
           (string->bytes/utf-8 (number->string nonce))))))

;function to check if the block is valid
;this function takes the block as input
;the "?" at the end of the function name is a convention in Racket to indicate that the function returns a boolean value
;bl is the block that is passed as input
(define (valid-block? bl)
  ;compares the input hash with the calculated hash
  ;block-xxx is used to access the fields of the block data structure "bl"
  (equal? (block-hash bl)
          (calculate-block-hash (block-previous-hash bl)
                                (block-timestamp bl)
                                (block-transaction bl)
                                (block-nonce bl))))
;set the difficulty of the blockchain to 2
;the target is set to the hash of the block
;2 means that the first 2 characters of the hash should be 0
;eg: 00xxxxxx
(define difficulty 2)

;target is the hash of the block which is calculated by make-bytes function (requires two arguments, the first is the number of bytes to create, and the second is the value to fill the bytes with)
;the value is then converted to a hex string
;32 is the space character " " in ASCII
;if 65 will be "A" in ASCII
;make-bytes 2 32 will return a two space characters eg "  "
;converts the bytes to a hex string it will be "2020" (in hex)
(define target (bytes->hex-string (make-bytes difficulty 32)))

;this function tries to mine the block
;the input is the hash of the block which is calculated by the calculate-block-hash function
;function tries to find the hash that matches the target
;the hash is converted to bytes and then to a hex string 
(define (mined-block? hash)
  ; the hash matches the target, given the difficulty
  (equal? (subbytes (hex-string->bytes hash) 1 difficulty)
          (subbytes (hex-string->bytes target) 1 difficulty)))

; Hashcash implementation
; This function takes the previous hash, timestamp, transaction, and nonce as input
(define (make-and-mine-block
         previous-hash timestamp transaction nonce)
  ;let is used to bind the value of the hash to the variable "hash"
  ;the value of the hash is calculated by the calculate-block-hash function
  (let ([hash (calculate-block-hash
               previous-hash timestamp transaction nonce)])
    ;if the hash is mined, then the block is returned else the nonce is incremented and the function is called recursively
    (if (mined-block? hash)
        (block hash previous-hash transaction timestamp nonce)
        (make-and-mine-block
         previous-hash timestamp transaction (+ nonce 1)))))

; function to mine the block by calling the make-and-mine-block function
; the input is the transaction and the previous hash
(define (mine-block transaction previous-hash)
  (make-and-mine-block
   previous-hash (current-milliseconds) transaction 1))



