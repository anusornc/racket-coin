#lang racket
(require "transaction-io.rkt") ; นำเข้าโค้ดจากไฟล์ transaction-io.rkt
(require "utils.rkt") ; นำเข้าโค้ดจากไฟล์ utils.rkt
(require (only-in file/sha1 hex-string->bytes)) ; นำเข้าเฉพาะ hex-string->bytes จากไฟล์ file/sha1
(require "wallet.rkt") ; นำเข้าโค้ดจากไฟล์ wallet.rkt
(require crypto) ; นำเข้าโค้ดจาก crypto
(require crypto/all) ; นำเข้าโค้ดจาก crypto/all
(require racket/serialize) ; นำเข้าโค้ดจาก racket/serialize


; กำหนดให้
; มีการเรียกใช้งานโค้ดทั้งหมดจากไฟล์ transaction-io.rkt ที่ได้นำเข้ามาในไฟล์นี้ จากโมดูลอื่นๆ ได้
; สามารถให้โมดูลอื่นเรียกใช้งานโครงสร้าง transaction และฟังก์ชันทั้งหมดในไฟล์นี้ได้
(provide (all-from-out "transaction-io.rkt")
         (struct-out transaction)
         make-transaction process-transaction valid-transaction?)
;ตัวอย่างการใช้ all-from-out
;Examples:
#| 
> (module nest racket
    (provide num-eggs)
    (define num-eggs 2))
> (module hen-house racket
    (require 'nest)
    (provide (all-from-out 'nest)))
> (require 'hen-house)
> num-eggs
2 
|#

; สร้างโครงสร้าง transaction โดยมีฟิลด์ดังนี้
; signature: สตริง เก็บข้อมูล Transaction ที่ได้รับการเซ็นเรียบร้อยแล้ว
; from: สตริง เก็บข้อมูล wallet address ของผู้ส่ง
; to: สตริง เก็บข้อมูล wallet address ของผู้รับ
; value: จำนวนเงินที่จะส่ง
; inputs: ลิสต์ เก็บข้อมูล transaction-io ที่เป็น input
; outputs: ลิสต์ เก็บข้อมูล transaction-io ที่เป็น output
(struct transaction
  (signature from to value inputs outputs)
  #:prefab)

; use-all-factories! ใช้เพื่อให้โค้ดสามารถใช้งานได้ทุกโมดูลใน crypto
(use-all-factories!)

; ฟังก์ชันสร้าง transaction ใหม่ โดยรับพารามิเตอร์ from, to, value, inputs
(define (make-transaction from to value inputs)
  (transaction  ; สร้าง transaction ใหม่ โดยใช้ค่าจากพารามิเตอร์ที่รับเข้ามา
   "" ; กำหนด signature เริ่มต้นเป็นค่าว่างก่อน
   from
   to
   value
   inputs
   '()))

; ทำการเซ็น transaction โดยใช้ private key ของ wallet ที่ระบุ
; การ sign จะใช้ข้อมูลจาก from, to, value ของ transaction นั้นๆ มาเข้ารหัสด้วย private key ของ wallet ที่ระบุ และใช้ sha1 เป็นอัลกอริทึมในการเข้ารหัส และคืนค่าเป็นสตริงที่เป็น hex string ของข้อมูลที่เซ็น โดยใช้ฟังก์ชัน bytes->hex-string ในการแปลงข้อมูลที่เซ็นเป็น hex string 
(define (sign-transaction from to value)
  (let ([privkey (wallet-private-key from)]
        [pubkey (wallet-public-key from)])
    (bytes->hex-string
     (digest/sign
      (datum->pk-key (hex-string->bytes privkey) 'PrivateKeyInfo)
      'sha1
      (bytes-append
       (string->bytes/utf-8 (~a (serialize from)))
       (string->bytes/utf-8 (~a (serialize to)))
       (string->bytes/utf-8 (number->string value)))))))

; ฟังชันนี้จะประมวลผล transaction โดยรับ transaction เข้ามา และคืนค่าเป็น transaction ใหม่ที่ทำการลงนามด้วย private-key
(define (process-transaction t)
  (letrec
      ([inputs (transaction-inputs t)]
       [outputs (transaction-outputs t)]
       [value (transaction-value t)]
       [inputs-sum
        (foldr + 0 (map (lambda (i) (transaction-io-value i)) inputs))]
       [leftover (- inputs-sum value)]
       [new-outputs
        (list
         (make-transaction-io value (transaction-to t))
         (make-transaction-io leftover (transaction-from t)))])
    (transaction ;สร้าง transaction ใหม่ตามโครงสร้างที่กำหนด
     (sign-transaction (transaction-from t)
                       (transaction-to t)
                       (transaction-value t)) ; ทำการเซ็น transaction โดยใช้ฟังก์ชัน sign-transaction
     (transaction-from t) ; กำหนด from ให้เป็นค่าเดิม
     (transaction-to t) ; กำหนด to ให้เป็นค่าเดิม
     value ; กำหนด value ให้เป็นค่าเดิม
     inputs ; กำหนด inputs ให้เป็นค่าเดิม
     (append new-outputs outputs)))) ; กำหนด outputs ให้เป็นค่าที่ได้จากการประมวลผล

; ฟังก์ชันตรวจสอบว่าความถูกต้องของการ Sign ทรานเซ็คชัน โดยรับอินพุตที่เป็น transaction ที่ได้รับการเซ็นแล้ว และคืนค่าเป็นค่าความถูกต้องของการเซ็น
(define (valid-transaction-signature? t)
  (let ([pubkey (wallet-public-key (transaction-from t))])
    (digest/verify ;เรียกใช้ฟังก์ชัน digest/verify โดยให้ค่าเป็นค่าความถูกต้องของการเซ็น
     (datum->pk-key (hex-string->bytes pubkey) 'SubjectPublicKeyInfo)
     'sha1
     (bytes-append
      (string->bytes/utf-8 (~a (serialize (transaction-from t))))
      (string->bytes/utf-8 (~a (serialize (transaction-to t))))
      (string->bytes/utf-8 (number->string (transaction-value t))))
     (hex-string->bytes (transaction-signature t)))))

; ฟังก์ชันตรวจสอบความถูกต้องของ transaction โดยรับ transaction เข้ามา และคืนค่าเป็นความถูกต้องของ transaction
(define (valid-transaction? t)
  (let ([sum-inputs ; คำนวณผลรวมของ inputs
         (foldr + 0 (map (lambda (t) (transaction-io-value t))
                         (transaction-inputs t)))]
        [sum-outputs ; คำนวณผลรวมของ outputs
         (foldr + 0 (map (lambda (t) (transaction-io-value t))
                         (transaction-outputs t)))])
    (and
     ; และทรานเซ็คชันนี้มีการเซ็นเรียบร้อย
     (valid-transaction-signature? t)
     ; และทรานเซ็คชันนี้มี outputs ที่ถูกต้อง
     (true-for-all? valid-transaction-io? (transaction-outputs t))
     ; และผลรวมของ inputs มากกว่าหรือเท่ากับผลรวมของ outputs
     ; จะบอกว่ายอดเงินที่จะส่งมากกว่าหรือเท่ากับยอดเงินที่จะโอน
     (>= sum-inputs sum-outputs))))

