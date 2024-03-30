#lang racket
(require (only-in sha sha256))
(require (only-in sha bytes->hex-string))
(require racket/serialize)

(provide (struct-out transaction-io)
         make-transaction-io valid-transaction-io?)
#|
Transaction-IO ใช้เพื่อเก็บข้อมูลของการทำธุรกรรม โดยมีข้อมูลดังนี้
- hash: คือ has ของการทำธุรกรรม
- value: คือ จำนวนเงินที่ถูกโอน
- owner: คือ ผู้ถือเงิน
- timestamp: คือ เวลาที่ทำการทำธุรกรรม
|#
(struct transaction-io
  (hash value owner timestamp)
  #:prefab)

; ฟังก์ชันใช้ในการคำนวณหาค่า hash ของ ธุรกรรม
(define (calculate-transaction-io-hash value owner timestamp)
  (bytes->hex-string (sha256 (bytes-append    ; ใช้ฟังก์ชัน sha256 ในการคำนวณหาค่า hashโดยเติมค่าของ value, owner และ timestamp ลงไป จากนั้นแปลงค่า hash ที่ได้เป็น string
           (string->bytes/utf-8 (number->string value)) ; แปลงค่า value จาก number เป็น string และแปลงค่า string ที่ได้เป็น bytes
           (string->bytes/utf-8 (~a (serialize owner))) ; แปลงค่า owner จาก string เป็น bytes
           (string->bytes/utf-8 (number->string timestamp)))))) ; แปลงค่า timestamp จาก number เป็น string และแปลงค่า string ที่ได้เป็น bytes

; ฟังก์ชันใช้ในการสร้างสร้าง ธุรกรรม โดยมีการคำนวณหาค่า hash ของ ธุรกรรม
(define (make-transaction-io value owner)
  (let ([timestamp (current-milliseconds)]) ; get the current time in milliseconds
    (transaction-io ; ตัวแปรที่ใช้เก็บข้อมูลของการทำธุรกรรมมีค่าเท่ากับ 
     (calculate-transaction-io-hash value owner timestamp) ; การคำนวณหาค่า hash ของ ธุรกรรม
     value  ; ค่าของเงินที่ถูกโอน
     owner  ; ค่าของผู้ถือเงิน
     timestamp))) ; ค่าของเวลาที่ทำการทำธุรกรรม

; ฟังก์ชันใช้ในการตรวจสอบความถูกต้องของ ธุรกรรม
(define (valid-transaction-io? t-in)
  (equal? (transaction-io-hash t-in) ; ค่าของ hash ดึงออกมาจาก ธุรกรรม t-in
          (calculate-transaction-io-hash ; เรียกใช้ฟังก์ชัน
            (transaction-io-value t-in) ; ดึงค่า value จาก t-in
            (transaction-io-owner t-in) ; ดึงค่า owner จาก t-in
            (transaction-io-timestamp t-in)))) ; ดึงค่า timestamp จาก t-in