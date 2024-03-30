#lang racket
(require (only-in file/sha1 hex-string->bytes))
(require (only-in sha sha256))
(require (only-in sha bytes->hex-string))
(require racket/serialize)

(provide (struct-out block) mine-block valid-block? mined-block?)
; โครงสร้างของบล็อก ประกอบไปด้วย 5 ส่วน ได้แก่
; 1. hash คือ ค่า hash ของบล็อก
; 2. previous-hash คือ ค่า hash ของบล็อกก่อนหน้า
; 3. transaction คือ ข้อมูลที่จะถูกบันทึกลงบล็อก
; 4. timestamp คือ ค่าเวลาที่บล็อกถูกสร้างขึ้น
; 5. nonce คือ ค่าที่ถูกเพิ่มขึ้นเพื่อให้ค่า hash ของบล็อกตรงกับเงื่อนไขที่กำหนด nonce ย่อมาจาก "number used once" หมายถึง ค่าที่ถูกใช้ครั้งเดียว
(struct block
  (hash previous-hash transaction timestamp nonce)
  #:prefab)

;ฟังก์ชัน calculate-block-hash ใช้สำหรับคำนวณค่า hash ของบล็อก
(define (calculate-block-hash previous-hash timestamp transaction nonce)
  ;ค่าแฮชของบล็อกถูกคำนวณจากการเอาค่า hash ของบล็อกก่อนหน้ามาต่อกับค่าเวลาที่บล็อกถูกสร้างขึ้น และข้อมูลที่จะถูกบันทึกลงบล็อก และค่า nonce แล้วคำนวณค่า hash ของบล็อกด้วยฟังก์ชัน sha256
  (bytes->hex-string (sha256 (bytes-append
           (string->bytes/utf-8 previous-hash)
           (string->bytes/utf-8 (number->string timestamp))
           (string->bytes/utf-8 (~a (serialize transaction))) ; ~a คือ ฟังก์ชันที่ใช้แปลงข้อมูลให้เป็น string
           (string->bytes/utf-8 (number->string nonce))))))

;ฟังชัน valid-block? ใช้สำหรับตรวจสอบค่า hash ของบล็อกว่าถูกต้องหรือไม่
;การทำงานคือเปรียบเทียบค่า hash ของบล็อกกับค่า hash ที่คำนวณได้จากฟังก์ชัน calculate-block-hash
;ถ้าค่า hash ของบล็อกตรงกับค่า hash ที่คำนวณได้จากฟังก์ชัน calculate-block-hash ฟังก์ชันจะคืนค่าเป็น #t ถ้าไม่ตรงกันจะคืนค่าเป็น #f
;bl คือตัวแปรที่เป็นข้อมูลของบล็อกที่จะถูกตรวจสอบ
(define (valid-block? bl)
  ;equal? นั้นเป็นฟังก์ชันใน Racket ที่ใช้สำหรับการเปรียบเทียบค่าของสองตัวแปร
  ;block-xxx is used to access the fields of the block data structure "bl"
  (equal? (block-hash bl)
          (calculate-block-hash (block-previous-hash bl)
                                (block-timestamp bl)
                                (block-transaction bl)
                                (block-nonce bl))))

;difficulty คือการกำหนดความยากในการหาค่า hash ที่ตรงกับเงื่อนไขที่กำหนด
;2 หมายถึง ค่า hash ที่ต้องมีค่าตั้งแต่ตำแหน่งที่ 1 ถึง 2 ต้องมีค่าเป็น 0 เท่านั้นถึงจะยอมรับ
;eg: 00xxxxxx
(define difficulty 2)

;ตัวแปร target ใช้สำหรับเก็บค่า hash ที่ต้องการหา
;สร้างจากฟังก์ชัน make-bytes โดยกำหนดค่า 2 และ 32 ซึ่ง 32 คือค่า ASCII ของ space (" ") และ 2 คือจำนวนตัวอักษรที่ต้องการ
;make-bytes 2 32 จะได้ตำแหน่งว่างสองตำแหน่ง eg "  "
;ค่าจะถูกแปลงเป็น hex ซึ่งก็คือ "2020" แล้วเก็บไว้ในตัวแปร target
(define target (bytes->hex-string (make-bytes difficulty 32)))

;ฟังก์ชันนี้จะทำการเปรียบเทียบค่าของ hash ที่ได้จากการคำนวณกับค่า target ที่กำหนด
;ถ้าค่า hash ที่ได้มาตรงกับค่า target ที่กำหนด ฟังก์ชันจะคืนค่าเป็น #t ถ้าไม่ตรงกันจะคืนค่าเป็น #f
;subbytes คือฟังก์ชันที่ใช้สำหรับการดึงค่าออกมาจากตำแหน่งที่กำหนดซึ่งในกรณีนี้คือตัดออกมาตั้งแต่ตำแหน่งที่ 1 ถึง difficulty ของ hash ที่ได้
(define (mined-block? hash)
  ; เปรียบเทียบค่า hash ที่ได้จากการคำนวณกับค่า target ที่กำหนด
  (equal? (subbytes (hex-string->bytes hash) 1 difficulty)
          (subbytes (hex-string->bytes target) 1 difficulty)))

; ฟังก์ชันนี้จะทำการสร้างบล็อกและทำการคำนวณค่า hash ของบล็อก และทำการหาค่า nonce ที่ทำให้ค่า hash ของบล็อกตรงกับเงื่อนไขที่กำหนด
; ฟังก์ชันนี้ทำงานตรงกับ Concensus PoW (Proof of Work) ซึ่งเป็นการทำงานที่ทำให้ค่า hash ของบล็อกตรงกับเงื่อนไขที่กำหนด
(define (make-and-mine-block
         previous-hash timestamp transaction nonce)
  ;let เป็นการกำหนดตัวแปรใช้ภายในฟังก์ชันและการกำหนดด้วย let ในกรณีนี้คิือตัวแปร hash ที่เก็บค่า hash ของบล็อกที่ได้จากการคำนวณ
  ;ด้วยฟังก์ชัน calculate-block-hashโดยส่งค่า previous-hash timestamp transaction nonce ไปเป็นอาร์กิวเมนต์
  (let ([hash (calculate-block-hash
               previous-hash timestamp transaction nonce)])
    ;เช็คว่าค่า hash ที่ได้มาตรงกับเงื่อนไขที่กำหนดหรือไม่กับฟังก์ชัน mined-block?
    ;ถ้าตรงกันจะสร้างบล็อกและคืนค่าเป็นบล็อกที่ถูกสร้างขึ้น
    ;ถ้าไม่ตรงกันจะทำการเพิ่มค่า nonce แล้วทำการเรียกฟังก์ชัน make-and-mine-block ใหม่อีกครั้ง (recursive)
    (if (mined-block? hash)
        (block hash previous-hash transaction timestamp nonce)
        (make-and-mine-block
         previous-hash timestamp transaction (+ nonce 1)))))

; ฟังก์ชัน mine-block ใช้สำหรับสร้างบล็อกและทำการคำนวณค่า hash ของบล็อก และทำการหาค่า nonce ที่ทำให้ค่า hash ของบล็อกตรงกับเงื่อนไขที่กำหนด โดยการเรียกไปยังฟังก์ชัน make-and-mine-block กำหนดค่าเริ่มต้นของ nonce เป็น 1
(define (mine-block transaction previous-hash)
  (make-and-mine-block
   previous-hash (current-milliseconds) transaction 1))



