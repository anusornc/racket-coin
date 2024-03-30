#lang racket
(require "block.rkt")
(require "transaction.rkt")
(require "utils.rkt")
(require "wallet.rkt")
(require "smart-contracts.rkt")

; อนุญาตให้โมดูลอื่นเรียกใช้งานข้อมูลและฟังก์ชันที่ได้นำเข้ามาในโมดูลนี้ดังต่อไปนี้
; block.rkt
; transaction.rkt
; wallet.rkt
; และอนุญาตให้โมดูลอื่นเรียกใช้งานโครงสร้างข้อมูล blockchain และฟังก์ชันดังต่อไปนี้
; init-blockchain
; send-money-blockchain
; balance-wallet-blockchain
; valid-blockchain?
(provide (all-from-out "block.rkt")
         (all-from-out "transaction.rkt")
         (all-from-out "wallet.rkt")
         (struct-out blockchain)
         init-blockchain send-money-blockchain
         balance-wallet-blockchain valid-blockchain?)

; กำหนดให้โครงสร้างข้อมูล blockchain ประกอบด้วยฟิลด์ดังต่อไปนี้
; blocks: รายการของบล็อกทั้งหมดในบล็อกเชน
; utxo: รายการของ unspent transaction outputs ทั้งหมดในบล็อกเชน
(struct blockchain
  (blocks utxo)
  #:prefab)

; กำหนดให้ฟังก์ชัน init-blockchain สร้างบล็อกเชนใหม่โดยรับข้อมูลการทำธุรกรรมแรก t, ค่า hash ของบล็อกเริ่มต้น seed-hash และ unspent transaction outputs แรก utxo
; โดยฟังก์ชันนี้จะสร้างบล็อกเชนใหม่โดยเริ่มต้นด้วยการขุดบล็อกแรกจากการประมวลผลการทำธุรกรรมแรก และ unspent transaction outputs แรก
; ฟังก์ชันนี้จะคืนค่าเป็นบล็อกเชนใหม่ที่สร้างขึ้น
; cons คือฟังก์ชันสร้างรายการใหม่โดยเพิ่มข้อมูลใหม่ลงไปที่หัวรายการเดิม และคืนค่าเป็นรายการใหม่ที่สร้างขึ้น
; โดยเรียกใช้ฟังก์ชัน mine-block และ process-transaction จากโมดูล block.rkt และ transaction.rkt ตามลำดับ เพื่อขุดบล็อกแรกและประมวลผลการทำธุรกรรมแรก โดยใช้ seed-hash และ utxo ที่กำหนดให้

(define (init-blockchain t seed-hash utxo)
  (blockchain (cons (mine-block (process-transaction t) seed-hash) '())
              utxo))

; กำหนดให้ฟังก์ชัน mining-reward-factor คำนวณตัวคูณของรางวัลการขุดบล็อกจากจำนวนบล็อกทั้งหมดในบล็อกเชนโดยใช้สูตรดังต่อไปนี้ 50 / 2^(จำนวนบล็อกทั้งหมดในบล็อกเชน / 210000)
; ฟังก์ชันนี้จะคืนค่าเป็นตัวคูณของรางวัลการขุดบล็อก
(define (mining-reward-factor blocks)
  (/ 50 (expt 2 (floor (/ (length blocks) 210000)))))

; กำหนดให้ฟังก์ชัน add-transaction-to-blockchain รับบล็อก b และธุรกรรม t และเพิ่มการทำธุรกรรมนี้ลงในบล็อกเชนผ่านการเรียกใช้ฟังก์ชัน mine-block ผลลัพธ์ที่ได้คือ hash ของบล็อกเชนที่ขุดเสร็จสิ้น
(define (add-transaction-to-blockchain b t)
  (letrec ([hashed-block
            (mine-block t (block-hash (car (blockchain-blocks b))))]
           ; processed-inputs คือ รายการของ เงินที่ถูกส่งเข้ามาในการทำธุรกรรม
           [processed-inputs (transaction-inputs t)]
           ; processed-outputs คือ รายการของ เงินที่ถูกส่งออกไปในการทำธุรกรรม
           [processed-outputs (transaction-outputs t)]
           ; utxo คือ รายการของ unspent transaction outputs ทั้งหมดในบล็อกเชน
           ; set-unions คือ ฟังก์ชันที่รับรายการของเซตและรวมเซตทั้งหมดเข้าด้วยกัน
           [utxo (set-union processed-outputs
                            (set-subtract (blockchain-utxo b)
                                          processed-inputs))]
           ; new-blocks คือ รายการของบล็อกทั้งหมดในบล็อกเชนที่ถูกเพิ่มเข้าไปโดยเพิ่มบล็อกที่ขุดเสร็จสิ้นลงไปที่หัวรายการเดิม
           [new-blocks (cons hashed-block (blockchain-blocks b))]
           [utxo-rewarded (cons
                           (make-transaction-io
                            (mining-reward-factor new-blocks)
                            (transaction-from t))
                           utxo)])
    ; พิมพ์ข้อมูลการทำธุรกรรมที่ถูกประมวลผล
    (printf "Processed Inputs: ~a\n" processed-inputs)
    (printf "Processed Outputs: ~a\n" processed-outputs)
    ; ส่งค่ากลับเป็นบล็อกเชนใหม่ที่ถูกเพิ่มเข้าไปโดยเพิ่มบล็อกที่ขุดเสร็จสิ้นลงไปที่หัวรายการเดิม และอัปเดต unspent transaction outputs ใหม่
    (blockchain
     new-blocks
     utxo-rewarded)))

; ส่งเงินผ่านบล็อกเชน รับข้อมูลบล็อกเชน b, ผู้ส่งเงิน from, ผู้รับเงิน to, จำนวนเงิน value และสัญญา c
(define (send-money-blockchain b from to value c)
  (letrec ([my-ts ; รายการของ unspent transaction outputs ทั้งหมดที่เป็นของผู้ส่งเงิน
            ;ทำการกรองข้อมูลจากรายการของ unspent transaction outputs ทั้งหมดในบล็อกเชนโดยเลือกเฉพาะข้อมูลที่เป็นของผู้ส่งเงินออกมา
            (filter (lambda (t) (equal? from (transaction-io-owner t)))
                    (blockchain-utxo b))]
           ; t คือ การทำธุรกรรมใหม่ที่สร้างขึ้นจากการธุรกรรมที่ได้คัดกรองจาก utxo และสร้างขึ้นโดยใช้ฟังก์ชัน make-transaction จากโมดูล transaction.rkt
           [t (make-transaction from to value my-ts)])
    (if (transaction? t)  ; ถ้า t เป็นธุรกรรมที่ถูกต้อง
        (let ([processed-transaction (process-transaction t)]) ; ประมวลผลธุรกรรม
          (if (and  ; ถ้ามีเงื่อนไขดังต่อไปนี้
               (>= (balance-wallet-blockchain b from) value)  ; ยอดเงินในกระเป๋าเงินของผู้ส่งเงินมากกว่าหรือเท่ากับจำนวนเงินที่ต้องการส่ง
               (valid-transaction-contract? processed-transaction c)) ; สัญญาที่ถูกต้อง
              (add-transaction-to-blockchain b processed-transaction) ; เพิ่มธุรกรรมลงในบล็อกเชน
              b))
        (add-transaction-to-blockchain b '()))))  ; ถ้าไม่ใช่ เราจะไม่ทำการส่งเงินและจะไม่เพิ่มธุรกรรมลงในบล็อก b และจะคืนค่าเป็นบล็อกเชนเดิม

; คำนวณยอดเงินในกระเป๋าเงิน w ของผู้ใช้งานจากบล็อกเชน b 
(define (balance-wallet-blockchain b w)
  ; คำนวณยอดเงินในกระเป๋าเงิน w ของผู้ใช้งานจากบล็อกเชน b โดยใช้ฟังก์ชัน letrec เพื่อกำหนดตัวแปร utxo และ my-ts
  (letrec ([utxo (blockchain-utxo b)]
          ; กรองข้อมูลจากรายการของ unspent transaction outputs ทั้งหมดในบล็อกเชนโดยเลือกเฉพาะข้อมูลที่เป็นของผู้ใช้งานออกมา
           [my-ts (filter
                   (lambda (t) (equal? w (transaction-io-owner t)))
                   utxo)])
    ; คืนค่าเป็นผลรวมของเงินในกระเป๋าเงิน w ของผู้ใช้งาน
    (foldr + 0 (map (lambda (t) (transaction-io-value t)) my-ts))))

; กำหนดให้ฟังก์ชัน valid-blockchain? ตรวจสอบความถูกต้องของบล็อกเชน b โดยตรวจสอบดังต่อไปนี้
(define (valid-blockchain? b)
  (let ([blocks (blockchain-blocks b)]) ; รายการของบล็อกทั้งหมดในบล็อกเชน
    (and ; ตรวจสอบดังต่อไปนี้ ถ้ามีเงื่อนไขใดเป็นเท็จจะคืนค่าเป็นเท็จ
     ; ต้องเป็น Block ที่ถูกต้อง
     (true-for-all? valid-block? blocks)
     ; เปรียบเทียบค่า hash ของบล็อกก่อนหน้ากับค่า hash ของบล็อกถัดไป
     ; ถ้าไม่เท่ากันจะคืนค่าเป็นเท็จ
     ; drop-right คือ ฟังก์ชันที่ตัดข้อมูลที่อยู่ทางด้านขวาของรายการออกไปจากจำนวนที่กำหนด
     (equal? (drop-right (map block-previous-hash blocks) 1)
             (cdr (map block-hash blocks)))
     ; เป็นทรานเซ็คชันที่ถูกต้อง
     (true-for-all?
      valid-transaction? (map
                          (lambda (block) (block-transaction block)) ; รายการของการทำธุรกรรมในบล็อก
                          blocks))
     ; ต้องเป็นบล็อกที่ถูกขุด โดยการเปรียบเทียบค่า hash ของบล็อกก่อนหน้ากับค่า hash ของบล็อกถัดไป
     (true-for-all?
      mined-block? (map block-hash blocks)))))


