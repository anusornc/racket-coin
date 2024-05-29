#lang racket
;ไฟล์นี้ใช้สำหรับทดสอบการทำงานของ Blockchain

;เรียกใช้ไฟล์ main-helper.rkt ที่มีฟังก์ชันที่ช่วยในการทดสอบ
(require "main-helper.rkt")

;เมื่อมีไฟล์ blockchain.data อยู่แล้ว ให้โหลดข้อมูลจากไฟล์นั้นและแสดงผล
(when (file-exists? "blockchain.data")
  (begin
    (printf "Found 'blockchain.data', reading...\n")
    (print-blockchain (file->struct "blockchain.data"))
    (exit)))

; เริ่มสร้างกระเป๋าเงิน 
(define coin-base (make-wallet))
(define wallet-a (make-wallet))
(define wallet-b (make-wallet))

; สร้าง genesis transaction เติมเงินใน wallet-a 100 หน่วย
(printf "Making genesis transaction...\n")
(define genesis-t (make-transaction coin-base wallet-a 100 '()))

; รายการ UTXO ที่เกิดจาก genesis transaction
; สร้าง ทรานแซคชั่นไอโอ โดยใช้ฟังก์ชัน make-transaction-io
(define utxo (list
              (make-transaction-io 100 wallet-a)))

; Blockchain initiation
(printf "Mining genesis block...\n")
(define blockchain (init-blockchain genesis-t "cmu-block" utxo))
(print-wallets blockchain wallet-a wallet-b)

; Make a second transaction
(printf "Mining second transaction...\n")
(set! blockchain (send-money-blockchain blockchain wallet-a wallet-b 2 (file->contract "contract.script")))
(print-wallets blockchain wallet-a wallet-b)

; Make a third transaction
(printf "Mining third transaction...\n")
(set! blockchain (send-money-blockchain blockchain wallet-b wallet-a 1 (file->contract "contract.script")))
(print-wallets blockchain wallet-a wallet-b)

; Attempt to make a fourth transaction
(printf "Attempting to mine fourth (not-valid) transaction...\n")
(set! blockchain (send-money-blockchain blockchain wallet-b wallet-a 3 (file->contract "contract.script")))
(print-wallets blockchain wallet-a wallet-b)

(printf "Blockchain is valid: ~a\n\n" (valid-blockchain? blockchain))

(for ([block (blockchain-blocks blockchain)])
  (print-block block)
  (newline))

(struct->file blockchain "blockchain.data")
(printf "Exported blockchain to 'blockchain.data'...\n")
