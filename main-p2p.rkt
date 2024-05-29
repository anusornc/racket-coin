#lang racket
(require "main-helper.rkt")

; กำหนด args ให้เป็น list ของ arguments ที่รับมาจาก command line
; vector->list คือฟังก์ชันที่แปลง vector เป็น list
; current-command-line-arguments คือฟังก์ชันที่ส่งค่า arguments ที่รับมาจาก command line
; vector คือข้อมูลชนิดที่เก็บข้อมูลหลายๆ ชนิดในตัวเดียว 
(define args (vector->list (current-command-line-arguments)))

; ถ้าจำนวน arguments ไม่เท่ากับ 3 ให้แสดงข้อความและออกจากโปรแกรม
(when (not (= 3 (length args)))
  (begin
    (printf "Usage: main-p2p.rkt db.data port ip1:port1,ip2:port2...")
    (newline)
    (exit)))

; กำหนดฟังก์ชัน string-to-peer-info ที่รับ string และคืนค่าเป็น peer-info
(define (string-to-peer-info s)
  (let ([s (string-split s ":")])
    (peer-info (car s) (string->number (cadr s)))))

; กำหนดตัวแปร db-filename เป็นค่าแรกของ args  
(define db-filename (car args))
; กำหนดตัวแปร port เป็นค่าที่สองของ args
(define port (string->number (cadr args)))
; กำหนดตัวแปร valid-peers เป็น list ของ peer-info ที่ได้จากการแปลงค่าที่สามของ args ด้วยฟังก์ชัน string-to-peer-info
(define valid-peers
  (map string-to-peer-info (string-split (caddr args) ",")))



; สร้าง wallet a โดยใช้ make-wallet
(define wallet-a (make-wallet))

; กำหนดฟังก์ชัน initialize-new-blockchain ที่สร้าง wallet และ genesis transaction และ unspent transactions และ blockchain ใหม่
(define (initialize-new-blockchain)
  (begin
    ; กำหนด coin-base เป็น wallet ใหม่เพื่อใช้เป็น coinbase ที่จะใช้ในการสร้าง genesis transaction
    (define coin-base (make-wallet))

    ; แสดงข้อความ "Making genesis transaction..."
    (printf "Making genesis transaction...\n")
    ; สร้าง genesis transaction โดยใช้ make-transaction 
    ; โดยกำหนด wallet ที่เป็น coin-base และ wallet-a และ 100 และ list ว่าง
    ; from = coin-base, 
    ; to = wallet-a, 
    ; amount = 100, 
    ; inputs = empty list
    (define genesis-t (make-transaction coin-base wallet-a 100 '()))

    ; สร้าง unspent transactions โดยใช้ list ที่มี make-transaction-io ที่มี amount เป็น 100 และ wallet เป็น wallet-a
    (define utxo (list
                  (make-transaction-io 100 wallet-a)))

    ; แสดงข้อความ "Mining genesis block..."
    (printf "Mining genesis block...\n")
    ; สร้าง blockchain ใหม่โดยใช้ make-blockchain โดยกำหนด genesis-t และ "1337cafe" และ utxo
    ; genesis-t = genesis transaction
    ; "cmu" = seed-hash
    (define b (init-blockchain genesis-t "cmu-block" utxo))
    b))

; db-blockchain เป็น blockchain ที่ถูกเก็บไว้ในไฟล์ db-filename ถ้ามีไฟล์นี้อยู่แล้ว ถ้าไม่มีให้สร้าง blockchain ใหม่
(define db-blockchain
  (if (file-exists? db-filename) ; ถ้ามีไฟล์ db-filename อยู่แล้ว
      (file->struct db-filename)  ; ให้เปิดไฟล์ db-filename 
      (initialize-new-blockchain))) ; ถ้าไม่มีให้สร้าง blockchain ใหม่

; สร้าง peer-context โดยใช้ peer-context-data โดยกำหนดชื่อเป็น "Test peer" และ port เป็น port และ valid-peers เป็น valid-peers และ blockchain เป็น db-blockchain
(define peer-context
  (peer-context-data " peer node" port (list->set valid-peers) '() db-blockchain))
  
(define (get-blockchain) (peer-context-data-blockchain peer-context))

(run-peer peer-context)

; กำหนดฟังก์ชัน export-loop ที่ทำการ export blockchain ทุก 10 วินาที
(define (export-loop)
  (begin
    (sleep 10)
    (struct->file (get-blockchain) db-filename) ; ทำการเขียน blockchain ลงไฟล์ db-filename
    (printf "Exported blockchain to '~a'...\n" db-filename)
    (export-loop)))

(thread export-loop) ; ทำการเริ่ม thread ของ export-loop

; กำหนดฟังก์ชัน mine-loop ที่ทำการสร้าง blockchain ใหม่โดยใช้ send-money-blockchain โดยกำหนด blockchain เป็น blockchain ปัจจุบัน และ wallet ที่ส่งเป็น wallet-a และ wallet ที่รับเป็น wallet-a และ 1 และ contract เป็น contract ที่ได้จากการเปลี่ยนไฟล์เป็น contract โดยใช้ file->contract
(define (mine-loop)
  (let ([newer-blockchain
         (send-money-blockchain (get-blockchain) wallet-a wallet-a 1 (file->contract "contract.script"))])
    (set-peer-context-data-blockchain! peer-context newer-blockchain)
    (printf "Mined a block!")
    (sleep 5) ; หยุดการทำงาน 5 วินาที ก่อนที่จะทำการ mine อีกครั้ง
    (mine-loop)))

(mine-loop)
