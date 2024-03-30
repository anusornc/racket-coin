#lang racket
(provide true-for-all? struct->file file->struct file->contract)

(require racket/serialize)

; ฟังก์ชันนี้ใช้สำหรับตรวจสอบว่า predicate ทุกตัวใน list ที่กำหนดมานั้นเป็นจริงหรือไม่
; input: pred - ฟังก์ชันที่รับอาร์กิวเมนต์เดียว และคืนค่าเป็นจริงหรือเท็จ
; ตัวอย่างการใช้เช่น (true-for-all? (lambda (x) (>= x 0)) '(1 2 3 4 5)) จะคืนค่าเป็นจริง
; ตัวอย่างการใช้เช่น (true-for-all? (lambda (x) (>= x 0)) '(1 2 -3 4 5)) จะคืนค่าเป็นเท็จ
(define (true-for-all? pred list)
  (cond
    [(empty? list) #t] ; ถ้า list ว่าง ให้คืนค่าเป็นจริง
    [(pred (car list)) (true-for-all? pred (cdr list))] ; ถ้า pred ที่รับค่าเป็นจริง ให้เรียกตัวเองต่อ
    [else #f]))

; ฟังก์ชันนี้ชื่อ struct->file ใช้สำหรับเขียน struct ลงในไฟล์
; input: object - struct ที่ต้องการเขียนลงในไฟล์
(define (struct->file object file)
  (let ([out (open-output-file file #:exists 'replace)]) ; ทำการเปิดไฟล์เพื่อเขียนและถ้าไฟล์นั้นมีอยู่แล้วให้เขียนทับ
    (write (serialize object) out) ; เขียน struct ลงในไฟล์โดยใช้ serialize เพื่อแปลง struct ให้เป็น string
    (close-output-port out))) ; ทำการปิดไฟล์

; ฟังก์ชันนี้ชื่อ file->struct ใช้สำหรับอ่านไฟล์และแปลงเป็น struct
; input: file - ชื่อไฟล์ที่ต้องการอ่าน
(define (file->struct file)
  (letrec ([in (open-input-file file)] ; อ่านไฟล์ที่กำหนด
           [result (read in)]) ; อ่านข้อมูลจากไฟล์เก็บไว้ในตัวแปร result
    (close-input-port in) ; ปิดไฟล์
    (deserialize result))) ; ทำการ deserialize ข้อมูลที่อ่านมาเพื่อแปลงเป็น struct และคืนค่า struct ที่ได้

; ฟังก์ชันนี้ชื่อ file-contract ใช้สำหรับอ่านไฟล์ smart contract
(define (file->contract file)
  ; ถ้าไฟล์ไม่มีอยู่ให้คืนค่าเป็น #f
  ; with-handlers ใช้สำหรับจัดการกับ exception ที่เกิดขึ้น
  ; exn คือตัวแปรที่ใช้เก็บ exception ที่เกิดขึ้น ซึ่งในกรณีนี้คือ exn:fail? ที่เกิดขึ้นจากการอ่านไฟล์
  ; ถ้าเกิด exception ให้คืนค่าเป็น #f '() คือการเรียกใช้ lambda ที่ไม่มีอาร์กิวเมนต์ และคืนค่าเป็น #f
  (with-handlers ([exn:fail? (lambda (exn) '())])
    (read (open-input-file file)))) ; ถ้าไฟล์มีอยู่ให้ทำการอ่านไฟล์และคืนค่าเป็น contract ที่ได้

