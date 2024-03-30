#lang racket

(require crypto)
(require crypto/all)

;provide คือการกำหนดให้โมดูลนี้สามารถเรียกใช้โดยโมดูลอื่นได้ในกรณีนี้ได้กำหนดให้ wallet และ make-wallet สามารถเรียกใช้ได้
;wallet คือโครงสร้างของข้อมูลที่เก็บข้อมูลของ wallet ซึ่งประกอบด้วย private key และ public key
;make-wallet คือฟังก์ชันที่ใช้สำหรับสร้าง wallet โดยการสร้าง public key และ private key ขึ้นมา
(provide (struct-out wallet) make-wallet)

;โครงสร้างข้อมูล wallet ประกอบด้วย private key และ public key
(struct wallet
  (private-key public-key)
  #:prefab)
;prefab ทำให้โครงสร้างนี้สามารถทำ serialization ได้
; ฟังก์ชัน make-wallet ใช้สำหรับสร้าง wallet โดยการสร้าง public key และ private key ขึ้นมา
; โดยการสร้าง public key และ private key ขึ้นมาจะใช้ RSA algorithm โดยใช้ library crypto
; ฟังก์ชันนี้จะคืนค่า wallet ที่สร้างขึ้นมาในรูปแบบของโครงสร้างข้อมูล wallet
(define (make-wallet)
  ;letrec นั้นเป็นการกำหนดตัวแปรแบบ local โดยใช้ letrec จะสามารถเรียกใช้ตัวแปรที่ถูกกำหนดใน letrec ได้
  ;rsa-impl คือตัวแปรที่เก็บค่าของ RSA algorithm ที่ได้จากการเรียกใช้ get-pk โดยใช้ libcrypto-factory
  ;'rsa คือ algorithm ที่ใช้ในการสร้าง public key และ private key ในไลบรารี crypto ​ซึ่งใน Racket นั้นการใช้ ' นำหน้าค่าหมายถึงการใช้ค่านั้นเป็นค่าแบบ symbol (หมายถึง ค่าที่ไม่เปลี่ยนแปลง) และไม่ใช่ค่าแบบ string (นึกถึงการใช้งานใน Excel)
  (letrec ([rsa-impl (get-pk 'rsa libcrypto-factory)]
           [privkey (generate-private-key rsa-impl '((nbits 512)))]
           [pubkey (pk-key->public-only-key privkey)])
    (wallet (bytes->hex-string  ; ฟังก์ชัน bytes->hex-string ใช้สำหรับแปลงค่าข้อมูลจาก bytes ให้เป็น hex-string
             (pk-key->datum privkey 'PrivateKeyInfo)) ; ฟังก์ชัน pk-key->datum ใช้สำหรับแปลงค่าข้อมูลจาก public key หรือ private key ให้เป็นค่าข้อมูลแบบอื่น ๆ โดยในกรณีนี้คือแปลงค่าข้อมูลจาก private key ให้เป็นรูปแบบ PrivateKeyInfo 
            (bytes->hex-string
             (pk-key->datum pubkey 'SubjectPublicKeyInfo)))))