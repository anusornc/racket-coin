#lang racket

(require "transaction.rkt")

(provide valid-transaction-contract?)

(define (valid-transaction-contract? t c)
  (and (eval-contract t c)
       (valid-transaction? t)))

(define (eval-contract t c)
  (match c
    [(? number? x) x]
    [(? string? x) x]
    [`() #t]
    [`true #t]
    [`false #f]
    [`(if ,co ,tr ,fa) (if (eval-contract t co)
                           (eval-contract t tr)
                           (eval-contract t fa))]
    [`(+ ,l ,r) (+ (eval-contract t l) (eval-contract t r))]
    [`from (transaction-from t)]
    [`to (transaction-to t)]
    [`value (transaction-value t)]
    [`(+ ,l ,r) (+ (eval-contract t l) (eval-contract t r))]
    [`(* ,l ,r) (* (eval-contract t l) (eval-contract t r))]
    [`(- ,l ,r) (- (eval-contract t l) (eval-contract t r))]
    [`(= ,l ,r) (= (eval-contract t l) (eval-contract t r))]
    [`(> ,l ,r) (> (eval-contract t l) (eval-contract t r))]
    [`(< ,l ,r) (< (eval-contract t l) (eval-contract t r))]
    [`(and ,l ,r) (and (eval-contract t l) (eval-contract t r))]
    [`(or ,l ,r) (or (eval-contract t l) (eval-contract t r))]
    [else #f]))
#|
  ตรวจสอบความถูกต้องของ transaction และ contract ในบล็อกเชน โดยมีฟังก์ชันหลักคือ valid-transaction-contract? ที่รับ transaction (t) และ contract (c) แล้วตรวจสอบว่า transaction นั้นเป็นไปตามเงื่อนไขของ contract หรือไม่ และ transaction นั้นถูกต้องหรือไม่

ฟังก์ชัน eval-contract เป็นฟังก์ชันที่ใช้ในการประเมินค่าของ contract โดยใช้ match เพื่อจับคู่รูปแบบของ contract และประเมินค่าตามรูปแบบที่กำหนด รูปแบบของ contract ที่รองรับมีดังนี้:

ถ้า contract เป็นตัวเลข (number?) หรือสตริง (string?) จะคืนค่านั้นเลย
ถ้า contract เป็น '() (empty list), 'true หรือ 'false จะคืนค่า #t หรือ #f ตามลำดับ
ถ้า contract เป็น '(if cond then else) จะประเมินค่า cond ก่อน ถ้าเป็นจริงจะประเมินค่า then ถ้าเป็นเท็จจะประเมินค่า else
ถ้า contract เป็น 'from, 'to หรือ 'value จะดึงค่าของฟิลด์ from, to หรือ value จาก transaction t ตามลำดับ
ถ้า contract เป็น '(+ l r), '(* l r), '(- l r), '(= l r), '(> l r) หรือ '(< l r) จะประเมินค่า l และ r แล้วใช้ตัวดำเนินการตามสัญลักษณ์ที่กำหนด
ถ้า contract เป็น '(and l r) หรือ '(or l r) จะประเมินค่า l และ r แล้วใช้ตัวดำเนินการ and หรือ or ตามลำดับ
ถ้า contract ไม่ตรงกับรูปแบบใด ๆ จะคืนค่า #f
ฟังก์ชัน valid-transaction-contract? จะเรียกใช้ eval-contract เพื่อประเมินค่าของ contract กับ transaction ที่ให้มา และเรียกใช้ valid-transaction? เพื่อตรวจสอบความถูกต้องของ transaction โดยจะคืนค่า #t ก็ต่อเมื่อทั้ง contract และ transaction เป็นจริง

โมดูลนี้ใช้โมดูล "transaction.rkt" และเปิดเผยเฉพาะฟังก์ชัน valid-transaction-contract? ออกมาให้ใช้งานจากภายนอกผ่าน provide
|#