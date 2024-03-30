#lang racket
(require "blockchain.rkt")
(require "block.rkt")
(require racket/serialize)


(provide (struct-out peer-context-data)
         (struct-out peer-info)
         run-peer)

;โครงสร้างของ peer-info ประกอบด้วย ip และ port 
(struct peer-info
  (ip port)
  #:prefab)

; โครงสร้างของ peer-info-io ประกอบด้วย 
; pi เก็บข้อมูล peer-info และ input-port และ output-port
(struct peer-info-io
  (pi input-port output-port)
  #:prefab)

; โครงสร้างของ peer-context-data ประกอบด้วย
; name เก็บชื่อของ peer
; port เก็บ port ของ peer
; valid-peers เก็บเพียง peer ที่ถูกต้องที่ได้ตรวจสอบแล้ว
; connected-peers เก็บ peer ที่เชื่อมต่ออยู่
; blockchain เก็บ blockchain ที่ peer นี้มี
; mutable ใช้เพื่อให้สามารถเปลี่ยนแปลงค่าได้ในโครงสร้างนี้
(struct peer-context-data
  (name
   port
   [valid-peers #:mutable]
   [connected-peers #:mutable]
   [blockchain #:mutable])
  #:prefab)

; ฟังก์ชันจัดการ ประกอบด้วย 
; peer-context ข้อมูลของ peer
; in และ out ใช้สำหรับการสื่อสาร
(define (handler peer-context in out)
  (flush-output out) ; ส่งข้อมูลที่อยู่ใน buffer ออกไป 
  (define line (read-line in)) ; อ่านข้อมูลจาก คีย์บอร์ด
  (when (string? line) ; ถ้าข้อมูลที่อ่านมาเป็น string
    (cond [(string-prefix? line "get-valid-peers") ; ถ้าข้อมูลที่อ่านมาเป็น ส่วน prefix เป็น get-valid-peers
           (fprintf out "valid-peers:~a\n"
                    (serialize
                     (set->list
                      (peer-context-data-valid-peers peer-context)))) ; ดึงข้อมูล valid-peers จาก peer-context และ แปลงเป็น list พร้อมกับทำเป็น serialize และ ฟังก์ชัน fprintf จะทำการเขียนข้อมูลงใน outport ที่ชื่อว่า out และ ~a คือข้อมูลที่ถูก serialize และ \n คือการขึ้นบรรทัดใหม่ ~a ที่อยู่ในข้อมูลที่ถูก serialize (placeholder)  
           (handler peer-context in out)] ; เรียกใช้ฟังก์ชัน handler อีกครั้ง
          [(string-prefix? line "get-latest-blockchain") ; ถ้าข้อมูลที่อ่านมาเป็น ส่วน prefix เป็น get-latest-blockchain
           (fprintf out "latest-blockchain:")
           (write
            (serialize (peer-context-data-blockchain peer-context)) out)
           (handler peer-context in out)]
          [(string-prefix? line "latest-blockchain:") ; ถ้าข้อมูลที่อ่านมาเป็น ส่วน prefix เป็น latest-blockchain
           (begin (maybe-update-blockchain peer-context line)
                  (handler peer-context in out))]
          [(string-prefix? line "valid-peers:") ; ถ้าข้อมูลที่อ่านมาเป็น ส่วน prefix เป็น valid-peers
           (begin (maybe-update-valid-peers peer-context line)
                  (handler peer-context in out))]
          [(string-prefix? line "exit") ; ถ้าข้อมูลที่อ่านมาเป็น ส่วน prefix เป็น exit
           (fprintf out "bye\n")]
          [else (handler peer-context in out)])))

; โพรซีเจอร์สำหรับตรวจสอบว่า blockchain ที่ได้มาถูกต้องหรือไม่
; รับข้อมูล peer-context และ line (คำสั่งที่รับมาจากคีย์บอร์ด)
(define (maybe-update-blockchain peer-context line)
  (let ([latest-blockchain  ; กำหนดตัวแปร latest-blockchain ให้เป็นข้อมูลที่ได้จากการตัดคำสั่ง
         (trim-helper line #rx"(latest-blockchain:|[\r\n]+)")]
        [current-blockchain ; กำหนดตัวแปร current-blockchain 
         (peer-context-data-blockchain peer-context)])
    (when (and (valid-blockchain? latest-blockchain)
               (> (get-blockchain-effort latest-blockchain)
                  (get-blockchain-effort current-blockchain)))
      (printf "Blockchain updated for peer ~a\n"
              (peer-context-data-name peer-context))
      (set-peer-context-data-blockchain! peer-context
                                         latest-blockchain))))

; โพรซีเจอร์สำหรับคำนวณความยากของ blockchain
; ใช้ฟังก์ชัน foldl ในการคำนวณผลรวมของค่าที่ได้จากการ map ค่า nonce ของ block ทั้งหมดใน blockchain
; ฟังก์ชัน map ใช้ในการดึงค่า nonce ของ block ทั้งหมดใน blockchain และ นำมาคำนวณผลรวมด้วยฟังก์ชัน foldl 
(define (get-blockchain-effort b)
  (foldl + 0 (map block-nonce (blockchain-blocks b))))

; โพรซีเจอร์สำหรับอัพเดทข้อมูลของ peer ที่ถูกต้อง
(define (maybe-update-valid-peers peer-context line)
  (let ([valid-peers (list->set
                      (trim-helper line #rx"(valid-peers:|[\r\n]+)"))]
        [current-valid-peers (peer-context-data-valid-peers
                              peer-context)])
    (set-peer-context-data-valid-peers!
     peer-context
     (set-union current-valid-peers valid-peers))))

; โพรซีเจอร์สำหรับตัดคำสั่ง
; รับข้อมูล line และ x และทำการตัดคำสั่งออกจากข้อมูล line โดยใช้ฟังก์ชัน string-replace
(define (trim-helper line x)
  (deserialize
   (read
    (open-input-string
     (string-replace line x "")))))

; โพรซีเจอร์สำหรับยอมรับและจัดการข้อมูล
; รับข้อมูล listener และ peer-context และใช้ฟังก์ชัน tcp-accept ในการรับข้อมูลจาก listener และใช้ฟังก์ชัน thread ในการสร้างเธรดใหม่
; in คือ input-port และ out คือ output-port ที่ได้จากการ tcp-accept และใช้ฟังก์ชัน handler ในการจัดการข้อมูล
; define-value แตกต่างจาก define ตรงที่มีการ evaluate expression และ return ค่าออกมาและกำหนดค่าให้กับตัวแปร
(define (accept-and-handle listener peer-context)
  (define-values (in out) (tcp-accept listener)) 
  (thread
   (lambda ()
     (handler peer-context in out)
     (close-input-port in)
     (close-output-port out))))

; โพรซีเจอร์สำหรับเปิดพอร์ตเพื่อรอรับข้อมูล
#|
  ฟังก์ชัน peers/serve เป็นฟังก์ชันที่ใช้สำหรับสร้าง TCP server เพื่อรอรับ connection จาก peer อื่น ๆ ในระบบ P2P โดยมีการทำงานดังนี้
สร้าง main-cust ซึ่งเป็น custodian ด้วยฟังก์ชัน make-custodian เพื่อใช้ในการจัดการ resource ต่าง ๆ ของ server
ใช้ parameterize เพื่อกำหนดให้ current-custodian มีค่าเป็น main-cust ภายในบล็อกของ parameterize นี้ resource ที่ถูกสร้างขึ้นจะอยู่ภายใต้การดูแลของ main-cust custodian
สร้าง listener ด้วยฟังก์ชัน tcp-listen โดยรับ parameters ดังนี้
port ที่จะใช้รัน server ซึ่งได้จาก (peer-context-data-port peer-context)
maximum number ของ pending connections (backlog) ให้เป็น 5
#t flag เพื่อระบุว่าจะ reuse address เดิมได้เมื่อ restart server
สร้างฟังก์ชัน loop ที่จะถูกรันใน thread แยกต่างหาก โดยมีหน้าที่ดังนี้
เรียกฟังก์ชัน accept-and-handle เพื่อ accept incoming connection และจัดการกับ connection นั้น โดยส่ง listener และ peer-context เข้าไปเป็น argument
เมื่อจัดการ connection เสร็จแล้ว จะเรียก (loop) เพื่อวนกลับไปรอ accept connection ใหม่อีกครั้ง (recursive loop)
สร้าง thread ใหม่ด้วยคำสั่ง (thread loop) เพื่อรัน loop ใน thread แยกต่างหากไม่ให้ block thread หลัก
return ฟังก์ชัน (lambda () (custodian-shutdown-all main-cust)) ออกไป เพื่อใช้เป็นตัวจัดการในการปิด server ฟังก์ชันนี้จะเรียก custodian-shutdown-all เพื่อปิด custodian และ cleanup resource ต่าง ๆ รวมถึง thread ที่ถูกสร้างด้วย
โดยสรุปแล้ว peers/serve จะสร้าง TCP server ที่รอ accept connection อยู่เรื่อย ๆ เมื่อมี connection เข้ามาก็จะส่งให้ accept-and-handle จัดการต่อ server จะถูกรันบน thread แยกต่างหากเพื่อไม่ให้ไปบล็อก thread หลัก และจะ return ฟังก์ชันสำหรับปิด server ออกมาให้ใช้เมื่อต้องการหยุดการทำงานของ server
|#
(define (peers/serve peer-context)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener
      (tcp-listen (peer-context-data-port peer-context) 5 #t))  ; max 5 pending connections
    (define (loop)
      (accept-and-handle listener peer-context)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

#|
  ฟังก์ชัน connect-and-handle เป็นฟังก์ชันที่ใช้สำหรับเชื่อมต่อไปยัง peer อื่นในระบบ P2P และจัดการกับ connection นั้น โดยมีขั้นตอนการทำงานดังนี้

ใช้ฟังก์ชัน tcp-connect เพื่อเชื่อมต่อไปยัง peer โดยใช้ IP และ port ที่ได้จาก (peer-info-ip peer) และ (peer-info-port peer) ตามลำดับ ฟังก์ชันนี้จะ return input port (in) และ output port (out) สำหรับใช้ในการสื่อสารกับ peer
สร้าง current-peer-io โดยใช้ฟังก์ชัน peer-info-io เพื่อสร้าง data structure ที่เก็บข้อมูลของ peer ที่กำลังเชื่อมต่ออยู่ รวมถึง input port และ output port ที่เชื่อมกับ peer นั้น
เพิ่ม current-peer-io เข้าไปใน list ของ connected peers ใน peer-context ด้วย set-peer-context-data-connected-peers! โดยใช้ cons เพื่อเพิ่มเข้าไปที่ตำแหน่งแรกของ list
สร้าง thread ใหม่เพื่อจัดการกับ connection โดยใช้ thread และ lambda ภายใน thread จะมีการทำงานดังนี้
เรียกฟังก์ชัน handler เพื่อจัดการกับ connection โดยส่ง peer-context, in และ out เข้าไปเป็น argument
หลังจากเสร็จสิ้นการทำงานของ handler แล้ว ปิด in และ out port ด้วย close-input-port และ close-output-port
ลบ current-peer-io ออกจาก list ของ connected peers ใน peer-context ด้วย set-peer-context-data-connected-peers! และ set-remove เพื่อเคลียร์ connection ที่จบไปแล้วออกจาก list
โดยสรุป connect-and-handle จะทำหน้าที่เชื่อมต่อไปหา peer, เพิ่ม connection เข้าไปใน list ของ connected peers, สร้าง thread ใหม่เพื่อจัดการกับ connection (ด้วยฟังก์ชัน handler), และเคลียร์ connection ออกจาก list เมื่อจบการทำงาน ทั้งหมดนี้ทำให้สามารถจัดการ connection แต่ละอันแยกจากกันได้ และไม่ต้องไปบล็อก thread หลักในขณะที่กำลังสื่อสารกับ peer อยู่
|#
(define (connect-and-handle peer-context peer)
  (begin
    (define-values (in out)
      (tcp-connect (peer-info-ip peer)
                   (peer-info-port peer)))

    (define current-peer-io (peer-info-io peer in out))

    (set-peer-context-data-connected-peers!
     peer-context
     (cons current-peer-io
           (peer-context-data-connected-peers peer-context)))

    (thread
     (lambda ()
       (handler peer-context in out)
       (close-input-port in)
       (close-output-port out)

       (set-peer-context-data-connected-peers! ; 
        peer-context
        (set-remove
         (peer-context-data-connected-peers peer-context)
         current-peer-io))))))

#|
  ฟังก์ชัน peers/connect เป็นฟังก์ชันที่ใช้ในการเชื่อมต่อไปยัง peer อื่น ๆ ในระบบ P2P (peer-to-peer) โดยมีขั้นตอนการทำงานดังนี้

สร้าง custodian ชื่อ main-cust ด้วยฟังก์ชัน make-custodian เพื่อใช้ในการจัดการและควบคุมทรัพยากรต่าง ๆ ที่ถูกสร้างขึ้นภายในฟังก์ชัน peers/connect
ใช้ parameterize เพื่อกำหนดค่า current-custodian ให้เป็น main-cust ภายในบล็อกของ parameterize ทำให้ทรัพยากรที่สร้างขึ้นในบล็อกนี้อยู่ภายใต้การดูแลของ main-cust
กำหนดฟังก์ชัน loop เพื่อทำการเชื่อมต่อไปยัง peer อื่น ๆ แบบวนลูป โดยมีขั้นตอนดังนี้
ใช้ฟังก์ชัน get-potential-peers เพื่อดึงรายชื่อ peer ที่มีโอกาสเชื่อมต่อได้จาก peer-context เก็บไว้ในตัวแปร potential-peers
ใช้ for เพื่อวนลูปไปตามแต่ละ peer ใน potential-peers
สำหรับแต่ละ peer ใช้ with-handlers เพื่อจัดการกับ exception ที่อาจเกิดขึ้นระหว่างการเชื่อมต่อ
ถ้าเกิด exception ประเภท exn:fail? (ซึ่งเป็น exception ทั่วไปที่บ่งบอกว่ามีข้อผิดพลาดเกิดขึ้น) จะใช้ฟังก์ชัน (lambda (x) #t) เพื่อเพิกเฉยต่อ exception นั้นและข้ามไปยัง peer ถัดไป
ถ้าไม่เกิด exception จะเรียกใช้ฟังก์ชัน connect-and-handle เพื่อทำการเชื่อมต่อและจัดการกับ peer นั้น
เมื่อวนลูปเสร็จแล้ว ใช้ sleep เพื่อหยุดพักเป็นเวลา 10 วินาที
เรียกใช้ฟังก์ชัน loop เพื่อเริ่มต้นวนลูปใหม่
สร้าง thread ใหม่ด้วย thread และให้มันรันฟังก์ชัน loop แยกออกไปจาก thread หลัก เพื่อให้การเชื่อมต่อเกิดขึ้นในแบ็กกราวด์โดยไม่บล็อกการทำงานของโค้ดส่วนอื่น ๆ
ส่งคืนฟังก์ชันที่สร้างจาก lambda ซึ่งเมื่อถูกเรียกใช้จะทำการปิดและทำลายทรัพยากรทั้งหมดที่ถูกสร้างภายใต้ main-cust ด้วยฟังก์ชัน custodian-shutdown-all
โดยสรุป peers/connect ทำหน้าที่เชื่อมต่อไปยัง peer ต่าง ๆ ที่อาจพร้อมให้บริการ (potential peers) ซ้ำไปเรื่อย ๆ ตามรอบเวลาที่กำหนด (10 วินาทีต่อรอบ) โดยใช้ thread แยกเพื่อไม่ให้กระทบการทำงานส่วนอื่น มีการจัดการกับ exception ที่อาจเกิดขึ้นระหว่างการเชื่อมต่อ และส่งคืนฟังก์ชันสำหรับปิดและคืนทรัพยากรทั้งหมดเมื่อต้องการหยุดการเชื่อมต่อ
|# 
(define (peers/connect peer-context)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define (loop)
      (let ([potential-peers (get-potential-peers peer-context)])
        (for ([peer potential-peers])
          (with-handlers ([exn:fail? (lambda (x) #t)])
            (connect-and-handle peer-context peer))))
      (sleep 10)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

#|
  ฟังก์ชัน get-potential-peers เป็นฟังก์ชันที่ใช้สำหรับดึงรายชื่อของ peer ที่มีโอกาสจะเชื่อมต่อได้ (potential peers) จาก peer-context โดยมีขั้นตอนการทำงานดังนี้

ดึงรายชื่อ peer ที่กำลังเชื่อมต่ออยู่ในปัจจุบัน (current connected peers) จาก peer-context-data-connected-peers ใน peer-context แล้วแปลงให้อยู่ในรูปแบบของ set โดย:
ใช้ฟังก์ชัน map เพื่อแปลงแต่ละ peer-info-io ใน connected-peers ให้เป็น peer-info ด้วยฟังก์ชัน peer-info-io-pi
แปลง list ที่ได้จาก map ให้เป็น set ด้วยฟังก์ชัน list->set และเก็บผลลัพธ์ไว้ในตัวแปร current-connected-peers
ดึงรายชื่อของ peer ที่ถูกต้องและน่าเชื่อถือได้ (valid peers) จากฟิลด์ valid-peers ใน peer-context-data ของ peer-context และเก็บไว้ในตัวแปร valid-peers
ใช้ฟังก์ชัน set-subtract เพื่อลบ current-connected-peers ออกจาก valid-peers ซึ่งจะทำให้เราได้เซตของ peer ที่ถูกต้องแต่ยังไม่ได้เชื่อมต่อในปัจจุบัน นั่นคือ potential peers นั่นเอง
คืนค่า set ของ potential peers ที่ได้จากขั้นตอนที่ 3 เป็นผลลัพธ์ของฟังก์ชัน
โดยสรุป get-potential-peers จะคืนค่าเป็นเซตของ peer ที่อยู่ในรายชื่อ valid peers แต่ไม่ได้เชื่อมต่ออยู่ในปัจจุบัน ซึ่งเป็น peer ที่มีโอกาสจะสามารถเชื่อมต่อได้ในรอบถัดไป

ตัวอย่างเช่น สมมติให้:

valid-peers เป็น #{'A 'B 'C 'D}
connected-peers เป็น '(#<peer-info-io> 'A #<peer-info-io> 'C)
หลังจากแปลง connected-peers ให้เป็น set ของ peer-info ด้วยฟังก์ชัน peer-info-io-pi เราจะได้ current-connected-peers เป็น #{'A 'C}

ดังนั้น (set-subtract valid-peers current-connected-peers) จะให้ผลลัพธ์เป็น #{'B 'D} ซึ่งเป็นเซตของ peer ที่มีโอกาสจะเชื่อมต่อได้ในรอบถัดไป (potential peers) นั่นเอง
|#
(define (get-potential-peers peer-context)
  (let ([current-connected-peers
         (list->set
          (map peer-info-io-pi
               (peer-context-data-connected-peers peer-context)))]
        [valid-peers (peer-context-data-valid-peers peer-context)])
    (set-subtract valid-peers current-connected-peers)))

#|
  ฟังก์ชัน peers/sync-data เป็นฟังก์ชันที่ใช้สำหรับซิงโครไนซ์ข้อมูลกับ peer อื่น ๆ ที่เชื่อมต่ออยู่ โดยมีขั้นตอนการทำงานดังนี้

กำหนดฟังก์ชัน loop เพื่อทำการซิงโครไนซ์ข้อมูลแบบวนลูป โดยมีขั้นตอนดังนี้
ใช้ sleep เพื่อหยุดพักเป็นเวลา 10 วินาที
วนลูปไปตามแต่ละ peer-info-io (p) ใน peer-context-data-connected-peers ของ peer-context โดยใช้ for
สำหรับแต่ละ p ให้:
ดึง input port (in) และ output port (out) จาก p ด้วยฟังก์ชัน peer-info-io-input-port และ peer-info-io-output-port ตามลำดับ
ใช้ fprintf เพื่อเขียนข้อความ "get-latest-blockchain\nget-valid-peers\n" ไปยัง out เพื่อขอข้อมูลล่าสุดของ blockchain และรายชื่อ peer ที่ถูกต้องจาก peer ปลายทาง
ใช้ flush-output กับ out เพื่อให้มั่นใจว่าข้อความถูกส่งไปแล้วจริง ๆ
ใช้ printf เพื่อแสดงข้อมูลสรุปของ peer ปัจจุบัน ได้แก่:
ชื่อของ peer จาก peer-context-data-name
จำนวน valid peers ที่มีอยู่จาก peer-context-data-valid-peers โดยใช้ set-count เพื่อนับจำนวนสมาชิกในเซต
จำนวน connected peers ที่มีอยู่จาก peer-context-data-connected-peers โดยใช้ set-count เช่นกัน
เรียก loop อีกครั้งเพื่อเริ่มรอบต่อไปของการซิงโครไนซ์ข้อมูล
สร้าง thread ใหม่ (t) ด้วย thread และให้มันรันฟังก์ชัน loop แยกออกไปจาก thread หลัก เพื่อให้การซิงโครไนซ์ข้อมูลเกิดขึ้นในแบ็กกราวด์โดยไม่บล็อกการทำงานของโค้ดส่วนอื่น ๆ
คืนฟังก์ชันที่สร้างจาก lambda ซึ่งเมื่อถูกเรียกใช้จะทำการหยุด thread t ด้วยฟังก์ชัน kill-thread เพื่อยุติการซิงโครไนซ์ข้อมูล
โดยสรุป peers/sync-data จะทำการซิงโครไนซ์ข้อมูลกับ peer ที่เชื่อมต่ออยู่ทุก ๆ 10 วินาที โดยการขอข้อมูล blockchain และรายชื่อ valid peers ล่าสุดจากแต่ละ peer การซิงโครไนซ์จะเกิดขึ้นในอีก thread หนึ่งแยกออกไป และฟังก์ชันจะคืนค่าเป็นฟังก์ชันสำหรับหยุดการซิงโครไนซ์เมื่อต้องการ นอกจากนี้มันยังแสดงข้อมูลสรุปเกี่ยวกับ peer ปัจจุบัน เช่น จำนวน valid peers และ connected peers ที่มีอยู่ เป็นต้น
|#
(define (peers/sync-data peer-context)
  (define (loop)
    (sleep 10)
    (for [(p (peer-context-data-connected-peers peer-context))]
      (let ([in (peer-info-io-input-port p)]
            [out (peer-info-io-output-port p)])
        (fprintf out "get-latest-blockchain\nget-valid-peers\n")
        (flush-output out)))
    (printf "Peer ~a reports ~a valid and ~a connected peers.\n"
            (peer-context-data-name peer-context)
            (set-count
             (peer-context-data-valid-peers peer-context))
            (set-count
             (peer-context-data-connected-peers peer-context)))
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)))

; Helper procedure for running a peer-to-peer connection.
(define (run-peer peer-context)
  (begin
    (peers/serve peer-context)
    (peers/connect peer-context)
    (peers/sync-data peer-context)))