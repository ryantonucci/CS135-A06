;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname examples-a06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;1a
(check-expect (my-list-ref (list 95 7 1) 1) 7)
(check-expect (my-list-ref (list 9 8 6 7 2 4 3 1 5) 8) 5)
(check-expect (my-list-ref (4 5 7 99 21 56) 3) 99)

;;1b
(check-expect (zip (list 9 8 7 6) (list 'z 'y 'x 'w))
              (list (list 9 'z) (list 8 'y) (list 7 'x) (list 6 'w)))
(check-expect (zip (list 'a 'b 'c 'd) (list "a" "b" "c" "d"))
              (list (list 'a "a") (list 'b "b") (list 'c "c") (list 'd "d")))

;;1c
(check-expect (list-xor (list 1 3 5 8 10) (list 0 4 5 6 10))
              (list 1 3 4 6 8))
(check-expect (list-xor (list 1 2 3 4 6) (list 9 10 11 14 23)) 
              (list 1 2 3 4 6 9 10 11 14 23))
(check-expect (list-xor (list 1 2 3 4 6) (list 1 2 3 4 6)) 
              empty)

;;2c
(check-expect (num->signmag 16) (make-SignMag 'positive 16))
(check-expect (num->signmag 0) (make-SignMag 'zero 1))
(check-expect (num->signmag -22) (make-SignMag 'negative 22))

;;2d
(check-expect (signmag->num (make-SignMag 'zero 1)) 0)
(check-expect (signmag->num (make-SignMag 'positive 99)) 99)
(check-expect (signmag->num (make-SignMag 'negative 42)) -42)

;;3a
(check-expect (full? (make-node 6 (make-node 5 3 empty) 7)) false)
(check-expect (full? (make-node 9 (make-node 7 3 8) (make-node 9 8 11))) true)

;;4a
(define instr-actlst (list (make-action ("Ricky" 50 "Bought lunch for a friend"))
                           (make-action ("Ricky" -80 "Cheated on a quiz"))
                           (make-action ("Debra" 61 "Donated to charity"))
                           (make-action ("Debra" -60 "Laughed at homeless person"))
                           (make-action ("Ricky" 20 "Pet neighbours dog"))
                           (make-action ("Ricky" -80 "Smacked sister"))
                           (make-action ("Bobby" -40 "Ate brother's Holloween candy"))
                           (make-action ("DJ" 78 "Baked cupcakes for his family"))
                           (make-action ("Bobby" 45 "Helped friend with math homework"))))

(check-expect (extreme-actions "Ricky" instr-actlst)
              (list "Cheated on a quiz" "Bought lunch for a friend"))
(check-expect (extreme-actions "Bobby" instr-actlst)
              (list "Ate brother's Holloween candy" "Helped friend with math homework"))
(check-expect (extreme-actions "Ava" instr-actlst) empty)

;;4b
(define instr-childrenlist (list (list "Ricky" (list (make-wish 48 "Toy car")
                                                     (make-wish 12 "Crayons")
                                                     (make-wish 10 "Math textbook")))
                                 (list "Debra" (list (make-wish 40 "Barbie doll")
                                                     (make-wish 12 "Water bottle")))
                                 (list "Bobby" (list (make-wish 40 "Teddy bear")
                                                     (make-wish 20 "Wallet")))
                                 (list "DJ" (list (make-wish 30 "Cupcakes")
                                                  (make-wish 25 "Pizza")))))

(check-expect (gifts-recieved "Ricky" -90 instr-childrenlist -20) 'krampus)
(check-expect (gifts-recieved "DJ" 78 instr-childrenlist -20)
              (list "Playing Card Deck" "Cupcakes" "Pizza"))
(check-expect (gifts-recieved "Debra" 1 instr-childrenlist -20)
              (list "Playing Card Deck"))
(check-expect (gifts-recieved "Johnny" 0 instr-childrenlist -20) empty)

;;Q4c
(define-struct actionnode (name score actions left right))

(define tree (make-actionnode (make-action ("DJ" 78 "Baked cupcakes for his family"))
                              (make-action ("Debra" "Debra" 61 "Donated to charity"))
                              (make-action ("Ricky" 50 "Bought lunch for a friend"))))

(check-expect (add-action (make-action ("Bobby" -12 "Fed geese")) tree)
              (make-actionnode (make-action ("DJ" 78 "Baked cupcakes for his family"))
                               (make-actionnode ("Debra" "Debra" 61 "Donated to charity"
                                                         (make-action "Bobby" -12 "Fed geese") empty))
                               (make-action ("Ricky" 50 "Bought lunch for a friend"))))
              
(check-expect (add-action (make-action ("Ryan" 45 "Ate his vegatables")) tree)
              (make-actionnode (make-action ("DJ" 78 "Baked cupcakes for his family"))
                               (make-action ("Debra" "Debra" 61 "Donated to charity"))
                               (make-actionnode ("Ricky" 50 "Bought lunch for a friend" empty
                                                         (make-action ("Ryan" 45 "Ate his vegatables"))))))

;;4d

(check-expect (gift-list tree instr-childrenlist -20)
              (list (list "Debra" (list "Playing Card Deck" "Barbie doll" "Water bottle"))
                    (list "DJ" (list "Playing Card Deck" "Cupcakes" "Pizza"))
                    (list "Ricky" (list "Playing Card Deck" "Toy car" "Crayons" "Math textbook"))))

(define tree2 (make-actionnode (make-action ("Bobby" -21 "Fell asleep during english"))
                               (make-action ("Jim" 10 "Shared legos"))
                               (make-action ("Debra" 15 "Shared Candy"))))

(check-expect (gift-list tree2 instr-childrenlist -20)
               (list (list "Bobby" (list 'krampus))
                (list "Debra" (list "Playing Card Deck" "Water bottle"))))
                    
              








               