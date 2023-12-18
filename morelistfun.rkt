;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname morelistfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;1a
;;(my-list-ref list-of-nums index) returns the value in list-of-nums at an index
;;Examples
(check-expect (my-list-ref (list 95 7 1) 1) 7)
(check-expect (my-list-ref (list 9 8 6 7 2 4 3 1 5) 8) 5)
;;my-list-ref: (listof Num) Nat -> Num
;;Requires index be < the length of (listof Num)

(define (my-list-ref list-of-nums index)
  (cond [(zero? index) (first list-of-nums)]
        [(empty? list-of-nums) false]
        [else (my-list-ref (rest list-of-nums) (sub1 index))]))

(check-expect (my-list-ref (list 4 5 7 99 21 56) 3) 99)

;;1b
;;(zip list1 list2) creates an associated list with each index of each list being related.
;;the first being the key and the second is the corresponding values.
;;Examples
(check-expect (zip (list 9 8 7 6) (list 'z 'y 'x 'w))
              (list (list 9 'z) (list 8 'y) (list 7 'x) (list 6 'w)))
(check-expect (zip (list 'a 'b 'c 'd) (list "a" "b" "c" "d"))
              (list (list 'a "a") (list 'b "b") (list 'c "c") (list 'd "d")))
;;zip: (listof Any) (listof Any)->AL
;;Requires list1 and list2 to be equal in length.

(define (zip list1 list2)
  (cond [(empty? list1) empty]
        [else (cons (cons (first list1) (cons (first list2) empty))
                    (zip (rest list1) (rest list2)))]))

;;1c
;;(list-xor slon1 slon2)
;;Examples
(check-expect (list-xor (list 1 3 5 8 10) (list 0 4 5 6 10))
              (list 0 1 3 4 6 8))
(check-expect (list-xor (list 1 2 3 4 6) (list 9 10 11 14 23)) 
              (list 1 2 3 4 6 9 10 11 14 23))
;;list-xor: (listof Num) (listof Num) -> (listof Num)
;;Requires slon1 and slon2 to be sorted in increasing order.

(define (list-xor slon1 slon2)
  (cond [(and (empty? slon1) (empty? slon2)) empty]
        [(and (cons? slon1) (empty? slon2)) (append slon1 empty)]
        [(and (empty? slon1) (cons? slon2)) (append slon2 empty)]
        [(> (first slon1) (first slon2))
         (cond [(member? (first slon2) slon1)
                (list-xor (eliminate-instance (first slon2) slon1) (rest slon2))]
               [else (cons (first slon2) (list-xor slon1 (rest slon2)))])]
        [(< (first slon1) (first slon2))
         (cond [(member? (first slon1) slon2)
                (list-xor (rest slon1)  (eliminate-instance (first slon1) slon2))]
               [else (cons (first slon1) (list-xor (rest slon1) slon2))])]
        [else (list-xor (rest slon1) (rest slon2))]))
 
(check-expect (list-xor (list 1 2 3 4 6) (list 1 2 3 4 6)) 
              empty)

(define (eliminate-instance instance list)
  (cond [(empty? list) empty]
        [(= instance (first list)) (eliminate-instance instance (rest list))]
        [else (cons (first list) (eliminate-instance instance (rest list)))]))

(check-expect (eliminate-instance 4 (list 2 3 4 6 7 8)) (list 2 3 6 7 8))



