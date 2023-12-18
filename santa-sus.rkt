;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname santa-sus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan TOnucci (21059852)
;;Assignment 6

;;Q4a
(define-struct action (name niceness desc))

(define instr-actlst (list (make-action "Ricky" 50 "Bought lunch for a friend")
                           (make-action "Ricky" -80 "Cheated on a quiz")
                           (make-action "Debra" 61 "Donated to charity")
                           (make-action "Debra" -60 "Laughed at homeless person")
                           (make-action "Ricky" 20 "Pet neighbours dog")
                           (make-action "Ricky" -80 "Smacked sister")
                           (make-action "Bobby" -40 "Ate brother's Holloween candy")
                           (make-action "Dj" 78 "Baked cupcakes for his family")
                           (make-action "Bobby" 45 "Helped friend with math homework")))
;;(extreme-actions name Actions) returns a list of the description of the best and
;;worst thing the child did.
;;Eamples
(check-expect (extreme-actions "Ricky" instr-actlst)
              (list "Cheated on a quiz" "Bought lunch for a friend"))
(check-expect (extreme-actions "Bobby" instr-actlst)
              (list "Ate brother's Holloween candy" "Helped friend with math homework"))
(check-expect (extreme-actions "Ava" instr-actlst) empty)
;;extreme-actions: Str listof Action -> listof Str

(define (extreme-actions name list-of-actions)
  (extreme-actions-acc name list-of-actions (list "" 0) (list "" 0)))

(define (extreme-actions-acc name list-of-actions hi lo)
  (cond [(empty? list-of-actions) (cond [(and (string=? (first hi) "") (string=? (first lo) "")) empty]
                                        [(and (string=? (first hi) "") (not (string=? (first lo) "")))
                                         (list (first lo) (first lo))]
                                        [(and (not (string=? (first hi) "")) (string=? (first lo) ""))
                                         (list (first hi) (first hi))]
                                        [else (list (first lo) (first hi))])]
        [(and (string=? (action-name (first list-of-actions)) name)
              (> (action-niceness (first list-of-actions)) (second hi)))
         (cond [(string=? (first lo) "")
                (extreme-actions-acc name (rest list-of-actions)
                                     (list (action-desc (first list-of-actions))
                                           (action-niceness (first list-of-actions))) hi)]
               [else (extreme-actions-acc name (rest list-of-actions)
                                          (list (action-desc (first list-of-actions))
                                                (action-niceness (first list-of-actions)))
                                          lo)])]
        [(and (string=? (action-name (first list-of-actions)) name)
              (< (action-niceness (first list-of-actions)) (second lo)))
         (cond [(string=? (first hi) "")
                (extreme-actions-acc name (rest list-of-actions) lo
                                     (list (action-desc (first list-of-actions))
                                           (action-niceness (first list-of-actions))))]
               [else (extreme-actions-acc name (rest list-of-actions)
                                          hi (list (action-desc (first list-of-actions))
                                                   (action-niceness (first list-of-actions))))])] 
        [else (extreme-actions-acc name (rest list-of-actions)
                                   hi lo)]))

(define CL2 (list (make-action "Aaron" 20 "Cleaned dishes")
                  (make-action "Aaron" 30 "Being a good guy")
                  (make-action "Loriane" -10 "Not good thing")
                  (make-action "Loriane" -20 "Worse thing")))

(check-expect (extreme-actions "Aaron" CL2) (list "Cleaned dishes"
                                                  "Being a good guy"))
(check-expect (extreme-actions "Loriane" CL2) (list "Worse thing"
                                                    "Not good thing"))

;;4b
(define-struct wish (score gift))
(define instr-childrenlist (list (list "Ricky" (list (make-wish 48 "Toy car")
                                                     (make-wish 12 "Crayons")
                                                     (make-wish 10 "Math textbook")))
                                 (list "Debra" (list (make-wish 40 "Barbie doll")
                                                     (make-wish 12 "Water bottle")))
                                 (list "Bobby" (list (make-wish 40 "Teddy bear")
                                                     (make-wish 20 "Wallet")))
                                 (list "Dj" (list (make-wish 30 "Cupcakes")
                                                  (make-wish 25 "Pizza")))
                                 (list "Gino" (list (make-wish 30 "Toy1")
                                                    (make-wish 25 "Toy2")
                                                    (make-wish 20 "Toy3")
                                                    (make-wish 15 "Toy4")))))

;;(gifts-received name niceness CL threshold) 
;;Examples
(check-expect (gifts-received "Ricky" -90 instr-childrenlist -20) 'krampus)
(check-expect (gifts-received "Dj" 78 instr-childrenlist -20)
              (list "Playing Card Deck" "Pizza" "Cupcakes"))
;;gifts-received: Str Num ChildrenList Num -> listof Str

(define (gifts-received name niceness CL threshold)
  (cond [(empty? CL) empty]
        [(< niceness threshold) 'krampus]
        [(< niceness 0) 'coal]
        [(string=? (first (first CL)) name)
         (cons "Playing Card Deck" (what-toys-acc niceness (second (first CL)) empty))]
        [else (gifts-received name niceness (rest CL) threshold)]))

(check-expect (gifts-received "Debra" 1 instr-childrenlist -20)
              (list "Playing Card Deck"))
(check-expect (gifts-received "Johnny" 0 instr-childrenlist -20) empty)
(check-expect (gifts-received "Gino" 40 instr-childrenlist -10)
              (list "Playing Card Deck" "Toy4" "Toy3" "Toy2" "Toy1"))

(define (what-toys-acc niceness list-of-wishes acc) ;returns the toys the child will recieve from wish list
  (cond [(empty? list-of-wishes) acc]
        [(>= niceness (wish-score (first list-of-wishes)))
         (what-toys-acc niceness
                        (rest list-of-wishes)
                        (append (list (wish-gift (first list-of-wishes))) acc))]
        [else (what-toys-acc niceness (rest list-of-wishes) acc)]))


;;4c
(define-struct actionnode (name score actions left right))


;;An ActionNode is a (make-actionnode Str Int (listof Action)
;;                                    ActionSearchTree
;;                                    ActionSearchTree)
;; requires: name > every name in left ActionNode
;;           name < every name in right ActionNode

;; An ActionSearchTree (ActionST) is one of:
;; * empty
;; * an ActionNode

(define tree (make-actionnode "Dj" 58 (list (make-action "Dj" 78 "Baked cupcakes for his family")
                                            (make-action "Dj" -20 "Ate all the cupcakes"))
                              (make-actionnode "Debra" 61
                                               (list
                                                (make-action "Debra" 61 "Donated to charity")) empty empty)
                              (make-actionnode "Ricky" 50
                                               (list
                                                (make-action "Ricky" 50 "Bought lunch for a friend")) empty empty)))

;;(add-action action AST) adds an action to the action search tree (AST)
;;Examples
(check-expect (add-action (make-action "Ricky" -12 "Fed geese") tree)
              (make-actionnode "Dj" 58
                               (list (make-action "Dj" 78 "Baked cupcakes for his family")
                                     (make-action "Dj" -20 "Ate all the cupcakes"))
                               (make-actionnode "Debra" 61 (list (make-action "Debra" 61 "Donated to charity"))
                                                empty empty)
                               (make-actionnode
                                "Ricky" 38
                                (list (make-action "Ricky" -12 "Fed geese")
                                      (make-action "Ricky" 50
                                                   "Bought lunch for a friend")) empty empty)))
(check-expect (add-action (make-action "Dj" 20 "Ate his vegatables") tree)
              (make-actionnode "Dj" 78 (list (make-action "Dj" 20 "Ate his vegatables")
                                             (make-action "Dj" 78 "Baked cupcakes for his family")
                                             (make-action "Dj" -20 "Ate all the cupcakes"))
                               (make-actionnode "Debra" 61
                                                (list (make-action "Debra" 61 "Donated to charity"))
                                                empty empty)
                               (make-actionnode "Ricky" 50
                                                (list (make-action "Ricky" 50 "Bought lunch for a friend"))
                                                empty empty)))


;; add-action: Action ActionST -> ActionST
(define (add-action action ast)
  (add-action/acc action ast true))

;; add-action/acc: Action ActionST Bool -> ActionST
(define (add-action/acc action ast closer?)
  (cond
    [(and (empty? ast) (not closer?)) empty]
    [(and (empty? ast) closer?)
     (make-actionnode (action-name action)
                      (action-niceness action)
                      (list action) empty empty)]
    [(string=? (actionnode-name ast) (action-name action))
     (make-actionnode (actionnode-name ast)
                      (+ (actionnode-score ast) (action-niceness action))
                      (cons action (actionnode-actions ast))
                      (add-action/acc action (actionnode-left ast) false)
                      (add-action/acc action (actionnode-right ast) false))]
    [closer? (make-actionnode (actionnode-name ast)
                              (actionnode-score ast)
                              (actionnode-actions ast)
                              (cond
                                [(string<? (action-name action) (actionnode-name ast))
                                 (add-action/acc action (actionnode-left ast) true)]
                                [else (add-action/acc action (actionnode-left ast) false)])
                              (cond
                                [(string>? (action-name action) (actionnode-name ast))
                                 (add-action/acc action (actionnode-right ast) true)]
                                [else (add-action/acc action (actionnode-right ast) false)]))]
    [else (make-actionnode (actionnode-name ast)
                           (actionnode-score ast)
                           (actionnode-actions ast)
                           (add-action/acc action (actionnode-left ast) false)
                           (add-action/acc action (actionnode-right ast) false))]))


;;4d
(define tree2 (make-actionnode "Debra" 15 (list (make-action "Debra" 15 "Shared Candy")) 
                               (make-actionnode "Bobby" -21 (list (make-action "Bobby" -21 "Fell asleep during english")) empty empty)
                               (make-actionnode "Gino" 10
                                                (list (make-action "Gino" 10 "Shared legos")) empty empty)))
;;(gift-list AST CL theshold) produces the gifts each child will recieve.
;;Examples
(check-expect (gift-list tree instr-childrenlist -20)
              (list (list "Ricky" (list "Playing Card Deck" "Math textbook" "Crayons" "Toy car"))
                    (list "Dj" (list "Playing Card Deck" "Pizza" "Cupcakes"))
                    (list "Debra" (list "Playing Card Deck" "Water bottle" "Barbie doll"))))
(check-expect (gift-list tree2 instr-childrenlist -20)
              (list (list "Gino" (list "Playing Card Deck"))
                    (list "Debra" (list "Playing Card Deck" "Water bottle"))
                    (list "Bobby" 'krampus)))
;;gift-list: ActionST ChildrenList Num -> (listof (listof Any))

(define (gift-list AST CL threshold)
  (sort-by-name (gift-list-acc AST CL threshold empty)))

(define (gift-list-acc AST CL threshold result)
  (cond [(empty? AST) result]
        [else (gift-list-acc (actionnode-left AST) CL threshold
                                (gift-list-acc (actionnode-right AST) CL threshold
                                                  (cons (list (actionnode-name AST)
                                                              (gifts-received (actionnode-name AST)
                                                                              (second (find-score (actionnode-name AST) AST))
                                                                              CL
                                                                              threshold))
                                                        result)))]))
                     
(define (find-score name AST) ;produces child's name and score 
  (cond [(empty? AST) empty]
        [(string=? name (actionnode-name AST)) (list (actionnode-name AST)
                                                     (actionnode-score AST))]
        [(string<? (actionnode-name AST) name) (find-score name (actionnode-left AST))]
        [else (find-score name (actionnode-right AST))]))

(check-expect (find-score "Ryan"
                          (make-actionnode "Jim" 15 (list 'action1 'action2)
                                           (make-actionnode "Ryan" 25 (list (make-action "Ryan" 25 "Ate an apple"))
                                                            empty empty)
                                           (make-actionnode "Abby" 40 (list 'something1 'something2) empty empty)))
              (list "Ryan" 25)) 
                   
         
              
(define (sort-by-name list)
  (cond [(empty? list) empty]
        [else (insert (first list) (sort-by-name (rest list)))]))

(check-expect (sort-by-name (list (list "John" (list 'action3))
                                  (list "Rick" (list 'action2))
                                  (list "Bob" (list 'action1))))
              (list (list "Bob" (list 'action1))
                    (list "John" (list 'action3))
                    (list "Rick" (list 'action2))))
(check-expect (sort-by-name (list
                             (list "Dj" (list "Playing Card Deck" "Pizza" "Cupcakes"))
                             (list "Ricky" (list "Playing Card Deck" "Math textbook" "Crayons" "Toy car"))
                             (list "Debra" (list "Playing Card Deck" "Water bottle" "Barbie doll"))))
              (list
               (list "Debra" (list "Playing Card Deck" "Water bottle" "Barbie doll"))
               (list "Dj" (list "Playing Card Deck" "Pizza" "Cupcakes"))
               (list "Ricky" (list "Playing Card Deck" "Math textbook" "Crayons" "Toy car"))))

(check-expect (sort-by-name (list (list "Dj" (list 'gift1 'gift2 'gift3))
                                  (list "Ricky" (list 'toy1 'toy2 'toy3 'toy4))
                                  (list "Debra" (list 'thing1 'thing2 'thing3))))
              (list (list "Debra" (list 'thing1 'thing2 'thing3))
                    (list "Dj" (list 'gift1 'gift2 'gift3))
                    (list "Ricky" (list 'toy1 'toy2 'toy3 'toy4))))


(define (insert n list)
  (cond [(empty? list) (cons n empty)]
        [(string>? (first n) (first (first list))) (cons n list)]
        [else (cons (first list)
                    (insert n (rest list)))]))




