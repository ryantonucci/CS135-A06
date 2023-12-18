;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bst-full) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;Assignment 6

;;Q3a
(define-struct node (key left right))
;; A Node is a (make-node Nat BST BST)
;; requires: key > every key in left BST ;; key < every key in right BST
;; A Binary Search Tree (BST) is one of:
;; * empty
;; * Node
;;(full? BST)

(check-expect (full? (make-node 6 (make-node 5 (make-node 3 empty empty) empty) (make-node 7 empty empty))) false)
(check-expect (full? (make-node 9 (make-node 7 (make-node 3 empty empty) (make-node 8 empty empty))
                                (make-node 9 (make-node 8 empty empty)
                                           (make-node 11 empty empty)))) true)
;;full?: BST -> Bool

(define (full? bst)
  (cond
    [(empty? bst) true]
    [(and (empty? (node-left bst)) (empty? (node-right bst))) true]
    [(or (and (node? (node-left bst)) (empty? (node-right bst)))
         (and (empty? (node-left bst)) (node? (node-right bst)))) false]
    [(and (node? (node-left bst)) (node? (node-right bst)))
     (and (full? (node-left bst))
          (full? (node-right bst)))]))

  
(check-expect (full? (make-node 5 empty (make-node 4 empty empty))) false) 
(check-expect (full? empty) true)
(check-expect (full? (make-node 5 empty empty)) true)

;;3b
(define bad-full-tree
  (make-node 2 (make-node 1 empty empty)
             (make-node 4 (make-node 3 empty empty)
                        (make-node 6 (make-node 5 empty empty) (make-node 8 (make-node 7 empty empty)
                                                  (make-node 9 empty empty))))))