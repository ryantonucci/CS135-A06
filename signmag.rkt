;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname signmag) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;CS135 F23
;;Assignment 6
;;2a

(define-struct signmag (sign mag))
;;A SignMag is a (make-signmag Sym Num)
;;requires: sign is one of 'positive, 'negative, 'zero
;;mag > 0
;;if sign is 'zero, then mag = 1

;;2b
;;(signmag-template a-signmag) is a template for signmag
;;signmag-template: SignMag->Any
(define (signmag-template a-signmag)
  (... (signmag-sign a-signmag)
       (signmag-mag a-signmag)...))

;;2c
;;(num->signmag number) produces the number's equivalent signmag value
;;Examples
(check-expect (num->signmag 16) (make-signmag 'positive 16))
(check-expect (num->signmag 0) (make-signmag 'zero 1))
;;num->signmag: Num->signmag

(define zero 0)

(define (num->signmag number)
  (cond [(> number zero) (make-signmag 'positive number)]
        [(< number zero) (make-signmag 'negative (abs number))]
        [else (make-signmag 'zero 1)]))

(check-expect (num->signmag -22) (make-signmag 'negative 22))

;;2d
;;(signmag->num a-signmag) produces the number as a real number.
;;Examples
(check-expect (signmag->num (make-signmag 'zero 1)) 0)
(check-expect (signmag->num (make-signmag 'positive 99)) 99)
;;signmag->num: signmag->Num

(define (signmag->num a-signmag)
  (cond [(symbol=? (signmag-sign a-signmag) 'zero) zero]
        [(symbol=? (signmag-sign a-signmag) 'negative)
         (- (signmag-mag a-signmag) (* 2 (signmag-mag a-signmag)))]
        [else (signmag-mag a-signmag)]))

(check-expect (signmag->num (make-signmag 'negative 42)) -42)