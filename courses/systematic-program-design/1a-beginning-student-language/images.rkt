;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname images) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;(circle 10 "solid" "red")
;(rectangle 30 60 "outline" "blue")
;(text "hello" 24 "orange")

(overlay (circle 10 "solid" "red")
       (circle 20 "solid" "yellow")
       (circle 30 "solid" "green"))

(above (beside (triangle 20 "solid" "orange") (triangle 20 "solid" "orange")) (overlay (above (beside (circle 5 "solid" "black") (circle 3 "solid" "orange") (circle 5 "solid" "black")) (rectangle 5 2 "solid" "black")) (square 40 "solid" "orange") ) )

(above (triangle 30 "solid" "green") (triangle 60 "solid" "green") (triangle 80 "solid" "green") (rectangle 10 30 "solid" "brown"))
