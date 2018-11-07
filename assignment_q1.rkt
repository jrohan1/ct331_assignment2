#lang racket
;a cons pair of two numbers
(cons 1 2)

;a list of 3 numbers using only cons function
(cons 1(cons 2 (cons 3 empty)))

;a list containing a string, a number and a nested list of three numbers using cons function
(cons 'string(cons 1(cons (cons 2(cons 3(cons 4 empty))) empty)))

;a list containing a string, a number and a nested list of three numbers using list function
(list 'string 1 (list 2 3 4))

;a list containing a string, a number and a nested list of three numbers using append function
(append '(string) '(1) '((2 3 4)))

