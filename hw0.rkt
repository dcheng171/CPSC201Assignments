#lang racket
; ********************************************************
; CS 201a HW #0  DUE Sept 7, 2020 11:59 pm
; ** using the Zoo submit system **

; ********************************************************
; Name: David Cheng
; Email address: david.cheng@yale.edu
; ********************************************************

; This file may be loaded into DrRacket.  
; Lines beginning with semicolons are Racket comments.

; Homework #0 will be worth 20 points -- other homeworks will be 
; worth 100 points.

; One purpose of homework #0 is to make sure you can use the 
; submit system on the Zoo.  You will receive no credit for this 
; assignment unless you successfully use the submit system to submit it.

; You will be submitting two files for homework #0.
; You must name them (with correct case and extension):

; hw0.rkt (for the Racket definitions and procedures)
; response.txt (for the reading response)

; There will be NO credit for incorrectly named files.

; ********************************************************
; ** problem 0 ** (1 easy point) 
; replace the number 0 in this definition to
; indicate how many hours you spent doing this assignment.
; Round to the nearest integer.

(define hours 2)

; ********************************************************
; ** problem 1 ** (5 points)

; Write a procedure (degreeF-to-degreeC F)
; that takes as an argument a number representing
; a temperature in Fahrenheit F, and returns a number
; representing the same temperature in
; degrees Celsius.

; Note that returning a value does not mean displaying or printing it.

; Examples

; (degreeF-to-degreeC 32) =>  0
; (degreeF-to-degreeC 212) => 100
; (degreeF-to-degreeC 98.6) => 37.0

; ********************************************************

(define (degreeF-to-degreeC x) (* (- x 32) (/ 5 9)))

; ********************************************************
; ** problem 2 ** (4 points)

; Write a procedure (greeting) that takes no arguments and
; returns a *string* with a common greeting.

; Example (your procedure can produce a different string)

; (greeting) => "good morning"
; ********************************************************

(define (greeting) "what is up")

; Your procedure will be tested automatically, and will 
; be called only with *no* arguments.  Note that a
; Racket symbol is not a string.

; ********************************************************
; ** problem 3 ** (10 points)

; For this problem, read
; "Why Computing Belongs Within the Social Sciences"
; https://cacm.acm.org/magazines/2020/8/246368-why-computing-belongs-within-the-social-sciences/fulltext
; You can access this article through Yale VPN
; Answer these two questions:

; a.) What is one reason that the author gives for why computing is a social science?
; b.) What is something the author recommends changing in computer science?

; Your answer should be *at least* 100 words and  *at most* 400 words, as counted by the 
; wc utility on the Zoo, saved in .txt format, and submitted as
; the file response.txt for assignment 0.  Please include your name 
; and email address in the file. 
; This problem will be graded as done (10 points) or not done (0 points) by running
; > wc response.txt
; and marking the result correct if the returned value for number of words is between 100 and 400 (inclusive).
; We will read your responses as part of getting to know you. If you write nonsense you will make an impression
; and are likely to hear from us.

; Note that if you prepare your answer in a word-processing program, 
; you'll probably need to save it as (plain) Text (.txt) to get the
; correct format.

; ********************************************************
; ********  end of homework #0
; ********************************************************
