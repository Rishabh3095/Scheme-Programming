;;; CS 152 Homework 4 - A simple chatbot
;;; starter code

;;; We'll use the random function implemented in Racket
;;; (random k) returns a random integer in the range 0 to k-1
(#%require (only racket/base random))

;;; some input and output helper functions

;;; prompt:  prompt the user for input
;;; return the input as a list of symbols
(define (prompt)
   (newline)
   (display "talk to me >>>")
   (read-line))

;;; read-line: read the user input till the eof character
;;; return the input as a list of symbols
(define (read-line)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-line)))))

;;; output: take a list such as '(how are you?) and display it
(define (output lst)
       (newline)
       (display (to-string lst))
       (newline))

;;; to-string: convert a list such as '(how are you?)
;;; to the string  "how are you?"
(define (to-string lst)       
  (cond ((null? lst) "")
        ((eq? (length lst) 1) (symbol->string (car lst)))
        (else (string-append (symbol->string (car lst))
                              " "
                             (to-string (cdr lst))))))


;;;  main function
;;;  usage:  (chat-with 'your-name)

(define (chat-with name)
  (output (list 'hi name))
  (chat-loop name))

;;; chat loop
(define (chat-loop name)
  (let ((input (prompt))) ; get the user input
    (if (eqv? (car input) 'bye)
        (begin
          (output (list 'bye name))
          (output (list 'have 'a 'great 'day!)))
        (begin
	  (reply input name)
          (chat-loop name)))))


;;; your task is to fill in the code for the reply function
;;; to implement rules 1 through 11 with the required priority
;;; each non-trivial rule must be implemented in a separate function
;;; define any helper functions you need below 
(define (reply input name)
  (cond
    ((checkONE? input) (output (pick-random (ruleOneOutput input name))))
    ((ruletwo? input) (output (append (list 'tell 'me 'about 'your) (list (pick-random (ruleTwo input))) (list name))))
    ((checkTHREE? input) (output (list 'why 'not '?)))
    ((checkFOUR? input) (output (pick-random rule-four)))
    ((checkFIVE? input) (output (pick-random rule-five)))
    ((checkSix? (lastElement input)) (output (pick-random rule-six)))
    ((in? 'because input) (output (list 'is 'that 'the 'real 'reason '?)))
    ((rule-8? input) (output (rule-8-join input))) 
    ((checkNine? input) (output (append input (list 'too))))
    ((in? (car input) '(tell give say)) (output (append (list 'you) input)))
    (else (output (pick-random generic-response))) ; rule 11 has been implemented for you
    ))


;extra question mark

(define (checkONE? input)
  (if (and (checkOne? input) (checkSix? (lastElement input))) #t #f))

(define (checkTHREE? input)
  (if (and (eqv? 'why (car input)) (checkSix? (lastElement input))) #t #f))

(define (checkFOUR? input)
  (if (and (eqv? 'how (car input)) (checkSix? (lastElement input))) #t #f))

(define (checkFIVE? input)
  (if (and (eqv? 'what (car input)) (checkSix? (lastElement input))) #t #f))

;rule 8
(define (rule-8? input)
  (if (and (eqv? 'i (car input)) (in? (cadr input) '(need think have want))) #t #f))

;rule-8 pieces
(define (rule-8-join input)
  (append '(why) '(do) '(you) (list (cadr input)) (notLast (changePerson (cddr input))) (add_? (changePerson (list (lastElement input))))))

;rule two
(define (ruleTwo input)
  (cond
    ((null? input) '())
    ((in? (car input) (list 'family 'friend 'friends 'mom 'dad 'brother 'sister 'girlfriend 'boyfriend 'children 'son 'daughter 'child 'wife 'husband 'home 'dog 'cat 'pet)) (cons (car input) (ruleTwo (cdr input))))
    (else (ruleTwo (cdr input)))))

;check rule two
(define (ruletwo? input)
  (cond
    ((null? input) #f)
    ((in? (car input) (list 'family 'friend 'friends 'mom 'dad 'brother 'sister 'girlfriend 'boyfriend 'children 'son 'daughter 'child 'wife 'husband 'home 'dog 'cat 'pet)) #t)
    (else (ruletwo? (cdr input)))
    ))


;checking rule one
(define (checkOne? input)
  (if (and (in? (car input) '(do can will would)) (eqv? (cadr input) 'you)) #t #f))

;pieces of rule one together
(define (ruleOneOutput input name)
  (list (append '(no) (list name) '(i) (list (car input)) '(not) (notLast (changePerson (cddr input))) (changePerson (remove_? (lastElement  input)))
          )
        (append '(yes) '(i) (list (car input)))))

;change person
(define (changePerson input)
  (cond
    ((null? input) '())
    ((eqv? 'i (car input)) (cons 'you (changePerson (cdr input))))
    ((eqv? 'am (car input)) (cons 'are (changePerson (cdr input))))
    ((eqv? 'my (car input)) (cons 'your (changePerson (cdr input))))
    ((eqv? 'your (car input)) (cons 'my (changePerson (cdr input))))
    ((eqv? 'me (car input)) (cons 'you (changePerson (cdr input))))
    ((eqv? 'you (car input)) (cons 'me (changePerson (cdr input))))
    (else (cons (car input) (changePerson (cdr input))))
    ))


;remove the question mark from the input
(define (remove_? input)
  (list (string->symbol (substring (symbol->string input) 0 (- (string-length (symbol->string input)) 1)))))


;add the question mark to the input
(define (add_? input)
  (list (string->symbol (string-append (symbol->string (car input)) "?"))))

;everything other than the last element
(define (notLast input)
  (cond
    ((null? (cdr input)) '())
    (else (cons (car input) (notLast (cdr input))))))


;get the last element
(define (lastelement input)
  (cond
    ((null? (cdr input)) (car input))
    (else (lastelement (cdr input)))))

;checks if the element is in the list
(define (in? e s)
  (cond
    ((null? s) #f)
    ((eqv? e (car s)) #t)
    (else(in? e (cdr s)))))


;;; pick one random element from the list choices
(define (pick-random choices)
  (list-ref choices (random (length choices))))

  ;;; generic responses for rule 4
(define rule-four '((why do you ask?)
                    (how would an answer to that help you?)))

  
  ;;; generic responses for rule 5
(define rule-five '((why do you ask?)
                    (what do you think?)))

;;;check Six
(define (checkSix? input)
  (if (string=? (substring (symbol->string input) (- (string-length (symbol->string input)) 1) (string-length (symbol->string input))) "?") #t #f))

  ;;; generic responses for rule 6
(define rule-six '((i don't know)
                    (i have no idea)
                    (i have no clue)
                    (maybe)))

  ;;; generic responses for rule 11
(define generic-response '((that\'s nice)
                           (good to know)
                           (can you elaborate on that?)))

;;;check Nine
(define (checkNine? input)
  (if (and (eqv? 'i (car input)) (checkToo? input)) #t #f))

(define (checkToo? input)
  (if (eqv? (lastElement input) 'too) #f #t))