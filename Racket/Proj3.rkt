#lang racket

; === Parser and Interpreter for the language given in
;           Project 3, CIS505/705, 
;        Kansas State University, Fall 2019
;
; This is a skeleton, with 
;     9 places where code needs to be changed/inserted.
; Each such place is marked with "CHANGE #k" where
;     k is a number that indicates the suggested priority 
;     (lower numbers mean higher priority).
; 
; #1: this is about how to evaluate an identifier;
;      it should be looked up in the environment
; 
; #2: this is about how to evaluate a function definition;
;      the appropriate closure should be created
;
; #3: this is about how to evaluate a function application,
;       after the function part has been evaluated to a closure
;       and after the argument part has been evaluated;
;      this involves calling the closure body in an environment
;       that implements static scope 
;        (cf. the slides on implementing higher-order-functions
;         that we will discuss in detail in class)
;
; #4: this is about how to evaluate a let expression
;      (which you should do by replacing it with an equivalent expression)
;
;  At this point, you can handle programs without if-expressions,
;    and without strings.
;
; #5: this is about how to parse if-expressions;
;      you should take inspiration from how other constructs are parsed
;
; #6: this is about which expressions are considered true and which
;      are considered false (this is specified in the question text).
;
;  Finally, we can embark on how to handle strings:
;
; #7: this is about how to extend the evaluation of '+' to 
;       allow also operands that are both strings
;
; #8: this is about how to extend the evaluation of '-' to 
;       allow also operands that are both strings
;      (and report error otherwise)
;
; #9: this is about how to extend the evaluation of  '*' 
;       according to the description in the question text.

; --- what is visible to the outside

(provide run-lexer)
(provide run-parser)
(provide run)

; --- syntax

; exp ::= id
;      |  num
;      |  '<characters>'
;      |  "lambda" id exp
;      |  "apply" exp1 exp2
;      |  "let" id exp1 exp2
;      |  "cond" exp1 exp2 exp3
;      |  op exp1 exp2 

;  op ::= "+"  (overloaded)
;      |  "-"  (overloaded)
;      |  "*"  (overloaded)
;
; === lexical analysis

; --- tokens

(struct IdT (string))
(struct NumT (num))
(struct StringT (string))
(struct LambdaT ())
(struct ApplyT ())
(struct LetT ())
(struct CondT ())
(struct PlusT ())
(struct MinusT ())
(struct TimesT ())

(define (char->digit ch)
  (- (char->integer ch) (char->integer #\0)))

(define (lexer chars)
   (if (null? chars)
      null
      (let ([current (first chars)] [remain (rest chars)])
         (cond
           [(eq? current #\+) (cons (PlusT) (lexer remain))]
           [(eq? current #\-) (cons (MinusT) (lexer remain))]
           [(eq? current #\*) (cons (TimesT) (lexer remain))]
           [(eq? current #\') (string-state '() remain)]
           [(eq? current #\space) (lexer remain)]	   	   
           [(eq? current #\newline) (lexer remain)]	   	   
           [(char-numeric? current) (num-state (char->digit current) remain)]
           [(char-alphabetic? current) (ident-state (list current) remain)]
           [else (display (string-append "unknown symbol "
                                         (list->string (list current)) "\n"))]
	))))

(define (num-state seen chars)
   (if (and (pair? chars) (char-numeric? (first chars)))
      (num-state (+ (* 10 seen) (char->digit (first chars))) (rest chars))
      (cons (NumT seen) (lexer chars))
    ))

(define (ident-state seen chars)
   (if (and (pair? chars) 
            (or (char-alphabetic? (first chars))
                (char-numeric? (first chars))))
      (ident-state (append seen (list (first chars))) (rest chars))
      (cons (mk-alpha-token (list->string seen)) (lexer chars))
   ))

(define (string-state seen chars)
   (if (pair? chars) 
       (if (eq? #\' (first chars))
           (cons (StringT (list->string seen)) (lexer (rest chars)))
           (string-state (append seen (list (first chars))) (rest chars)))
       (display (string-append "nothing closes the string "
	                       (list->string seen) "\n"))))

(define (mk-alpha-token seen)
   (cond
      [(equal? seen "lambda") (LambdaT)]
      [(equal? seen "apply") (ApplyT)]
      [(equal? seen "let") (LetT)]
      [(equal? seen "cond") (CondT)]
      [else (IdT seen)]
     ))

(define (run-lexer x) (lexer (string->list x)))

; === parsing

; --- syntax trees

(struct IdentExp (var))
(struct NumExp (num))
(struct StringExp (string))
(struct LambdaExp (formal body))
(struct ApplyExp (fun arg))
(struct LetExp (id exp0 body))
(struct CondExp (test exp1 exp2))
(struct PlusExp (exp1 exp2))
(struct MinusExp (exp1 exp2))
(struct TimesExp (exp1 exp2))

(define (parExpectIdent error-msg tks)
   (if (and (pair? tks) (IdT? (first tks)))
      (values (IdT-string (first tks)) (rest tks))
      (display error-msg)
   ))

(define (parExp tks)
   (if (pair? tks)
      (let ([tk (first tks)] [tks0 (rest tks)])
         (cond
            [(IdT? tk)
               (values (IdentExp (IdT-string tk)) tks0)]
            [(NumT? tk)
               (values (NumExp (NumT-num tk)) tks0)]
            [(StringT? tk)
               (values (StringExp (StringT-string tk)) tks0)]
            [(LambdaT? tk)
               (let*-values (
                 [(id tks1) 
                     (parExpectIdent "identifier expected after 'lambda'\n" tks0)]
                  [(e tks2) (parExp tks1)])
         	 (values (LambdaExp id e) tks2))]
            [(ApplyT? tk)
               (let*-values (
                  [(e1 tks1) (parExp tks0)]
         	  [(e2 tks2) (parExp tks1)])
        	 (values (ApplyExp e1 e2) tks2))]
            [(LetT? tk)
               (let*-values (
                  [(id tks1) 
                     (parExpectIdent "identifier expected after 'let'\n" tks0)]
                  [(e1 tks2) (parExp tks1)]
                  [(e2 tks3) (parExp tks2)])
        	 (values (LetExp id e1 e2) tks3))]
            [(CondT? tk)
               (let*-values ( ;;; CHANGE #5 something is wrong, but I can't figure out what
                  [(e1 tks1) (parExp tks0)]
        	  [(e2 tks2) (parExp tks1)]
        	  [(e3 tks3) (parExp tks2)])
		 (values (CondExp e2 e2 e3) tks3))]
 	    [(PlusT? tk)
               (let*-values (
                  [(e1 tks1) (parExp tks0)]
                  [(e2 tks2) (parExp tks1)])
 		 (values (PlusExp e1 e2) tks2))]
 	    [(MinusT? tk)
               (let*-values (
                  [(e1 tks1) (parExp tks0)]
                  [(e2 tks2) (parExp tks1)])
        	 (values (MinusExp e1 e2) tks2))]
           [(TimesT? tk)
               (let*-values (
                  [(e1 tks1) (parExp tks0)]
                  [(e2 tks2) (parExp tks1)])
                 (values (TimesExp e1 e2) tks2))]
          ))
      (display "expression expected\n")
   ))
   
(define (parse tks)
   (let-values ([(main tks1) (parExp tks)])
      (if (null? tks1)
         main
         (display "program read but more input given\n"))
    ))

(define (run-parser x) (parse (run-lexer x)))

; === evaluating (abstract) syntax

; --- helper methods

(define (string-replicate n s)
  (cond
    [(< n 1) "number of duplications must be 1 or greater"]
    [(= n 1) s]
    [else (string-append s (string-replicate (- n 1) s))]
    )
  )
(define (string-sub s1 list2)
  (cond
    [(null? s1) "Cannot subtract s2 because null"]
    [(null? list2) s1]
    [(eq? s1 "") s1]
    [else (string-sub (string-replace s1 (first list2) "") (rest list2))]
    )
  )

; --- values

(struct NumVal (num))
(struct ClosureVal (formal body env))
(struct StringVal (str))

(define (extend-env id val env)
  (cons (cons id val) env)
 )

(define (lookup-env env id)
  (cond
     [(null? env)
        (display (string-append "undefined identifier " id "\n"))]
     [(equal? id (car (first env)) )
        (cdr (first env))]
     [else (lookup-env (rest env) id)]
  ))

(define (eval exp env)
   (cond
      [(IdentExp? exp)
       (lookup-env env (IdentExp-var exp))]  ;;; CHANGE #1 finished
      [(NumExp? exp) 
          (NumVal (NumExp-num exp))]
      [(StringExp? exp) 
          (StringVal (StringExp-string exp))]
      [(LambdaExp? exp)
       (ClosureVal (LambdaExp-formal exp) (LambdaExp-body exp) env)];;; CHANGE #2 finished
      [(ApplyExp? exp)
          (let ([v1 (eval (ApplyExp-fun exp) env)])
             (cond
                [(ClosureVal? v1)
                   (let ([v2 (eval (ApplyExp-arg exp) env)])
                     (eval (ClosureVal-body v1) (extend-env (ClosureVal-formal v1) v2 (ClosureVal-env v1)))
                     )
                   ];;; CHANGE #3 finished?
                [(NumVal? v1) (display "integer applied as a function\n")]
                [(StringVal? v1) (display "string applied as a function\n")]))]
      [(LetExp? exp)
          (eval (ApplyExp (LambdaExp (LetExp-id exp) (LetExp-body exp))
                          (LetExp-exp0 exp))  ;;; CHANGE #4 finished?
                 env)]
      [(CondExp? exp)
          (let ([v (eval (CondExp-test exp) env)])         
             (if (NumVal? v) ;;; CHANGE #6
                 (eval (CondExp-exp1 exp) env)
                 (eval (CondExp-exp2 exp) env))
             (if (StringVal? v) ;;; CHANGE #6
                 (eval (CondExp-exp1 exp) env)
                 (eval (CondExp-exp2 exp) env))
            )]
      [(PlusExp? exp)
          (let ([v1 (eval (PlusExp-exp1 exp) env)]
                [v2 (eval (PlusExp-exp2 exp) env)])
            (cond
               [(and (NumVal? v1) (NumVal? v2))
                   (NumVal (+ (NumVal-num v1) (NumVal-num v2)))]
               [(and (StringVal? v1) (StringVal? v2))
                (StringVal (string-append (StringVal-str v1) (StringVal-str v2)))] ;;; CHANGE #7 Finished
               [else (display "operands to '+' must be either both numbers or both strings\n")]))]
      [(MinusExp? exp)
          (let ([v1 (eval (MinusExp-exp1 exp) env)]
                [v2 (eval (MinusExp-exp2 exp) env)])
            (cond
               [(and (NumVal? v1) (NumVal? v2))
                   (NumVal (- (NumVal-num v1) (NumVal-num v2)))]
               [(and (StringVal? v1) (StringVal? v2))
                (StringVal (string-sub (StringVal-str v1) (string-split(StringVal-str v2) "")))]
               [else (display "operands to '-' must be either both numbers or both strings\n")]
               ;(StringVal "something else!")] ;;; CHANGE #8
              ))]
      [(TimesExp? exp)
          (let ([v1 (eval (TimesExp-exp1 exp) env)]
                [v2 (eval (TimesExp-exp2 exp) env)])
            (if (NumVal? v1)
                (cond
                   [(NumVal? v2)
                       (NumVal (* (NumVal-num v1) (NumVal-num v2)))]
                   [(StringVal? v2)
                    (StringVal (string-replicate (NumVal-num v1) (StringVal-str v2)))]
                   [(ClosureVal? v2)
                    (ClosureVal (ClosureVal-formal v2) (TimesExp-exp1 exp) env)
                    ;(ClosureVal (ClosureVal-formal v2) (TimesExp-exp1 (eval (ClosureVal-body v2) v1)) env)
                    ;(eval (ClosureVal-formal v2) (extend-env (ClosureVal-formal v2) v1 (ClosureVal-env v1))
                    ;(ClosureVal (LambdaExp-formal exp) (LambdaExp-body exp) env
                    ;I am at a lost to what was the correct solution is.
                    ]
                   [else  ;;; CHANGE #9
                       (display "operands to v2 of '*' must be either a number, string, or function\n")
                       ]
                )
                (display "the first operand to '*' must be a number\n")))]
 ))

(define (run x)
   (let ([main (run-parser x)])
     (let ([v (eval main null)])
       (cond
         [(NumVal? v) (NumVal-num v)]
         [(StringVal? v) (StringVal-str v)]
         [(ClosureVal? v) (display "program cannot return a function\n")])
   )))
