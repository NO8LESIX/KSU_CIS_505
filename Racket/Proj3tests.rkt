#lang racket

(require "Proj3.rkt")

(define p01
  "let plus lambda x lambda y + x y\
   let plussix apply plus 6\
   apply plussix 8")
(define o01 14)

(define p02
  "let twice lambda f lambda x apply f apply f x\
   let multfive lambda x * 5 x\
   apply apply twice multfive 3")
(define o02 75)

(define p03
  "let fac\
     lambda n\
        cond n\
             * n\
               apply fac\
                     - n 1\
             1\
    apply fac 5")
(define o03 (void))

(define p04
  "let Z lambda f apply lambda x apply f lambda v apply apply x x v \
                     lambda x apply f lambda v apply apply x x v\
   let Fac\
     lambda f lambda n\
       cond n\
            * n\
              apply f\
                    - n 1\
            1\
   apply apply Z Fac 5")
(define o04 120)

(define p05 "+ 'ab''cde'")
(define o05 "abcde")

(define p06 "- 'abcabc' 'dbd'")
(define o06 "acac")

(define p07 "* 4 'again '")
(define o07 "again again again again ")

(define p08
  "apply\
     * 3\
       lambda w + w 2\
     7")
(define o08 27)

(define p09
  "let f * 3\
        lambda w + - w '&'\
                   '&'\
   apply f 'bread&butter'")
(define o09 "breadbutter&breadbutter&breadbutter&")

(define progs (list p01 p02 p03 p04 p05 p06 p07 p08 p09))
(define outs-expected (list o01 o02 o03 o04 o05 o06 o07 o08 o09))


(define outs-actual (map run progs))

(define (compare n left right)
  (let inner ([i 1] [left left] [right right])
  (if (> i n)
      null
      (cons (cons i (equal? (car left) (car right)))
            (inner (+ i 1) (cdr left) (cdr right))))))

(define results (compare 9 outs-expected outs-actual))
