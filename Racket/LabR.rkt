#lang racket
(provide lookup)
(provide Leaf)
(provide Node)
(provide average)
(provide SumTree)
(provide TotalLeaves)
(provide test0)

(define test0 `(("a" . 4) ("b" . 2) ("a" . 5)))
;Leaf -> int
[struct Leaf (num)]
;Node -> Leaf * Node | Leaf * Leaf | Node * Node
[struct Node (left right)]

;Takes some TreeNode and adds up all Leaves returning the sum
[define (SumTree root)
	(cond
		([null? root] 0)
		([Leaf? root] (+ (Leaf-num root)))
		([Node? root] (+ (SumTree (Node-left root)) (SumTree (Node-right root))))
	)	
]
;Add all of the nodes in the tree together
[define (TotalLeaves root )
	(cond
		[(null? root) 0]
		[(Leaf? root) 1]
		[(Node? root) (+ (TotalLeaves (Node-left root)) (TotalLeaves (Node-right root)))]
	)
]

[define (average root)
	(cond
		([null? root] 0)
		([Leaf? root] (+ (Leaf-num root)))
                ([Node? root] (/ (+ (SumTree (Node-left root)) (SumTree (Node-right root)) ) (+ (TotalLeaves (Node-left root)) (TotalLeaves (Node-right root)))))
	)

]
[define (lookup dic value)
	(cond
                [(null? dic) " k e y ␣ n o t ␣ f o u n d "]
		[(list? dic) (if(eq? (car (first dic)) value) (cdr (first dic)) (lookup (rest dic) value)) ]
                
	)
]

