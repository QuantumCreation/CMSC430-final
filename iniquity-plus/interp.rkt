#lang racket
(provide interp interp-env)
(require "ast.rkt"
         "env.rkt"
         "interp-prims.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (vector Value ...)
;; | (string Char ...)

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; Prog -> Answer
(define (interp p)
  (match p
    [(Prog ds e)
     (interp-env e '() ds)]))

;; Expr Env Defns -> Answer
(define (interp-env e r ds)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x)  (display x) (display (symbol? x)) (lookup r x)]
    [(Str s)  (string-copy s)]
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim1 p e)
     (match (interp-env e r ds)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (match (interp-env e3 r ds)
                   ['err 'err]
                   [v3 (interp-prim3 p v1 v2 v3)])])])]
    [(If p e1 e2)
     (match (interp-env p r ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds)
            (interp-env e2 r ds))])]
    [(Begin e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [_    (interp-env e2 r ds)])]
    [(Let x e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v (interp-env e2 (ext r x v) ds)])]

    [(Values vs)
     (match (interp-list vs r ds)
      ['err 'err]
      [v (apply values v)]     
     )]

    ; [(LetValues es expression)
    ;   ; (let-values ([(x y) (values 1 2)]) 1)
    ; ;  (let-values (interp-let-values es r ds) (interp-env expression r ds))
    ;  (let-values (interp-let-values es r ds) (interp-env expression r ds))
    ;  ]

    [(App f es)
     (match (interp-env* es r ds)
       ['err 'err]
       [vs
        (match (defns-lookup ds f)
          [(Defn _ fun)
           (apply-fun fun vs ds)])])]
    [(Apply f es e)
     (match (interp-env* es r ds)
       ['err 'err]
       [vs
        (match (interp-env e r ds)
          ['err 'err]
          [ws
           (if (list? ws)
               (match (defns-lookup ds f)
                 [(Defn _ fun)
                  (apply-fun fun (append vs ws) ds)])
               'err)])])]))



(define (interp-list-to-symbols es)
(match es
['() '()]
[(cons (Var v) rest) (cons v (interp-list-to-symbols rest))]
[_ 'err]
)
)

(define (interp-let-values vs r ds)
(match vs
['() '()]
[(cons (list vars vals) rest) 
        (match (interp-list-to-symbols vars r ds)
          ['err 'err]
          [a (match (interp-env vals r ds)
              ['err 'err]
              [b (cons (a b) (interp-let-values rest r ds))]
            )
  ])
]
[_ 'err]
)
)

(define (interp-list vs r ds)
(match vs
['() '()]
[(cons v rest) 
          (match (interp-env v r ds)
            ['err 'err]
            [v (cons v (interp-list rest r ds))])
]
[_ 'err]
)
)

;; Fun [Listof Values] Defns -> Answer
(define (apply-fun f vs ds)
  (match f
    [(FunPlain xs e)
     ; check arity matches-arity-exactly?
     (if (= (length xs) (length vs))
         (interp-env e (zip xs vs) ds)
         'err)]    
    [(FunRest xs x e)
     ; check arity is acceptable
     (if (< (length vs) (length xs))
         'err
           (interp-env e
                       (zip (cons x xs)
                            (cons (drop vs (length xs))
                                  (take vs (length xs))))
                       ds))]    
    [(FunCase cs)
     (match (select-case-lambda cs (length vs))
       ['err 'err]
       [f (apply-fun f vs ds)])]))

;; [Listof FunCaseClause] Nat -> Fun | 'err
(define (select-case-lambda cs n)
  (match cs
    ['() 'err]
    [(cons (and (FunPlain xs e) f) cs)
     (if (= (length xs) n)
         f
         (select-case-lambda cs n))]
    [(cons (and (FunRest xs x e) f) cs)
     (if (<= (length xs) n)
         f
         (select-case-lambda cs n))]))           

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds)
       ['err 'err]
       [v (match (interp-env* es r ds)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Defns Symbol -> Defn
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
