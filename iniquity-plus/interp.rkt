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
    [(Var x) (lookup r x)]
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

    [(LetValues es expression)
      ; (let-values ([(x y) (values 1 2)]) 1)
    ;  (let-values (interp-let-values es r ds) (interp-env expression r ds))
    ;  (let-values (interp-let-values es r ds) (interp-env expression r ds))
    ; (display "\n")
      ; (display es)
      ; (display es)
      ; (display es)
      ; (display "\n")
      ; (display (let-values-list es r ds))
      ; (display (zip-single (to-single-list (let-values-list es r ds))))
      ; (display
      ; (env-from-list
      ;     (zip-single (to-single-list (let-values-list es r ds)))
      ;     r
      ;     ds
      ;   )
      ; )
      (match
        (env-from-list
          (zip-single (to-single-list (let-values-list es r ds)))
          r
          ds
        )
      ['err 'err]
      [new_env (interp-env expression new_env ds)]
      )
      

      ;  
      ; expression 
      ; (env-from-list 
      ;       (zip-single (to-single-list (let-values-list es r ds)))
      ;       r ds)
      ; ds)
      ; (display (car es))
    ; (interp-env 
    ; expression 
    ; (env-from-list (zip-single ) r ds)
    ;  ds)
     ]

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
               'err)])])]
      ; For when I pass already interpreted things into this
      ; Not good design, but it might work. Hopefully.
      ; [anything anything]         
      ))


; (define (convert-values-to-list values)
;
; )


(define (interp-list-to-symbols es r ds)
  (match es
    ['() '()]
    [(cons (Var v) rest) (cons v (interp-list-to-symbols rest r ds))]
    [_ 'err]
  )
)

(define (same-length? a b)
  (= (length a) (length b))
)



; Converts let-values into lists of variables and vals
(define (let-values-list vs r ds)
  (match vs
    ['() '()]
    [(cons (list vars vals) rest) 
            ; Get list of symbols
          (match (interp-list-to-symbols vars r ds)
            ['err 'err]
            ; Convert values to a list
            [a
                (match  (call-with-values (lambda () (interp-env vals r ds)) list)
                  ['err 'err]
                  ; Check if they have matching lengths
                  [b 
                      (if (same-length? a b)
                      ; If so, continue and recurse until all symbol and value pairs are retrieved
                      (match (let-values-list rest r ds)
                        ['err 'err]
                        [lst (cons (list a b) lst)]
                      )
                      'err
                      )
                  ]
                )
            ])
    ]
    [_ 'err]
  )
)

; Merge every list in a list of lists
(define (merge lst)
  (match lst
    ['() '()]
    [(cons a rest) (append a (merge rest))]
    ['err 'err]
  )
)

; The input list is a list (cons (a b) rest), converted to a list holding all of the first values
; and a second list holding the second values
(define (to-single-list lst)
  (match lst
    ['err 'err]
    [_ (list (merge (map car lst)) (merge (map cadr lst)))]
  )
)

; Zip two lists held in lst
(define (zip-single lst)
  (match lst
    ['err 'err]
    [_ (map list (car lst) (cadr lst))]
  )   
)

; Create environment from the given variables list
(define (env-from-list lst r ds)
  (match lst
    ['err 'err]
    ['() r]
    [(cons (list var val) rest) 
          (match (env-from-list rest r ds)
            ['err 'err]
            [r_new (ext r_new var val)])
    ]
  )
)

; Interp each value from a list and return it
(define (interp-list vs r ds)
  (match vs
    ['() '()]
    [(cons v rest) 
              (match (interp-env v r ds)
                ['err 'err]
                [val 
                  (match (interp-list rest r ds)
                    ['err 'err]
                    [v2 (cons val v2)]
                  )
                ])
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
