#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r11 'r11) ; Return Values Count
(define r12 'r12) ; arity
(define r13 'r13) ; arity
(define r14 'r14) ; Scratch for Creating Vectors
(define r15 'r15) ; arity


;; type CEnv = [Listof Variable]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '())


            

            ; (Mov rax (imm->bits 69))
            ; (Push rax)
            
            ; (Mov rax (imm->bits 720))
            ; (Push rax)
            
            ; (Mov rax (imm->bits 727))
            ; (Push rax)
            ; (Mov rax val-empty)
            
            
            ; (compile-op2 'values)
            ; (compile-op2 'values)
            ; (compile-op1 'car-values)


            ; (Mov r12 1)
            ; (Mov (Offset rbx 0) r12)  ; write size of vector, 1
            ; (Mov rax val-empty)
            ; (Mov r14 rax)
            ; (compile-op1 'car-values)
            ; (Mov rax (imm->bits 1152921504606846978))
            ; (Mov rax (imm->bits -1))
            ; (Mov rax (imm->bits 9223372036854775808))
            
            ; (Sub rax r9)
            ; (Mov (Offset rbx (* 1 8)) rax)  ; write size of vector, 1
            ; (Mov (Offset rbx (* 2 8)) rax)  ; write size of vector, 1
            ; (Mov rax r14)
            ; (compile-op1 'cdr-values)
            ; (compile-op1 'car-values)
            ; (Mov (Offset rbx (* 2 8)) rax)  ; write size of vector, 1
          
            ; (Cmp r11 2)
            ; (Jg 'raise_error_align)
            ; (Cmp r11 1)
            ; (Je 'raise_error_align)
            (create-vector)
            ; (Mov r8 r11)
            ; (Mov (Offset rbx 0) r8)  ; write size of vector, 1

            ; (Pop rax)
            ; (Mov (Offset rbx (* 4 8)) rax)  ; write size of vector, 1
            ; (Pop rax)
            ; (Mov (Offset rbx (* 3 8)) rax)  ; write size of vector, 1
            ; (Pop rax)
            ; (Mov (Offset rbx (* 2 8)) rax)  ; write size of vector, 1
            ; (Pop rax)
            ; (Mov (Offset rbx (* 1 8)) rax)  ; write size of vector, 1
        
            (Mov rax rbx) ; return the pointer to the vector

           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           (Or rsp 8)
           (Jmp 'raise_error)
           )]))


(define (compile-mult-r11-rax)

(let (  (l1 (gensym 'mult))
        (l2 (gensym 'mult)))
  (seq  (Mov rax 0)


        (Label l1)
        (Cmp r11 0)
        (Jle l2)
        (Add rax 8)
        (Sub r11 1)
        (Jmp l1)

        (Label l2)
  )
)
)



(define (countdown-rbx-by-r11)

(let (  (l1 (gensym 'countdown))
        (l2 (gensym 'countdown)))
  (seq  
  
        (Mov rax r11)

        ; Start loop
        (Label l1)
        (Cmp rax 0)
        (Jle l2)

        (Sub rbx 8)
        (Sub rax 1)
        (Jmp l1)

        (Label l2)
  )
)
)

; Beware of bashing on r8

(define (create-vector)
  (let ((l1 (gensym 'vector))
      (l2 (gensym 'vector))
      (l3 (gensym 'vector))
      (l4 (gensym 'vector)))
    (seq 
      
      ; If we have no values being returned, go to the end immediately
      (Cmp r11 0)

      ; Move the r11 register holding the number of return values to r8
      (Mov r8 r11)
      (Mov (Offset rbx 0) r8)  ; write size of vector, 1
      (Jle l2)

      ; ; Saves the last value that was calculated to the stack
      ; (Push rax)

      
     
      ; If we only have one value, add just that one value
      (Cmp r11 1)
      (Je l3)


      ; Loop start
      (Label l1)
      ; If we are here, we have at least 2 return values
      
      ; Count down the number of return values
      (Sub r8 1)
      ; Count up where we are putting the multiple return values into the vector
      (Add rbx 8)


      ; Save the current address of the list of return values
      (Mov r14 rax)

      ; Get the first value
      (compile-op1 'car-values)
      ; Write the value to the vector
      (Mov (Offset rbx 0) rax) ; write rax as single element of vector()
      
      
      ;If we are out of return values, go to the end
      (Cmp r8 0)
      (Jle l2)

      ; Get the original list again, and then grab everything but the first value
      (Mov rax r14)
      (compile-op1 'cdr-values)

      ; Go back to the start of the loop
      (Jmp l1)
      

      ; In the case that there is only a single value, we won't have a values list so 
      ; just add that single value to the vector
      (Label l3)
      (Add rbx 8)
      (Mov (Offset rbx 0) rax)


      (Label l2)
      (countdown-rbx-by-r11)
      (Mov rax rbx) ; return the pointer to the vector
    ))
)

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f fun)
     (compile-fun f fun)]))

;; Id Fun -> Asm
(define (compile-fun f fun)
  (match fun
    [(FunPlain xs e) 
    ; (display "FUNPLAIN")

     (seq (Label (symbol->label f))
          ;; TODO: check arity
          (Cmp r12 (imm->bits (length xs)))
          (Jne 'raise_error_align)
          
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs)))

          (Ret))]
    ; TODO: handle other kinds of functions
    [(FunRest xs lst e)
    ; (display "\n")
    ; (display xs)
    ; (display "\n")
      (let ((l1 (gensym 'funrest))
            (l2 (gensym 'funrest)))
      (seq (Label (symbol->label f))
          ;; TODO: check arity

          ; If we have too few arguments, raise error
          (Cmp r12 (imm->bits (length xs)))
          (Jl 'raise_error_align)

          ; Add default empty list
          (Mov rax val-empty)

          ; ; Pop off arguments until we only have the original arguments, no list
          (Label l1)
          (Cmp r12 (imm->bits (length xs)))

          ; If we already popped the required arguments and made the list, continue, else do more
          (Je l2)
          ; tell us that we have one less argument since we added it to the list
          (Sub r12 (imm->bits 1))

          ; Take the previous list currently in rax as the first arg (second within the cons expression)
          (compile-op2 'cons)

          (Jmp l1)

          ; Push the list and do other necessary stuff
          (Label l2)
          (Push rax)
          (compile-e e (cons lst (reverse xs)))
          (Add rsp (* 8 (+ 1 (length xs))))
          (Ret)))
    ]
    [(FunCase cs) 
    ; (display "\n") 

    ; (display "\n")
    (let ((l1 (gensym 'funcase)))
      (seq  (Label (symbol->label f))

            ; Error if no arguments are provided (THIS IS WRONG AND SHOULD BE CHANGED LATER)
            ; (Cmp 'r13 (imm->bits 0))
            ; (Je 'raise_error_align)
            
            (Jmp l1)
            (compile-funcase-define f cs 1)

            (Label l1)
            (compile-funcase-call f cs 1)

            (Ret)
      )
      )
    ]
    [_
     (seq)]))


(define (compile-funcase-call func cs count)
    (let ((func-name 
                  (symbol->label 
                      (string->symbol 
                          (string-append 
                                (symbol->string func) (number->string count))))))
      (match cs
        ['() (seq (Jmp 'raise_error_align))]
        [(cons (FunPlain xs e) rest) 
            (let ((l1 (gensym 'funcase-call)))
                (seq  
                      (Cmp r12 (imm->bits (length xs)))
                      (Je func-name)
                      (compile-funcase-call func rest (+ count 1))
                )
            )
        ]
        [(cons (FunRest xs lst e) rest) 
        (let ((l1 (gensym 'funcase-call)))
                (seq  
                      (Cmp r12 (imm->bits (length xs)))
                      (Jge func-name)
                      (compile-funcase-call func rest (+ count 1))
                )
            )
        ]
        ; I DONT BELIEVE THIS CASE CAN EVEN BE REACHED. I KEPT IT ANYWAY
        ; [(cons (FunCase cs) rest) 
        ; (let ((l1 (gensym 'funcase-call)))
        ;         (seq  
        ;               (Jmp func-name)
        ;               (compile-funcase-call func rest (+ count 1))
        ;         )
        ;     )]
        [_ (seq (Jmp 'raise_error_align))]
      )
  )
)


(define (compile-funcase-define func cs count)
  (let ((func-name 
                  ; (symbol->label 
                      (string->symbol 
                          (string-append 
                                (symbol->string func) (number->string count)))))
                                  ; )
      (match cs
        ['() (seq)]
        [(cons (FunPlain xs e) rest) 
                (seq  
                      (compile-fun func-name (FunPlain xs e))
                      (compile-funcase-define func rest (+ 1 count))
                )
        ]
        [(cons (FunRest xs lst e) rest) 
                (seq  
                      (compile-fun func-name (FunRest xs lst e))
                      (compile-funcase-define func rest (+ 1 count))
                )
        ]
        [(cons (FunCase cs) rest) 
                (seq  
                      (compile-fun func-name (FunCase cs))
                      (compile-funcase-define func rest (+ 1 count))
                )]
        [_ (display "ERROR WAS HERE") (seq (Jmp 'raise_error_align))]
      )
  )
)


(define (check-number-of-return-values)
(let ((l1 (gensym 'check)))
      (seq (Cmp r11 1)
            (Jne 'raise_error_align))
  )



)

;; Expr CEnv -> Asm
(define (compile-e e c)
; (display e)
; Maybe check for multiple return values inside here?
  (match e
    [(Int i)            (seq (Mov r11 1) (compile-value i) (check-number-of-return-values)) ]
    [(Bool b)           (seq (Mov r11 1) (compile-value b) (check-number-of-return-values)) ]
    [(Char c)           (seq (Mov r11 1) (compile-value c) (check-number-of-return-values)) ]
    [(Eof)              (seq (Mov r11 1) (compile-value eof) (check-number-of-return-values)) ]
    [(Empty)            (seq (Mov r11 1) (compile-value '()) (check-number-of-return-values)) ]
    [(Var x)            (seq (Mov r11 1) (compile-variable x c) (check-number-of-return-values)) ]
    [(Str s)            (seq (Mov r11 1) (compile-string s) (check-number-of-return-values)) ]
    [(Prim0 p)          (seq (Mov r11 1) (compile-prim0 p c) (check-number-of-return-values)) ]
    [(Prim1 p e)        (seq (Mov r11 1) (compile-prim1 p e c) (check-number-of-return-values)) ]
    [(Prim2 p e1 e2)    (seq (Mov r11 1) (compile-prim2 p e1 e2 c) (check-number-of-return-values)) ]
    [(Prim3 p e1 e2 e3) (seq (Mov r11 1) (compile-prim3 p e1 e2 e3 c) (check-number-of-return-values)) ]
    [(If e1 e2 e3)      (seq (Mov r11 1) (compile-if e1 e2 e3 c) (check-number-of-return-values)) ]
    [(Begin e1 e2)      (seq (Mov r11 1) (compile-begin e1 e2 c) (check-number-of-return-values)) ]
    [(Let x e1 e2)      (seq (Mov r11 1) (compile-let x e1 e2 c) (check-number-of-return-values)) ]

    [(Values vs)        (seq (Mov r11 0) (compile-values-first vs c)) ]

    [(App f es)         (seq (Mov r11 1) (compile-app f es c)) ]
    [(Apply f es e)     (seq (Mov r11 1) (compile-apply f es e c)) ]))



(define (compile-values-first vs c)
  (match vs
  ['() (seq (Mov r11 0))]
  [(cons e '()) (seq  
                      (compile-e e c)
                      (Cmp r11 1)
                      (Jne 'raise_error_align)
                      (Mov r11 1)
                      )]
  [(cons e rest) (seq  
                      (compile-e e c)
                      (Cmp r11 1)
                      (Jne 'raise_error_align)
                      (Push rax)
                      ; (Mov rax val-empty)
                      ; (Push rax)
                      (compile-values rest c)
                      
                      (Add r11 1)

                      )]
  ; [_ (seq (Jmp 'raise_error_align))]
  )
)


(define (compile-values vs c)
(match vs
['() (seq (Mov r11 0)
          (Mov rax val-empty)
          (compile-op2 'values))]
[(cons e rest) (seq  
                    (compile-e e c)
                    (Cmp r11 1)
                    (Jne 'raise_error_align)
                    ; (Pop r8)
                    (Push rax)
                    ; (Mov rax r8)
                    ; (Mov rax val-empty)
                                      
                    (compile-values rest c)
                    (compile-op2 'values)
                    (Add r11 1)
                    )]
[_ (seq (Jmp 'raise_error_align))]
)
)




;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
       (compile-op3 p)))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app f es c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
         ;; TODO: communicate argument count to called function
         (Mov r12 (imm->bits (length es)))

         (Jmp (symbol->label f))
         (Jmp 'raise_error_align)
         (Label r))))



(define (add-env-padding c count)
  (match count
    [0 c]
    [x (cons #f (add-env-padding c (- count 1)))]
  )
)


;; Id [Listof Expr] Expr CEnv -> Asm
(define (compile-apply f es e c)
  ;; TODO: implement apply

  (let ((r (gensym 'ret))
        (start (gensym 'apply))
        (end (gensym 'apply))
        (addval (gensym 'apply)))
    (seq 
        (Lea rax r)
        (Push rax)
         
        ; TODO: communicate argument count to called function
        
        (compile-es es (cons #f c))
        (compile-e e (add-env-padding c (+ 1 (length es))))
        (Mov r13 rax)
        ; Arity
        (Mov r12 (imm->bits (length es)))

        ; Raise error if the initial input is not a list
        (compile-op1 'cons?)
        (Cmp rax val-true)
        (Mov rax r13)
        (Je start)
        (Cmp rax val-empty)
        (Jne 'raise_error_align)
        (Je end)
        

        ; Start of the loop
        (Label start)
        
        ; Check if rax is a list
        ; If rax is a list, car and cdr, else just add the value
        (compile-op1 'cons?)
        (Cmp rax val-true)
        (Mov rax r13)
        (Jne addval)

        ; Get the first value
        (compile-op1 'car)
        ; Add the first value to the stack
        (Push rax)

        ; Increment arity
        (Add r12 (imm->bits 1))

        ; retrieve the full list
        (Mov rax r13)
        ; Add the rest of the values to rax
        (compile-op1 'cdr)
        (Mov r13 rax)
        (Jmp start)


        (Label addval)
        (Cmp rax val-empty)
        (Je end)

        (Push rax)
        (Add r12 (imm->bits 1))



        (Label end)
        (Jmp (symbol->label f))
        (Label r)
        )
    )
)

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))
