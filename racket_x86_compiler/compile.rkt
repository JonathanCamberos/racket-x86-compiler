#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

(define r11 'r11) ;; 

;; type CEnv = [Listof Variable]
  
;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)  
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Push rbx)    ; save callee-saved register	   
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '())
           
           ;;**********************************************************
           ;; edited 5/16/23
           ; Create and return unary vector holding the result
          ;  (Mov r8 1)
          ;  (Mov (Offset rbx 0) r8)  ; write size of vector, 1
          ;  (Mov (Offset rbx 8) rax) ; write rax as single element of vector
          ;  (Mov rax rbx)            ; return the pointer to the vector
           
           ;; edited 5/17/23
           ;; 2 cases - Check if currently rax holds a ID-vector
           ;;             if yes, remove pointer label, leave the rest of the pointer in rax
           ;;             if no, create a unary vector holding the result
           ;; mimc, this function
           ;;  (define (ID-vect-bits? v)
           ;;  (zero? (bitwise-xor (bitwise-and v imm-mask) type-ID-vect)))
           
           (ID-Vector-Check)

           
           
           (Pop rbx)     ; restore callee-save register
           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))

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
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs))) ; pop args
          (Ret))]))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Values es)   (seq                          ;; here we make our OWN vector
                        ;;(Mov r10 11111)    ;; we have a "Values List", time to compile
                        (start-Values-Vec es (length-list es) c)

                        ;;parameters
                        ;; es --> "Value List"
                        ;; len --> length of es
                        ;; c --> env
                        )] 

    ;;[(Let-Values xs IDvec (App e es)) (compile-App-Let-Values xs IDvec e2 c)] ;; APP CASE
    [(Let-Values xs IDvec e2)       (compile-Let-Values xs IDvec e2 c)]  ;; NOTE ******* - In progress


    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]
    [(Str s)            (compile-string s)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(App f es)         (compile-app f es c)])
)

;; compile-Let-Values will only work if it recieves things from "Values" struct!!

(define (compile-Let-Values xs IDvec e2 c)     ;; STOPPED HERE!!!!!!!!!!!!!!!!!!!!!!!!!
  (let ( (VectCase (gensym 'VectCaseCheck) )
         (VectArityClean (gensym 'VectArity) )
        )
  
  (seq 
      (compile-e IDvec c)  ;; pointer to vector is now in 'rax
                              
                              ;; 2 cases
                              ;;     empty vector (in which case pointer is just the ID for vector)
                              ;;     non-empty vector (pointer to length integer on heap)

      (Mov r10 rax)        ;; TODO: Verify Rax is a ID-Vec pointer
      (And r10 imm-mask)       ;; removes everything EXCEPT pointer tag
      (Cmp r10 type-ID-vect)   ;; compares tag to ID-Vect
      (Je VectCase)            ;; Continue if true
      (Jmp 'raise_error_align) ;; else throw err

      (Label VectCase)
      (Xor rax type-ID-vect)   ;; remove ID-Vec pointer tag
      (Mov r10 (Offset rax 0))   ;; first value pointed to, is the length of the vector
      (Mov r11 (length-list xs)) ;; gets length of requested vector variables with the length of xs

      (Cmp r10 r11)       ;;compares length
      (Je VectArityClean)   ;; Continue if true
      (Jmp 'raise_error_align)    ;; else throw err


      ;(Mov r10 (Offset rax (* 8 2) ) )
      ;(Mov rax r10)
      
      ;;manually
      ;;(Push rax)
      ;;(compile-e e2 (cons x c))
      (Label VectArityClean)
      (Single-Fake-Let-Star xs e2 1 c)
      ;;(Fake-Let-Star xs e2 1 c)
      )  )
)

(define (Single-Fake-Let-Star VarList e2 n c)
  (match VarList
    [(cons y ys) (seq 
                      (Mov r10 (Offset rax (* 8 n) ) )
                      (Push r10)
                      ;;(Mov rax r10)
                      (Single-Fake-Let-Star ys e2 (+ n 1) (cons y c))
                      (Add rsp 8)
                      )
                      ]
    [_ (seq (compile-e e2 c)) ]
    )
)

;; parameters
;;        rax - has a clean ID-vector pointer 
;;        r10 - has length of ID-vector
;; TODO:
;;       The exact same thing as let*, except everything is on the stack
;;       instead of in a list
(define (Fake-Let-Star VarList e2 n c)
  (match VarList
    [(cons varX varXS) (seq 
                      (Mov r10 (Offset rax (* 8 n) ) )
                      (Mov rax r10)
                      (Push r10)
                      (Fake-Let-Star varXS e2 (+ n 1) (cons varX c))
                      (Add rsp 8)
                      )
                      ]
    [_ (compile-e e2 c)]
    )
)


(define (ID-Vector-Check)
  (let ( (VectCase (gensym 'IDVectCase) )
         (VectFin (gensym 'VectFinished) )
        )
  (seq
      (Mov r10 rax)    ;; checks if whatever is in rax is a ID-vect Pointer
                       ;;          if ID-Vector, skip and remove ID-vect Pointer tag
      (And r10 imm-mask)
      (Cmp r10 type-ID-vect)
      (Je VectCase)
      
      ;; (NON-Vector case) single return case ---> make unary vector
      (Mov r10 1)
      (Mov (Offset rbx 0) r10)  ; write size of vector, 1
      (Mov (Offset rbx 8) rax) ; write rax as single element of vector
      (Mov rax rbx)            ; return the pointer to the vector (untagged)
      ;;(Add rbx 16)
      (Jmp VectFin)

      (Label VectCase)
      (Xor rax type-ID-vect)
      
      (Label VectFin)
      )
  )
)

;; given parameters
                        ;; es --> "Value List"
                        ;; len --> length of es
                        ;; c --> env
(define (start-Values-Vec es len c)
  (seq 
      (Mov r10 len)  ;; move the length of the Vector to the heap pointer
      (Mov (Offset rbx 0) r10)
      (set-Values-Vec es len 1 c)  ;; compile and add "Value List" items
      
                        ;; es --> "Value List"
                        ;; len --> length of es
                        ;; 1 --> counter to offset rbx, starts at 1 to avoid overwriting Vec Length
                        ;; c --> env
      ))
;; given parameters
                        ;; es --> "Value List"
                        ;; len --> length of es
                        ;; 1 --> counter to offset rbx, starts at 1 to avoid overwriting Vec Length
                        ;; c --> env
(define (set-Values-Vec es len n c)
  (match es
    [(cons x xs)   
                  (seq  
                       ;;(Mov r10 33333)
                       (compile-e x c) ;; compiles item in list, into rax
                       (Mov (Offset rbx (* 8 n)) rax)
                       (set-Values-Vec xs len (+ n 1) c)
                       )]
    ['() (seq 
              (Mov rax rbx)
              (Or rax type-ID-vect)  ;; NOTE**** putting this back in leads to a "contract violation"       
              (Add rbx (* 8 (+ 1 len)))   ;; (+ 1 len) becayse if the len value
              ;;(Mov r10 420)   
              )] ;; dummy move
    )
)

;; for cases such as (add1 (values 5)), values returns a pointer, which needs to be read
(define (single-Return-ID-Vec-Check)
  (let ( (NonVectCase (gensym 'NonVectCaseCheck) )
         (SingleVectArityClean (gensym 'SingleVectArity) )
        )
  (seq
          ;; Assuming "Values" has just been compiled, pointer should be in rax
          ;; follow similar approach as to "compile-Let-Values" but for only length of 1
          ;; BUT FIRST, check if rax is a pointer if not, just skip this entierly
      (Mov r10 rax)        ;; TODO: Verify Rax is a ID-Vec pointer
      (And r10 imm-mask)       ;; removes everything EXCEPT pointer tag
      (Cmp r10 type-ID-vect)   ;; compares tag to ID-Vect
      (Jne NonVectCase)        ;; Skip all Code if NOT AN ID-VECT

                               ;; ID-Vect Case
      (Xor rax type-ID-vect)     ;; remove ID-Vec pointer tag
      (Mov r10 (Offset rax 0))   ;; first value pointed to, is the length of the vector
      (Mov r11 1) ;; gets length of requested vector NOTE* ONLY LENGTH 1 FOR (add1 (values 5))
      (Cmp r10 r11)
      (Je SingleVectArityClean)  ;; "Values" struct returning only 1 item, good!!

      (Jmp 'raise_error_align)    ;; else throw err
      
      (Label SingleVectArityClean)   ;; read the vector! and move the single item into Rax
      (Mov r10 (Offset rax 8))
      (Mov rax r10)

      (Label NonVectCase)
      )
  
  )
)


(define (length-list xs) 
  (match xs 
    [(cons y ys) (+ 1 (length-list ys))]
    ['() 0]
    ))





;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

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
       (single-Return-ID-Vec-Check)
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
         (Cmp rax (value->bits #f))
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
         (Jmp (symbol->label f))
         (Label r))))

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
