((JMP (LABEL ENDFIBO))

(LABEL FIBO)
(MOVE FP R0)
(SUB (LIT 1) R0)
(LOAD R0 R0)
(PUSH R0)

(MOVE FP R0)
(SUB (LIT 2) R0)
(LOAD R0 R0)
(PUSH R0)

(POP R0)
(POP R1)
(MOVE R0 FP)
(MOVE R1 SP)

(CMP (LIT 0) R0)
(JEQ (LABEL FIBO_END)) ; Si R0 est égal à zéro, termine la fonction

(ADD R1 R0)

; Vérifie si la pile n'est pas vide avant d'effectuer un POP
(CMP (LIT 10) R1) ; Assurez-vous que votre pile a au moins 10 éléments
(JLE (LABEL FIBO_POP_OK)) ; Si la pile a suffisamment d'éléments, continue
(ERROR "machine-pop : la pile est vide")

(LABEL FIBO_POP_OK)
(PUSH R0)
(MOVE FP R1)
(MOVE SP FP)
(PUSH (LIT 1))
(MOVE SP R2)
(SUB (LIT 2) R2)
(PUSH R2)
(PUSH R1)

(JSR (LABEL FIBO))

(LABEL FIBO_END)
(POP R0)
(POP R1)
(MOVE R0 FP)
(MOVE R1 SP)
(HALT)

(LABEL ENDFIBO)
(MOVE (LIT 10) R0)
(PUSH R0)
(MOVE FP R1)
(MOVE SP FP)
(PUSH (LIT 1))
(MOVE SP R2)
(SUB (LIT 2) R2)
(PUSH R2)
(PUSH R1)

(JSR (LABEL FIBO))

(POP R0)
(POP R1)
(MOVE R0 FP)
(MOVE R1 SP)
(HALT))