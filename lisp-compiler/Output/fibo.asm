((JMP (LABEL ENDFIBO)) (LABEL FIBO) (MOVE FP R0) (SUB (LIT 1) R0) (LOAD R0 R0)
 (PUSH R0) (MOVE (LIT 0) R0) (PUSH R0) (MOVE FP R1) (MOVE SP FP)
 (PUSH (LIT 2)) (MOVE SP R2) (SUB (LIT 3) R2) (PUSH R2) (PUSH R1) (MOVE FP R0)
 (SUB (LIT 1) R0) (LOAD R0 R0) (MOVE FP R1) (SUB (LIT 2) R1) (LOAD R1 R1)
 (CMP R0 R1) (MOVE (LIT 1) R0) (JEQ (LABEL FINCOMP3)) (MOVE (LIT 0) R0)
 (LABEL FINCOMP3) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (CMP (LIT 0) R0)
 (JEQ (LABEL SINON3)) (MOVE (LIT 0) R0) (JMP (LABEL FINSI3)) (LABEL SINON3)
 (MOVE FP R0) (SUB (LIT 1) R0) (LOAD R0 R0) (PUSH R0) (MOVE (LIT 1) R0)
 (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 2)) (MOVE SP R2)
 (SUB (LIT 3) R2) (PUSH R2) (PUSH R1) (MOVE FP R0) (SUB (LIT 1) R0)
 (LOAD R0 R0) (MOVE FP R1) (SUB (LIT 2) R1) (LOAD R1 R1) (CMP R0 R1)
 (MOVE (LIT 1) R0) (JEQ (LABEL FINCOMP4)) (MOVE (LIT 0) R0) (LABEL FINCOMP4)
 (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (CMP (LIT 0) R0)
 (JEQ (LABEL SINON4)) (MOVE (LIT 1) R0) (JMP (LABEL FINSI4)) (LABEL SINON4)
 (MOVE (LIT 2) R0) (PUSH R0) (MOVE FP R0) (SUB (LIT 1) R0) (LOAD R0 R0)
 (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 2)) (MOVE SP R2)
 (SUB (LIT 3) R2) (PUSH R2) (PUSH R1) (MOVE FP R0) (SUB (LIT 1) R0)
 (LOAD R0 R0) (MOVE FP R1) (SUB (LIT 2) R1) (LOAD R1 R1) (SUB R1 R0) (POP R1)
 (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0) (MOVE FP R1) (MOVE SP FP)
 (PUSH (LIT 1)) (MOVE SP R2) (SUB (LIT 2) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL FIBO)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0)
 (MOVE (LIT 1) R0) (PUSH R0) (MOVE FP R0) (SUB (LIT 1) R0) (LOAD R0 R0)
 (PUSH R0) (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 2)) (MOVE SP R2)
 (SUB (LIT 3) R2) (PUSH R2) (PUSH R1) (MOVE FP R0) (SUB (LIT 1) R0)
 (LOAD R0 R0) (MOVE FP R1) (SUB (LIT 2) R1) (LOAD R1 R1) (SUB R1 R0) (POP R1)
 (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0) (MOVE FP R1) (MOVE SP FP)
 (PUSH (LIT 1)) (MOVE SP R2) (SUB (LIT 2) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL FIBO)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (PUSH R0)
 (MOVE FP R1) (MOVE SP FP) (PUSH (LIT 2)) (MOVE SP R2) (SUB (LIT 3) R2)
 (PUSH R2) (PUSH R1) (MOVE FP R0) (SUB (LIT 1) R0) (LOAD R0 R0) (MOVE FP R1)
 (SUB (LIT 2) R1) (LOAD R1 R1) (ADD R1 R0) (POP R1) (POP R2) (MOVE R1 FP)
 (MOVE R2 SP) (LABEL FINSI4) (LABEL FINSI3) (MOVE FP R1) (ADD (LIT 4) R1)
 (MOVE R1 SP) (RTN) (LABEL ENDFIBO) (MOVE (LIT 10) R0) (PUSH R0) (MOVE FP R1)
 (MOVE SP FP) (PUSH (LIT 1)) (MOVE SP R2) (SUB (LIT 2) R2) (PUSH R2) (PUSH R1)
 (JSR (LABEL FIBO)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (HALT))