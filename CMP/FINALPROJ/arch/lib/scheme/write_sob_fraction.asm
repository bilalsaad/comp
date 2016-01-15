/* takes a pointer to a Scheme fraction which then it prints */

WRITE_SOB_FRACTION:
PUSH(FP);
MOV(FP,SP);
MOV(R2,FPARG(0));
MOV(R1,INDD(R2,1)); //numberator
PUSH(R1); 
CALL(WRITE_INTEGER);
DROP(1);
PUSH(IMM('/'));
CALL(PUTCHAR);
DROP(1);
MOV(R1,INDD(R2,2)); //denom
PUSH(R1); 
CALL(WRITE_INTEGER);
DROP(1);
POP(FP);
RETURN;
