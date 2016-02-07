/*makes a fraction */

L_gcd:
PUSH(FP);
MOV(FP,SP);
PUSH(R1);
PUSH(R2);
PUSH(R3);
MOV(R1,FPARG(0));
MOV(R2,FPARG(1));
XOR(R3,R3);
L_gcd_loop:
  CMP(R1,0);
  JUMP_EQ(L_gcd_finish);
  MOV(R3,R1);
  REM(R2,R1);
  MOV(R1,R2);
  MOV(R2,R3); 
  JUMP(L_gcd_loop);
L_gcd_finish:
MOV(R0,R2);

POP(R3);
POP(R2);
POP(R1);
POP(FP);
RETURN;

MAKE_SOB_FRACTION:
PUSH(FP);
MOV(FP,SP);
PUSH(FPARG(1));
PUSH(FPARG(0));
CALL(L_gcd);
DROP(2);
MOV(R1,FPARG(0));
MOV(R2,FPARG(1));
DIV(R1,R0);
DIV(R2,R0);
CMP(R2,IMM(0)); //if r2 is negative we're fucked
JUMP_LT(L_NEG_DENOM);
L_continue:
CMP(R2,IMM(1));
JUMP_EQ(L_make_int);
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(IND(R0),T_FRACTION);
MOV(INDD(R0,1),R1);
MOV(INDD(R0,2),R2);

POP(FP);
RETURN;

L_make_int:
PUSH(R1);
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;

L_NEG_DENOM:
MUL(R1,-1);
MUL(R2,-1);
JUMP(L_continue);
