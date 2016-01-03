//IN APPLIC
 PUSH(IMM(1)); 
CALL(MAKE_SOB_INTEGER);
DROP(1);
PUSH(R0); 
PUSH(IMM(1)); 
// IN LAMBDASIMPLE AAAAA 
MOV(R1,1+0); 
MOV(R2, FPARG(0)); 
for(int i=0,int j=1; i < 0; ++i, ++j){
         MOV(R3, INDD(R2,i));
         MOV(INDD(R1,j),R3);
       }
PUSH(FPARG(1)); 
CALL(MALLOC); 
DROP(1); 
for(int i=0; i<FPARG(1); ++i){
         MOV(R3,FPARG(2+i));
         MOV(INDD(R0,i),R3);
        }
MOV(INDD(R1,0),R0); 
PUSH(R1); 
PUSH(&&lambda1); 
CALL(MAKE_SOB_CLOSURE); 
JUMP(exit1); 
lambda1:
PUSH(FP);
MOV(FP,SP);
/* making the following param */

MOV(R0,FPARG(2));
POP(FP);
RETURN;
exit1: \OUT OF LAMBDA 
PUSH(R0);
CALL(IS_SOB_CLOSURE); 
CMP(R0,0); 
JUMP_EQ(L_ERROR_NOT_CLOSURE);
POP(R0); 
PUSH(INDD(R0,1));
CALL(INDD(RO,2));
POP(R1); 
POP(R1); 
DROP(R1); 

