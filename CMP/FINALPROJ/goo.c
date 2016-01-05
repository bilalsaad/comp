/* THIS IS THE START OF THE PROGRAN */
#include <stdio.h>
#include <stdlib.h>

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

#define db {printf("LINE: %d\n",__LINE__);}
#include "arch/cisc.h"
#include "debug.h"

int main()
{
  START_MACHINE;

  JUMP(CONTINUE);

#include "arch/char.lib"
#include "arch/io.lib"
#include "arch/math.lib"
#include "arch/string.lib"
#include "arch/system.lib"
#include "arch/scheme.lib"
  CONTINUE:
  PUSH(IMM(0));
  PUSH(IMM(0));
  PUSH(IMM(0));
  PUSH(IMM(0));
  MOV(FP,SP);


PUSH(IMM(13+1)); 
CALL(MALLOC); 
DROP(1); 
 
MOV(IND(R0),IMM(937610));
 INCR(R0);
MOV(IND(R0),IMM(722689));
 INCR(R0);
MOV(IND(R0),IMM(741553));
 INCR(R0);
MOV(IND(R0),IMM(0));
 INCR(R0);
MOV(IND(R0),IMM(741553));
 INCR(R0);
MOV(IND(R0),IMM(1));
 INCR(R0);
MOV(IND(R0),IMM(799345));
 INCR(R0);
MOV(IND(R0),IMM(5));
 INCR(R0);
MOV(IND(R0),IMM(109));
 INCR(R0);
MOV(IND(R0),IMM(111));
 INCR(R0);
MOV(IND(R0),IMM(115));
 INCR(R0);
MOV(IND(R0),IMM(104));
 INCR(R0);
MOV(IND(R0),IMM(101));
 INCR(R0);

 /*new expr */ 
 
//IN APPLIC 
// IN LAMBDA AAAAA 
MOV(R1,1+0); 
PUSH(R1); 
CALL(MALLOC);
MOV(R1,R0); 
DROP(1); 
MOV(R2, FPARG(0)); 
for(int i=0,j=1; i < 0; ++i, ++j){
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
PUSH(IMM(3)); 
CALL(MALLOC); 
DROP(1); 
MOV(INDD(R0,0),T_CLOSURE);
MOV(INDD(R0,1),R1); 
MOV(INDD(R0,2),&&lambda0);
JUMP(exit0); 
lambda0:
PUSH(FP);
MOV(FP,SP);
MOV(R0,IMM(7))POP(FP);
RETURN;
exit0: //OUT OF LAMBDA 
PUSH(R0); 
PUSH(IMM(1)); 
// IN LAMBDA AAAAA 
MOV(R1,1+0); 
PUSH(R1); 
CALL(MALLOC);
MOV(R1,R0); 
DROP(1); 
MOV(R2, FPARG(0)); 
for(int i=0,j=1; i < 0; ++i, ++j){
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
PUSH(IMM(3)); 
CALL(MALLOC); 
DROP(1); 
MOV(INDD(R0,0),T_CLOSURE);
MOV(INDD(R0,1),R1); 
MOV(INDD(R0,2),&&lambda1);
JUMP(exit1); 
lambda1:
PUSH(FP);
MOV(FP,SP);
//IN APPLIC 
// IN LAMBDA AAAAA 
MOV(R1,1+1); 
PUSH(R1); 
CALL(MALLOC);
MOV(R1,R0); 
DROP(1); 
MOV(R2, FPARG(0)); 
for(int i=0,j=1; i < 1; ++i, ++j){
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
PUSH(IMM(3)); 
CALL(MALLOC); 
DROP(1); 
MOV(INDD(R0,0),T_CLOSURE);
MOV(INDD(R0,1),R1); 
MOV(INDD(R0,2),&&lambda2);
JUMP(exit2); 
lambda2:
PUSH(FP);
MOV(FP,SP);
//IN APPLIC 
// IN LAMBDA AAAAA 
MOV(R1,1+2); 
PUSH(R1); 
CALL(MALLOC);
MOV(R1,R0); 
DROP(1); 
MOV(R2, FPARG(0)); 
for(int i=0,j=1; i < 2; ++i, ++j){
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
PUSH(IMM(3)); 
CALL(MALLOC); 
DROP(1); 
MOV(INDD(R0,0),T_CLOSURE);
MOV(INDD(R0,1),R1); 
MOV(INDD(R0,2),&&lambda3);
JUMP(exit3); 
lambda3:
PUSH(FP);
MOV(FP,SP);
//IN APPLIC 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(1)); 
//IN APPLIC 
MOV(R0,FPARG(0));
MOV(R0,INDD(R0,0));
MOV(R0,INDD(R0,0));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,FPARG(0));
MOV(R0,INDD(R0,0));
MOV(R0,INDD(R0,0));
PUSH(R0);
CALL(IS_SOB_CLOSURE); 
CMP(R0,0); 
JUMP_EQ(L_ERROR_NOT_CLOSURE);
POP(R0); 
PUSH(INDD(R0,1));
CALLA(INDD(R0,2));
POP(R1); 
POP(R1); 
DROP(R1); 
 //OUT OF APPLIC 
PUSH(R0);
CALL(IS_SOB_CLOSURE); 
CMP(R0,0); 
JUMP_EQ(L_ERROR_NOT_CLOSURE);
POP(R0); 
PUSH(INDD(R0,1));
PUSH(FPARG(-1));
MOV(R2,FP); 
SUB(R2,(FPARG(1)+4)); 
MOV(R3,FP); 
MOV(FP,FPARG(-2));
MOV(R5,R3+1+3); 
tail_call_copy0:
CMP(R3,R5); 
JUMP_EQ(jmpa0); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy0); 
jmpa0: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit3: //OUT OF LAMBDA 
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,FPARG(0));
MOV(R0,INDD(R0,0));
MOV(R0,INDD(R0,0));
PUSH(R0);
CALL(IS_SOB_CLOSURE); 
CMP(R0,0); 
JUMP_EQ(L_ERROR_NOT_CLOSURE);
POP(R0); 
PUSH(INDD(R0,1));
PUSH(FPARG(-1));
MOV(R2,FP); 
SUB(R2,(FPARG(1)+4)); 
MOV(R3,FP); 
MOV(FP,FPARG(-2));
MOV(R5,R3+1+3); 
tail_call_copy1:
CMP(R3,R5); 
JUMP_EQ(jmpa1); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy1); 
jmpa1: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit2: //OUT OF LAMBDA 
PUSH(R0); 
PUSH(IMM(1)); 
// IN LAMBDA AAAAA 
MOV(R1,1+1); 
PUSH(R1); 
CALL(MALLOC);
MOV(R1,R0); 
DROP(1); 
MOV(R2, FPARG(0)); 
for(int i=0,j=1; i < 1; ++i, ++j){
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
PUSH(IMM(3)); 
CALL(MALLOC); 
DROP(1); 
MOV(INDD(R0,0),T_CLOSURE);
MOV(INDD(R0,1),R1); 
MOV(INDD(R0,2),&&lambda4);
JUMP(exit4); 
lambda4:
PUSH(FP);
MOV(FP,SP);
//IN APPLIC 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,FPARG(2));
PUSH(R0);
CALL(IS_SOB_CLOSURE); 
CMP(R0,0); 
JUMP_EQ(L_ERROR_NOT_CLOSURE);
POP(R0); 
PUSH(INDD(R0,1));
PUSH(FPARG(-1));
MOV(R2,FP); 
SUB(R2,(FPARG(1)+4)); 
MOV(R3,FP); 
MOV(FP,FPARG(-2));
MOV(R5,R3+1+3); 
tail_call_copy2:
CMP(R3,R5); 
JUMP_EQ(jmpa2); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy2); 
jmpa2: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit4: //OUT OF LAMBDA 
PUSH(R0);
CALL(IS_SOB_CLOSURE); 
CMP(R0,0); 
JUMP_EQ(L_ERROR_NOT_CLOSURE);
POP(R0); 
PUSH(INDD(R0,1));
PUSH(FPARG(-1));
MOV(R2,FP); 
SUB(R2,(FPARG(1)+4)); 
MOV(R3,FP); 
MOV(FP,FPARG(-2));
MOV(R5,R3+1+3); 
tail_call_copy3:
CMP(R3,R5); 
JUMP_EQ(jmpa3); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy3); 
jmpa3: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit1: //OUT OF LAMBDA 
PUSH(R0);
CALL(IS_SOB_CLOSURE); 
CMP(R0,0); 
JUMP_EQ(L_ERROR_NOT_CLOSURE);
POP(R0); 
PUSH(INDD(R0,1));
CALLA(INDD(R0,2));
POP(R1); 
POP(R1); 
DROP(R1); 
 //OUT OF APPLIC 
/*epilogue */ 


PUSH(R0);
CALL(WRITE_SOB);
printf("\n");
DROP(1);
STOP_MACHINE;
return 0;

L_ERROR_NOT_CLOSURE:
  exit(12);
}
