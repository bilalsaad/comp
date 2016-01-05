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
PUSH(IMM(1)); 
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
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
CALL(MAKE_SOB_NIL); 
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
MOV(R5,R3+1+4); 
tail_call_copy0:
BP;CMP(R3,R5); 
JUMP_EQ(jmpa0); 
 MOV(IND(R2),IND(FP)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy0); 
jmpa0: 
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
