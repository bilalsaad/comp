/* THIS IS THE START OF THE PROGRAN */
#include <stdio.h>
#include <stdlib.h>

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1
#define FALSE_ADDR 32
#define db {printf("LINE: %d\n",__LINE__);}
#include "arch/cisc.h"

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
MOV(INDD(R0,2),&&lambda7);
JUMP(exit7); 
lambda7:
PUSH(FP);
MOV(FP,SP);
PUSH(IMM(109)); 
PUSH(IMM(111)); 
PUSH(IMM(115)); 
PUSH(IMM(104)); 
PUSH(IMM(101)); 
PUSH(IMM(5)); 
CALL(MAKE_SOB_STRING) 
DROP(1+5)POP(FP);
RETURN;
exit7: //OUT OF LAMBDA 
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
MOV(INDD(R0,2),&&lambda8);
JUMP(exit8); 
lambda8:
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
MOV(INDD(R0,2),&&lambda9);
JUMP(exit9); 
lambda9:
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
MOV(INDD(R0,2),&&lambda10);
JUMP(exit10); 
lambda10:
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
MOV(R5,FP+1+4); 
tail_call_copy5:
CMP(R3,R5); 
JUMP_EQ(jmpa5); 
 MOV(IND(R2),IND(FP)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy5); 
jmpa5: 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit10: //OUT OF LAMBDA 
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
MOV(R5,FP+1+4); 
tail_call_copy6:
CMP(R3,R5); 
JUMP_EQ(jmpa6); 
 MOV(IND(R2),IND(FP)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy6); 
jmpa6: 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit9: //OUT OF LAMBDA 
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
MOV(INDD(R0,2),&&lambda11);
JUMP(exit11); 
lambda11:
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
MOV(R5,FP+1+4); 
tail_call_copy7:
CMP(R3,R5); 
JUMP_EQ(jmpa7); 
 MOV(IND(R2),IND(FP)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy7); 
jmpa7: 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit11: //OUT OF LAMBDA 
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
MOV(R5,FP+1+4); 
tail_call_copy8:
CMP(R3,R5); 
JUMP_EQ(jmpa8); 
 MOV(IND(R2),IND(FP)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy8); 
jmpa8: 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit8: //OUT OF LAMBDA 
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
