/* cisc.c
 * Mock-assembly programming for a CISC-like architecture
 * 
 * Programmer: Mayer Goldberg, 2010
 */

#include <stdio.h>
#include <stdlib.h>

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

#include "cisc.h"

int main()
{
  START_MACHINE;

  JUMP(CONTINUE);

#include "char.lib"
#include "io.lib"
#include "math.lib"
#include "string.lib"
#include "system.lib"
#include "scheme.lib"

 CONTINUE:
 //IN APPLIC
  PUSH(IMM(1)); 
  CALL(MAKE_SOB_INTEGER);
  DROP(1);
  PUSH(R0); 
  PUSH(IMM(1)); 
  // IN LAMBDASIMPLE AAAAA 
  MOV(R1,1+0); 
  MOV(R2, FPARG(0)); 
  for(int i=0, j=1; i < 0; ++i, ++j){
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
  exit1: 
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

  STOP_MACHINE;

  return 0;
}
