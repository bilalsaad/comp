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


PUSH(IMM(10+1)); 
CALL(MALLOC); 
DROP(1); 
 
MOV(IND(R0),IMM(937610));
 INCR(R0);
MOV(IND(R0),IMM(722689));
 INCR(R0);
MOV(IND(R0),IMM(741553));
 INCR(R0);
MOV(IND(R0),IMM(1));
 INCR(R0);
MOV(IND(R0),IMM(741553));
 INCR(R0);
MOV(IND(R0),IMM(0));
 INCR(R0);
MOV(IND(R0),IMM(945311));
 INCR(R0);
MOV(IND(R0),IMM(1));
 INCR(R0);
MOV(IND(R0),IMM(945311));
 INCR(R0);
MOV(IND(R0),IMM(3));
 INCR(R0);

#define VOID 1
#define NIL 2
#define FALSE 5
#define TRUE  3

 /*new expr */ 
 
MOV(R0,IMM(5)); 

CMP(R0,FALSE);

JUMP_EQ(else0);
MOV(R0,IMM(7)); 

JUMP(if_exit0);
else0:
MOV(R0,IMM(9)); 
if_exit0:
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
