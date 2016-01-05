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


