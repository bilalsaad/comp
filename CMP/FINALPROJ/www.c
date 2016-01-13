#define SYMBOL_TABLE 1
#define VOID 2
#define NIL 3
#define FALSE 4
#define TRUE  6

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


PUSH(IMM(241+1)); 
CALL(MALLOC); 
MOV(R0,IMM(2)); 
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
MOV(IND(R0),IMM(945311));
 INCR(R0);
MOV(IND(R0),IMM(10));
 INCR(R0);
MOV(IND(R0),IMM(799345));
 INCR(R0);
MOV(IND(R0),IMM(4));
 INCR(R0);
MOV(IND(R0),IMM(97));
 INCR(R0);
MOV(IND(R0),IMM(108));
 INCR(R0);
MOV(IND(R0),IMM(97));
 INCR(R0);
MOV(IND(R0),IMM(97));
 INCR(R0);

 /*starting global env------------*/ 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 
MOV(IND(R0),IMM(T_UNDEFINED)); INCR(R0); 

/* STARTING TO ADD PRIMITIVES */ 
MOV(IND(17),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_car));
INCR(R0); 
MOV(IND(18),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_cdr));
INCR(R0); 
MOV(IND(19),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_cons));
INCR(R0); 
MOV(IND(20),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_plus));
INCR(R0); 
MOV(IND(21),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_minus));
INCR(R0); 
MOV(IND(22),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_is_zero));
INCR(R0); 
MOV(IND(23),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_is_null));
INCR(R0); 
MOV(IND(24),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_mul));
INCR(R0); 
MOV(IND(25),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_is_list));
INCR(R0); 
MOV(IND(26),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_is_pair));
INCR(R0); 
MOV(IND(27),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_v_plus));
INCR(R0); 
MOV(IND(28),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_v_minus));
INCR(R0); 
MOV(IND(29),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_v_mult));
INCR(R0); 
MOV(IND(30),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_v_div));
INCR(R0); 
MOV(IND(31),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_vector));
INCR(R0); 
MOV(IND(32),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_apply));
INCR(R0); 
MOV(IND(33),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_make_string));
INCR(R0); 
MOV(IND(34),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_is_eq));
INCR(R0); 
MOV(IND(35),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_v_lt));
INCR(R0); 
MOV(IND(36),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_v_gt));
INCR(R0); 
MOV(IND(37),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_v_eq));
INCR(R0); 
MOV(IND(38),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_string_to_symbol));
INCR(R0); 
MOV(IND(39),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_symbol_to_string));
INCR(R0); 
MOV(IND(40),R0);
MOV(IND(R0), IMM(T_CLOSURE)); 
INCR(R0);
MOV(IND(R0),IMM(2131));
INCR(R0); 
MOV(IND(R0), LABEL(L_make_vector));
INCR(R0); 

 //STRING LIST 
MOV(INDD(R0,0),IMM(T_UNDEFINED)); 
MOV(INDD(R0,1),IMM(205774250)); 
MOV(IND(SYMBOL_TABLE),R0); 


 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(27)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(41), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(28)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(42), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(29)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(43), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(30)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(44), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(35)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(45), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(36)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(46), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(37)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(47), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(33)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(48), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(40)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(49), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(34)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(50), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(22)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(51), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(23)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(52), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(26)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(53), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
MOV(R0,IND(25)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);

 MOV(IND(54), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
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
MOV(INDD(R0,2),LABEL(lambda9));
JUMP(exit9); 
lambda9:
PUSH(FP);
MOV(FP,SP);
MOV(R1,FPARG(1)); 
MOV(R2,NIL); 
lambda_opt_loop2:
 CMP(R1,0);
 JUMP_EQ(finish_lambda_opt2); 
 PUSH(R2); 
 PUSH(FPARG(R1+1)); 
 CALL(MAKE_SOB_PAIR);
 DROP(2); 
 SUB(R1,1); 
 MOV(R2,R0); 
 JUMP(lambda_opt_loop2);
finish_lambda_opt2: 
CMP(IND(R2),T_NIL); 
JUMP_EQ(empty_opt_case2); 
MOV(FPARG(2+0),R2); 
MOV(R4,FP); 
 SUB(R4,IMM(4)); 
SUB(R4,FPARG(1)); // now r4 should hold number of old args 
MOV(FPARG(1),1+0);
MOV(R3,FP); // now r3 should hold new number of args 
SUB(R3,IMM(4)); 
SUB(R3,FPARG(1)); 
MOV(R5,R3); 
SUB(R5,R4); 
JUMP_EQ(empty_opt_case2); 
stack_fix2: 
 CMP(R3,SP); 
 JUMP_EQ(stack_fix_end2); 
 MOV(STACK(R4),STACK(R3)); 
 INCR(R4); 
 INCR(R3); 
 JUMP(stack_fix2); 
empty_opt_case2:
  MOV(R5,IMM(-1)); 
 MOV(R6,SP);
  MOV(R3,SP); 
 DECR(R3); 
  MOV(R4,FP); 
  SUB(R4,IMM(4)); 
 SUB(R4,FPARG(1)); // now r4 should hold number of old args 
empty_opt2:
 CMP(R3,R4); 
 JUMP_LT(end_of_nil_case_opt2); 
 MOV(STACK(R6),STACK(R3)); 
 DECR(R6); 
 DECR(R3); 
 JUMP(empty_opt2);
end_of_nil_case_opt2: 
MOV(STACK(R6),R2); 
stack_fix_end2: 
//need to fix the stack pointer stuff now 
SUB(SP,R5); 
MOV(FP,SP); 
MOV(R0,FPARG(2));
POP(FP);
RETURN;
exit9: //OUT OF LAMBDA 

 MOV(IND(55), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
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
MOV(INDD(R0,2),LABEL(lambda10));
JUMP(exit10); 
lambda10:
PUSH(FP);
MOV(FP,SP);
//IN APPLIC 
MOV(R0,FPARG(4));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(52)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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

CMP(R0,FALSE);

JUMP_EQ(else7);
MOV(R0,FPARG(3));

JUMP(if_exit7);
else7:
//IN APPLIC 
//IN APPLIC 
MOV(R0,FPARG(4));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(18)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
//IN APPLIC 
MOV(R0,FPARG(3));
PUSH(R0); 
//IN APPLIC 
MOV(R0,FPARG(4));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(17)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
PUSH(IMM(2)); 
MOV(R0,FPARG(2));
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
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(3)); 
MOV(R0,IND(56)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
MOV(R5,R3+3+3); 
tail_call_copy8:
CMP(R3,R5); 
JUMP_EQ(jmpa8); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy8); 
jmpa8: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
if_exit7:
POP(FP);
RETURN;
exit10: //OUT OF LAMBDA 

 MOV(IND(56), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
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
MOV(INDD(R0,2),LABEL(lambda11));
JUMP(exit11); 
lambda11:
PUSH(FP);
MOV(FP,SP);
//IN APPLIC 
MOV(R0,FPARG(4));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(52)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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

CMP(R0,FALSE);

JUMP_EQ(else8);
MOV(R0,FPARG(3));

JUMP(if_exit8);
else8:
//IN APPLIC 
//IN APPLIC 
MOV(R0,FPARG(4));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(18)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
//IN APPLIC 
//IN APPLIC 
MOV(R0,FPARG(4));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(17)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
MOV(R0,FPARG(3));
PUSH(R0); 
PUSH(IMM(2)); 
MOV(R0,FPARG(2));
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
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(3)); 
MOV(R0,IND(57)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
MOV(R5,R3+3+3); 
tail_call_copy9:
CMP(R3,R5); 
JUMP_EQ(jmpa9); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy9); 
jmpa9: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
if_exit8:
POP(FP);
RETURN;
exit11: //OUT OF LAMBDA 

 MOV(IND(57), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
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
MOV(INDD(R0,2),LABEL(lambda12));
JUMP(exit12); 
lambda12:
PUSH(FP);
MOV(FP,SP);
//IN APPLIC 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(52)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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

CMP(R0,FALSE);

JUMP_EQ(else10);
MOV(R0,FPARG(3));

JUMP(if_exit10);
else10:
//IN APPLIC 
//IN APPLIC 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(54)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
PUSH(IMM(1)); 
MOV(R0,IND(61)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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

CMP(R0,FALSE);

JUMP_EQ(else9);
MOV(R0,FPARG(2));

JUMP(if_exit9);
else9:
//IN APPLIC 
//IN APPLIC 
MOV(R0,FPARG(3));
PUSH(R0); 
//IN APPLIC 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(17)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
PUSH(IMM(2)); 
MOV(R0,IND(19)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
//IN APPLIC 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(18)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
PUSH(IMM(2)); 
MOV(R0,IND(58)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
MOV(R5,R3+2+3); 
tail_call_copy10:
CMP(R3,R5); 
JUMP_EQ(jmpa10); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy10); 
jmpa10: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
if_exit9:
if_exit10:
POP(FP);
RETURN;
exit12: //OUT OF LAMBDA 

 MOV(IND(58), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
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
MOV(INDD(R0,2),LABEL(lambda13));
JUMP(exit13); 
lambda13:
PUSH(FP);
MOV(FP,SP);
//IN APPLIC 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(52)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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

CMP(R0,FALSE);

JUMP_EQ(else12);
MOV(R0,FPARG(3));

JUMP(if_exit12);
else12:
//IN APPLIC 
//IN APPLIC 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(54)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
PUSH(IMM(1)); 
MOV(R0,IND(61)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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

CMP(R0,FALSE);

JUMP_EQ(else11);
//IN APPLIC 
MOV(R0,FPARG(3));
PUSH(R0); 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(2)); 
MOV(R0,IND(19)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
MOV(R5,R3+2+3); 
tail_call_copy12:
CMP(R3,R5); 
JUMP_EQ(jmpa12); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy12); 
jmpa12: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 

JUMP(if_exit11);
else11:
//IN APPLIC 
//IN APPLIC 
MOV(R0,FPARG(3));
PUSH(R0); 
//IN APPLIC 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(17)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
PUSH(IMM(2)); 
MOV(R0,IND(19)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
//IN APPLIC 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(1)); 
MOV(R0,IND(18)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
PUSH(IMM(2)); 
MOV(R0,IND(59)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
MOV(R5,R3+2+3); 
tail_call_copy11:
CMP(R3,R5); 
JUMP_EQ(jmpa11); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy11); 
jmpa11: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
if_exit11:
if_exit12:
POP(FP);
RETURN;
exit13: //OUT OF LAMBDA 

 MOV(IND(59), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
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
MOV(INDD(R0,2),LABEL(lambda14));
JUMP(exit14); 
lambda14:
PUSH(FP);
MOV(FP,SP);
MOV(R1,FPARG(1)); 
MOV(R2,NIL); 
lambda_opt_loop3:
 CMP(R1,0);
 JUMP_EQ(finish_lambda_opt3); 
 PUSH(R2); 
 PUSH(FPARG(R1+1)); 
 CALL(MAKE_SOB_PAIR);
 DROP(2); 
 SUB(R1,1); 
 MOV(R2,R0); 
 JUMP(lambda_opt_loop3);
finish_lambda_opt3: 
CMP(IND(R2),T_NIL); 
JUMP_EQ(empty_opt_case3); 
MOV(FPARG(2+0),R2); 
MOV(R4,FP); 
 SUB(R4,IMM(4)); 
SUB(R4,FPARG(1)); // now r4 should hold number of old args 
MOV(FPARG(1),1+0);
MOV(R3,FP); // now r3 should hold new number of args 
SUB(R3,IMM(4)); 
SUB(R3,FPARG(1)); 
MOV(R5,R3); 
SUB(R5,R4); 
JUMP_EQ(empty_opt_case3); 
stack_fix3: 
 CMP(R3,SP); 
 JUMP_EQ(stack_fix_end3); 
 MOV(STACK(R4),STACK(R3)); 
 INCR(R4); 
 INCR(R3); 
 JUMP(stack_fix3); 
empty_opt_case3:
  MOV(R5,IMM(-1)); 
 MOV(R6,SP);
  MOV(R3,SP); 
 DECR(R3); 
  MOV(R4,FP); 
  SUB(R4,IMM(4)); 
 SUB(R4,FPARG(1)); // now r4 should hold number of old args 
empty_opt3:
 CMP(R3,R4); 
 JUMP_LT(end_of_nil_case_opt3); 
 MOV(STACK(R6),STACK(R3)); 
 DECR(R6); 
 DECR(R3); 
 JUMP(empty_opt3);
end_of_nil_case_opt3: 
MOV(STACK(R6),R2); 
stack_fix_end3: 
//need to fix the stack pointer stuff now 
SUB(SP,R5); 
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
MOV(INDD(R0,2),LABEL(lambda15));
JUMP(exit15); 
lambda15:
PUSH(FP);
MOV(FP,SP);
//IN APPLIC 
MOV(R0,FPARG(3));
PUSH(R0); 
//IN APPLIC 
MOV(R0,IMM(3)); 
PUSH(R0); 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(2)); 
MOV(R0,IND(58)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
PUSH(IMM(2)); 
MOV(R0,IND(59)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
MOV(R5,R3+2+3); 
tail_call_copy13:
CMP(R3,R5); 
JUMP_EQ(jmpa13); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy13); 
jmpa13: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit15: //OUT OF LAMBDA 
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
MOV(INDD(R0,2),LABEL(lambda16));
JUMP(exit16); 
lambda16:
PUSH(FP);
MOV(FP,SP);
//IN APPLIC 
MOV(R0,FPARG(0));
MOV(R0,INDD(R0,0));
MOV(R0,INDD(R0,0));
PUSH(R0); 
MOV(R0,IMM(3)); 
PUSH(R0); 
MOV(R0,FPARG(2));
PUSH(R0); 
PUSH(IMM(3)); 
MOV(R0,IND(57)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
MOV(R5,R3+3+3); 
tail_call_copy14:
CMP(R3,R5); 
JUMP_EQ(jmpa14); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy14); 
jmpa14: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit16: //OUT OF LAMBDA 
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
tail_call_copy15:
CMP(R3,R5); 
JUMP_EQ(jmpa15); 
 MOV(STACK(R2),STACK(R3)); 
 ADD(R3,1); 
 ADD(R2,1); 
 JUMP(tail_call_copy15); 
jmpa15: 
MOV(SP,R2); 
JUMPA(INDD(R0,2)); 
POP(FP);
RETURN;
exit14: //OUT OF LAMBDA 

 MOV(IND(60), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
/* IN DEFINE*/
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
MOV(INDD(R0,2),LABEL(lambda17));
JUMP(exit17); 
lambda17:
PUSH(FP);
MOV(FP,SP);
MOV(R0,FPARG(2));

CMP(R0,FALSE);

JUMP_EQ(else13);
MOV(R0,IMM(4)); 

JUMP(if_exit13);
else13:
MOV(R0,IMM(6)); 
if_exit13:
POP(FP);
RETURN;
exit17: //OUT OF LAMBDA 

 MOV(IND(61), R0);
MOV(R0,VOID); 

 /*new expr */ 
 
//IN APPLIC 
MOV(R0,IMM(10)); 
PUSH(R0); 
MOV(R0,IMM(8)); 
PUSH(R0); 
PUSH(IMM(2)); 
MOV(R0,IND(49)); 
CMP(R0,T_UNDEFINED); 
JUMP_EQ(UNDEFINED_VARIABLE_ERROR);
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
UNDEFINED_VARIABLE_ERROR:
printf("undefined variable \n");
L_ERROR_NOT_CLOSURE:
  printf("aaa\n");
  exit(12);
}