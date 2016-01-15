/* primitive implementations */
#define FUNC_START \
  PUSH(FP); \
  MOV(FP,SP);
#define FUNC_END \
  POP(FP); \
  RETURN;
#define PARAM(i) FPARG(i+2) 
#define NUMBER FPARG(1)
#define CHECK_ARGS(HAVE) \
  CMP(NUMBER,HAVE); \
  JUMP_NE(ERROR_NUMBER_OF_ARGS);
#define CHECK_TYPE(type,obj)  \
  CMP(type, IND(obj)); \
  JUMP_NE(ERROR_TYPE_MIS_MATCH);
/*this should be a macro for binary integer operations */
#define BINARY_ARTH(OPP) \
  FUNC_START;\
  CHECK_ARGS(2);\
  CHECK_TYPE(T_INTEGER, PARAM(0));\
  CHECK_TYPE(T_INTEGER, PARAM(1));\
  MOV(R0,PARAM(0)); \
  MOV(R0,INDD(R0,1)); \
  MOV(R1,PARAM(1)); \
  MOV(R1,INDD(R1,1)); \
  OPP(R0,R1); \
  PUSH(R0); \
  CALL(MAKE_SOB_INTEGER); \
  DROP(1); \
  FUNC_END; \
/*
#define VARIADIC_ARTH(OPP,INIT) /
  FUNC_START;/
  MOV(R0,INIT);/
  MOV(R1,0);/
  L_v_OPP_loop: /
  CMP(R1,NUMBER); /
  JUMP_EQ(L_v_plus_func_end);/
  MOV(R2,PARAM(R1));/
  CHECK_TYPE(T_INTEGER,R2);/
  OPP(R0,INDD(R2,1));/
  INCR(R1);/
  JUMP(L_v_plus_loop)
  L_v_OPP_func_end:/
  PUSH(R0);/
  CALL(MAKE_SOB_INTEGER);/
  DROP(1);/
  FUNC_END;/
*/
L_cons:
FUNC_START;
CHECK_ARGS(2);
PUSH(PARAM(1));
PUSH(PARAM(0));
CALL(MAKE_SOB_PAIR);
DROP(2);
FUNC_END

L_car:
FUNC_START
CHECK_ARGS(1);
CHECK_TYPE(T_PAIR, PARAM(0));
MOV(R0,PARAM(0));
MOV(R0, INDD(R0,1));
FUNC_END

L_cdr:
FUNC_START
CHECK_ARGS(1);
CHECK_TYPE(T_PAIR, PARAM(0));
MOV(R0,PARAM(0));
MOV(R0, INDD(R0,2));
FUNC_END



L_plus: 
BINARY_ARTH(ADD);

L_minus: 
BINARY_ARTH(SUB);

L_mul:
BINARY_ARTH(MUL);

#define return_bool(param,bool,label_name) \
  CMP(param,bool); \
  MOV(R0,TRUE); \
  JUMP_EQ(label_name); \
  MOV(R0,FALSE); \

L_is_eq:
  FUNC_START;
  CHECK_ARGS(2);
  return_bool(PARAM(0),PARAM(1),L_is_eq_end);
  L_is_eq_end:
  FUNC_END; 
L_is_zero:
FUNC_START;
CHECK_ARGS(1);
CHECK_TYPE(T_INTEGER,PARAM(0));
MOV(R0,PARAM(0));
return_bool(INDD(R0,1), IMM(0),finish_is_zero);
finish_is_zero:
FUNC_END

L_is_null:
FUNC_START;
CHECK_ARGS(1);
return_bool(PARAM(0),NIL,finish_is_nil);
finish_is_nil:
FUNC_END; 


L_is_pair:
FUNC_START;
CHECK_ARGS(1);
return_bool(PARAM(0),T_PAIR,finish_is_pair);
finish_is_pair:
FUNC_END;

L_is_bool:
FUNC_START;
CHECK_ARGS(1);
return_bool(PARAM(0),T_BOOL,finish_is_bool);
finish_is_bool:
FUNC_END;

L_is_proc:
FUNC_START;
CHECK_ARGS(1);
return_bool(PARAM(0),T_CLOSURE,finish_is_proc);
finish_is_proc:
FUNC_END;

L_is_string:
FUNC_START;
CHECK_ARGS(1);
return_bool(PARAM(0),T_STRING,finish_is_string);
finish_is_string:
FUNC_END;

L_is_symbol:
FUNC_START;
CHECK_ARGS(1);
return_bool(PARAM(0), T_SYMBOL, finish_is_symbol);
finish_is_symbol:
FUNC_END;

L_is_vector:
FUNC_START;
CHECK_ARGS(1);
return_bool(PARAM(0), T_VECTOR, finish_is_vector);
finish_is_vector:
FUNC_END;

L_vector_len:
FUNC_START;
CHECK_ARGS(1);
BP;
CHECK_TYPE(T_VECTOR,PARAM(0));
MOV(R0,PARAM(0));
MOV(R0,INDD(R0,1));
FUNC_END;

L_string_len:
FUNC_START;
CHECK_ARGS(1);
CHECK_TYPE(T_STRING,PARAM(0));
MOV(R0,PARAM(0));
MOV(R0,INDD(R0,1));
FUNC_END;

L_vector_ref:
FUNC_START;
CHECK_ARGS(2);
CHECK_TYPE(T_VECTOR,PARAM(0));
CHECK_TYPE(T_INTEGER,PARAM(1));
MOV(R1,PARAM(0));
MOV(R2,INDD(R1,1));
MOV(R3,PARAM(1));
MOV(R3,INDD(R3,1));
CMP(R3,R2);
JUMP_GE(ERROR_TYPE_MIS_MATCH);
MOV(R0,INDD(R1,2+R3));
FUNC_END;



FUNC_END;

L_is_list:
FUNC_START;
CHECK_ARGS(1);
MOV(R1,PARAM(0));
L_is_list_loop:
CMP(R1,NIL);
JUMP_EQ(L_is_list_return_true);
CMP(IND(R1), T_PAIR);
JUMP_NE(L_is_list_return_false);
MOV(R1,INDD(R1,2));
JUMP(L_is_list_loop);
L_is_list_return_true:
MOV(R0,TRUE);
JUMP(L_is_list_end);
L_is_list_return_false:
MOV(R0,FALSE);
L_is_list_end:
FUNC_END;

/*
*  variadic plus function, foo foo
*/
L_v_plus:
FUNC_START;
MOV(R0,0);
MOV(R1,0); 
L_v_plus_loop:
CMP(R1,NUMBER);
JUMP_EQ(L_v_plus_func_end);
MOV(R2,PARAM(R1))
CHECK_TYPE(T_INTEGER,R2)
ADD(R0,INDD(R2,1))
INCR(R1)
JUMP(L_v_plus_loop)
L_v_plus_func_end:
PUSH(R0);
CALL(MAKE_SOB_INTEGER);
DROP(1);
FUNC_END;
/*variadic minus function, goo goo 
*/
L_v_minus:
FUNC_START;
CMP(NUMBER,0);
JUMP_EQ(ERROR_TYPE_MIS_MATCH)

MOV(R0,PARAM(0));
MOV(R0,INDD(R0,1))
MOV(R1,1); 
L_v_minus_loop:
CMP(R1,NUMBER);
JUMP_EQ(L_v_minus_func_end);
MOV(R2,PARAM(R1))
CHECK_TYPE(T_INTEGER,R2)
SUB(R0,INDD(R2,1))
INCR(R1)
JUMP(L_v_minus_loop)
L_v_minus_func_end:
PUSH(R0);
CALL(MAKE_SOB_INTEGER);
DROP(1);
FUNC_END;

/* variadic mult function go go go go */
L_v_mult:
FUNC_START;
MOV(R0,1);
MOV(R1,0); 
L_v_mult_loop:
CMP(R1,NUMBER);
JUMP_EQ(L_v_mult_func_end);
MOV(R2,PARAM(R1))
CHECK_TYPE(T_INTEGER,R2)
MUL(R0,INDD(R2,1))
INCR(R1)
JUMP(L_v_mult_loop)
L_v_mult_func_end:
PUSH(R0);
CALL(MAKE_SOB_INTEGER);
DROP(1);
FUNC_END;

L_v_less_than_old:
FUNC_START;
MOV(R1,PARAM(0));
MOV(R0,TRUE);
L_v_less_than_loop:
  MOV(R2,INDD(R1,1)); //holds previous value
  MOV(R1,INDD(R1,2));  
  CMP(R1,NIL);
  JUMP_EQ(L_v_less_than_finish);
  MOV(R3,INDD(R1,1));
  CMP(INDD(R2,1), INDD(R3,1));
  JUMP_GT(L_v_less_than_false)
  JUMP(L_v_less_than_loop);
L_v_less_than_false:
  MOV(R0,FALSE);
L_v_less_than_finish:
FUNC_END;

L_v_lt:
FUNC_START;
MOV(R1,NUMBER);
MOV(R2,1);
MOV(R0,TRUE);
L_v_lt_loop:
  CMP(R1,R2);
  JUMP_EQ(L_v_lt_finish);
  MOV(R3,PARAM(R2-1));
  MOV(R4,PARAM(R2));
  CMP(INDD(R3,1),INDD(R4,1));
  JUMP_GE(L_v_lt_false);
  INCR(R2);
  JUMP(L_v_lt_loop);
L_v_lt_false:
MOV(R0,FALSE);
L_v_lt_finish:
FUNC_END;


L_v_gt:
FUNC_START;
MOV(R1,NUMBER);
MOV(R2,1);
MOV(R0,TRUE);
L_v_gt_loop:
  CMP(R1,R2);
  JUMP_EQ(L_v_gt_finish);
  MOV(R3,PARAM(R2-1));
  MOV(R4,PARAM(R2));
  CMP(INDD(R3,1),INDD(R4,1));
  JUMP_LE(L_v_gt_false);
  INCR(R2);
  JUMP(L_v_gt_loop);
L_v_gt_false:
MOV(R0,FALSE);
L_v_gt_finish:
FUNC_END;

L_v_eq:
FUNC_START;
MOV(R1,NUMBER);
MOV(R2,1);
MOV(R0,TRUE);
L_v_eq_loop:
  CMP(R1,R2);
  JUMP_EQ(L_v_eq_finish);
  MOV(R3,PARAM(R2-1));
  MOV(R4,PARAM(R2));
  CMP(INDD(R3,1),INDD(R4,1));
  JUMP_NE(L_v_eq_false);
  INCR(R2);
  JUMP(L_v_eq_loop);
L_v_eq_false:
MOV(R0,FALSE);
L_v_eq_finish:
FUNC_END;

/*variadic div function pew pew */
L_v_div:
FUNC_START;
CMP(NUMBER,0);
JUMP_EQ(ERROR_TYPE_MIS_MATCH)

MOV(R0,PARAM(0));
MOV(R0,INDD(R0,1))
MOV(R1,1); 
L_v_div_loop:
CMP(R1,NUMBER);
JUMP_EQ(L_v_div_func_end);
MOV(R2,PARAM(R1))
CHECK_TYPE(T_INTEGER,R2)
DIV(R0,INDD(R2,1))
INCR(R1)
JUMP(L_v_div_loop)
L_v_div_func_end:
PUSH(R0);
CALL(MAKE_SOB_INTEGER);
DROP(1);
FUNC_END;


/*vector: makes a vector, duh, using MAKE_SOB_VECTOR */
L_vector:
FUNC_START;
MOV(R3,IMM(0));
MOV(R4,NUMBER);
L_vector_loop:
 CMP(R3,R4);
 JUMP_EQ(L_vector_finish)
 PUSH(PARAM(R3)); 
 INCR(R3); 
 JUMP(L_vector_loop);
L_vector_finish:
PUSH(R4)
CALL(MAKE_SOB_VECTOR);
DROP(NUMBER+1)
POP(FP);
RETURN;

/*make-string given a char and a number, it will make a string hopefully*/
L_make_string:
FUNC_START;
db;
CHECK_ARGS(2);
CHECK_TYPE(T_INTEGER,PARAM(0));
CHECK_TYPE(T_CHAR,PARAM(1));
MOV(R3,IMM(0));
MOV(R4,PARAM(0));
MOV(R4,INDD(R4,1));
MOV(R5,PARAM(1));
MOV(R5,INDD(R5,1));
L_make_string_loop:
 CMP(R3,R4);
 JUMP_EQ(L_make_string_finish)
 PUSH(R5); 
 INCR(R3); 
 JUMP(L_make_string_loop);
L_make_string_finish:
PUSH(R4)
CALL(MAKE_SOB_STRING);
DROP(R4+1)
POP(FP);
RETURN;


L_make_vector:
FUNC_START;
db;
CHECK_ARGS(2);
MOV(R3,IMM(0));
MOV(R4,PARAM(0));
MOV(R4,INDD(R4,1));
MOV(R5,PARAM(1));
L_make_vector_loop:
 CMP(R3,R4);
 JUMP_EQ(L_make_vector_finish)
 PUSH(R5); 
 INCR(R3); 
 JUMP(L_make_vector_loop);
L_make_vector_finish:
PUSH(R4)
CALL(MAKE_SOB_VECTOR);
DROP(R4+1)
POP(FP);
RETURN;

L_apply:
FUNC_START;
CHECK_ARGS(2);
CHECK_TYPE(T_CLOSURE,PARAM(0));

//ASSERT_LIST(PARAM(1));
MOV(R1,0); //counter for number of arguments
MOV(R2,FPARG(-1)); //should hold the return address
MOV(R3,FPARG(-2)); //should hold the old fp
MOV(R4,PARAM(0)); //should hold the closure
MOV(R0,PARAM(1));
L_apply_loop1:
  CMP(R0,NIL);
  JUMP_EQ(L_apply_loop1_exit);
  INCR(R1);
  PUSH(INDD(R0,1));
  MOV(R0,INDD(R0,2))
  
  JUMP(L_apply_loop1);

L_apply_loop1_exit:
//here we've pushed the members of the list in reverse order on le stack
//bm-1.....b0 m. We need to put aside le old fp and and ret
//now, we need to reverse the order of bm-1....b0 on le stack
//stackptr-1 should be the address of bm-1 and fp->b0
BP;
MOV(R5,SP);
DECR(R5); // r5 <- bm-1
MOV(R6,FP); //r6 <- b0
//since we swap, we need a temporary value, we shall use r7 (FML) 
//Sometimes, assembly makes me cry
L_apply_reverse_loop: //fml
  CMP(R6,R5); //if r4 > r6 we have finished the reverse of the list 
  JUMP_GE(L_apply_reverse_finish);
  //move *r5 into mofo r7
  MOV(R7,STACK(R5));
  MOV(STACK(R5),STACK(R6));
  MOV(STACK(R6),R7);
  INCR(R6);
  DECR(R5);
  JUMP(L_apply_reverse_loop);
L_apply_reverse_finish:
//now, if the gods are merciful, we've managed to reverse the arguments on le
//stack, so we pray for mercy.(This code is terrible , please forgive me if
//you're debugging this code)
//we, should now push [m:= number of args] on the stack! Barak OBama
PUSH(R1);// think it's in R1, I SURE DO HOPE MAN
//Now the moving the stack down should commence, FFMLFMLFMLFMLFMLFMLFM
//okokok, we also want to push thre env and ret address onto the stack
PUSH(INDD(R4,1)); //env,or I hope atleast
PUSH(R2); //ret address ^^^
//now we want a magical pointer to s, i.e the last argument apply
//We can go about this in a number of ways,me thinks that it could be the old
//fp, me also hopes this is true, else we need MOAR ASSAMABAABALY. 
//so, the old fp is saved in, if I remember correctly, R3.
//SO r3 <- first arg of apply, and fp <- start of frame we want to write
MOV(R5,FP); //r5<- bm-1
//we want while(r5!=sp) {*r3++=*r5++;} 
L_apply_writeover_frame: //christ, that's long
  CMP(R5,SP);
  JUMP_EQ(L_apply_write_fin)  
  MOV(STACK(R3),STACK(R5)); // :(
  INCR(R3);
  INCR(R5);
  JUMP(L_apply_writeover_frame)
L_apply_write_fin:
// 2+2=4 mod 12
//Now we must update the SP, else we'll have BUGS
MOV(SP,R3);
JUMPA(INDD(R4,2)); //fml

FUNC_END;

//compares two strings, deeply
COMPARE_STRINGS:
FUNC_START;
PUSH(R1);
PUSH(R2);
PUSH(R3);
PUSH(R4);
MOV(R0,FALSE);
MOV(R1, FPARG(0)); //first string
MOV(R2, FPARG(1)); //second string

CMP(INDD(R1,1),INDD(R2,1));
JUMP_NE(L_compare_strings_finish);
MOV(R3,INDD(R1,1)); //size of the strings
ADD(R3,2);
MOV(R4,2);
L_compare_strings_loop:
CMP(R4,R3);
JUMP_EQ(L_compare_strings_true)
CMP(INDD(R1,R4),INDD(R2,R4));
JUMP_NE(L_compare_strings_finish);
INCR(R4);
JUMP(L_compare_strings_loop);

L_compare_strings_true:
MOV(R0,TRUE);
L_compare_strings_finish:

POP(R4);
POP(R3);
POP(R2);
POP(R1)
FUNC_END;


L_string_to_symbol:
FUNC_START;

CHECK_ARGS(1);
CHECK_TYPE(T_STRING,PARAM(0));
PUSH(IMM(2));
CALL(MALLOC);
DROP(1);
MOV(R3,R0);
MOV(IND(R3),IMM(T_SYMBOL));
MOV(R2,PARAM(0));
MOV(R1,IND(SYMBOL_TABLE)); //points to the beginning of the symbol table

L_string_to_symbol_loop1:
CMP(IND(R1),IMM(T_UNDEFINED));
JUMP_EQ(L_string_to_symbol_not_found);
PUSH(R2);
PUSH(IND(R1));
CALL(COMPARE_STRINGS);
DROP(2);
CMP(R0,TRUE);
JUMP_EQ(L_string_to_symbol_found);
MOV(R1,INDD(R1,1));
JUMP(L_string_to_symbol_loop1)

L_string_to_symbol_not_found:
MOV(IND(R1),R2);
PUSH(IMM(2));
CALL(MALLOC);
DROP(1);
MOV(IND(R0),T_UNDEFINED)
MOV(INDD(R1,1),R0);
MOV(INDD(R3,1),R2);
JUMP(L_string_to_symbol_finish)

L_string_to_symbol_found:
MOV(INDD(R3,1),IND(R1));

L_string_to_symbol_finish:

MOV(R0,R3);
FUNC_END;

L_symbol_to_string:
FUNC_START;
CHECK_ARGS(1);
CHECK_TYPE(T_SYMBOL,PARAM(0));
MOV(R0,PARAM(0));
MOV(R0,INDD(R0,1));

FUNC_END;

ERROR_TYPE_MIS_MATCH:
printf("vooom ooom \n");
ERROR_NUMBER_OF_ARGS:
printf("errrrorrrr \n");

  exit(12);
