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
  FUNC_END; 
/****************************************/


L_cons:
FUNC_START;
CHECK_ARGS(2);
PUSH(PARAM(1));
PUSH(PARAM(0));
CALL(MAKE_SOB_PAIR);
DROP(2);

FUNC_END
/****************************************/


L_car:
FUNC_START
CHECK_ARGS(1);
CHECK_TYPE(T_PAIR, PARAM(0));
MOV(R0,PARAM(0));
MOV(R0, INDD(R0,1));
FUNC_END
/****************************************/


L_cdr:
FUNC_START
CHECK_ARGS(1);
CHECK_TYPE(T_PAIR, PARAM(0));
MOV(R0,PARAM(0));
MOV(R0, INDD(R0,2));
FUNC_END


/****************************************/



#define return_bool(param,bool,label_name) \
  CMP(param,bool); \
  MOV(R0,TRUE); \
  JUMP_EQ(label_name); \
  MOV(R0,FALSE); 
/****************************************/

L_is_eq:
  FUNC_START;
  CHECK_ARGS(2);
  CMP(PARAM(0),PARAM(1));
  MOV(R0,TRUE);
  JUMP_EQ(L_is_eq_finish);
  MOV(R1,PARAM(0));
  MOV(R2,PARAM(1));
  CMP(IND(R1),T_SYMBOL);
  JUMP_NE(L_is_eq_false);
  CMP(IND(R2),T_SYMBOL);
  JUMP_NE(L_is_eq_false);
  CMP(INDD(R1,1),INDD(R2,1));
  JUMP_NE(L_is_eq_false);
  JUMP(L_is_eq_finish); 
L_is_eq_false:
  MOV(R0,FALSE);
L_is_eq_finish:
  FUNC_END;
/****************************************/
L_is_zero:
FUNC_START;
CHECK_ARGS(1);
CHECK_TYPE(T_INTEGER,PARAM(0));
MOV(R0,PARAM(0));
return_bool(INDD(R0,1), IMM(0),finish_is_zero);
finish_is_zero:
FUNC_END
/****************************************/
L_is_null:
FUNC_START;
CHECK_ARGS(1);
return_bool(PARAM(0),NIL,finish_is_nil);
finish_is_nil:
FUNC_END; 
/****************************************/
L_is_char:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,PARAM(0));
return_bool(IND(R0),T_CHAR,finish_is_char);
finish_is_char:
FUNC_END; 
/****************************************/


/****************************************/
L_is_pair:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,PARAM(0));
return_bool(IND(R0),T_PAIR,finish_is_pair);
finish_is_pair:
FUNC_END;
/****************************************/
L_is_bool:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,PARAM(0));
return_bool(IND(R0),T_BOOL,finish_is_bool);
finish_is_bool:
FUNC_END;
/****************************************/
L_is_proc:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,PARAM(0));
return_bool(IND(R0),T_CLOSURE,finish_is_proc);
finish_is_proc:
FUNC_END;
/****************************************/
L_is_string:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,PARAM(0));
return_bool(IND(R0),T_STRING,finish_is_string);
finish_is_string:
FUNC_END;
/****************************************/
L_is_symbol:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,PARAM(0));
return_bool(IND(R0), T_SYMBOL, finish_is_symbol);
finish_is_symbol:
FUNC_END;
/****************************************/
L_is_int:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,PARAM(0));
return_bool(IND(R0), T_INTEGER, finish_is_int);
finish_is_int:

FUNC_END;
/****************************************/
L_is_vector:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,PARAM(0));
return_bool(IND(R0), T_VECTOR, finish_is_vector);
finish_is_vector:
FUNC_END;
/****************************************/
L_is_number:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,TRUE);
MOV(R1,PARAM(0));
CMP(IND(R1),T_INTEGER);
JUMP_EQ(finish_is_number);
CMP(IND(R1),T_FRACTION);
JUMP_EQ(finish_is_number);
MOV(R0,FALSE);
finish_is_number:
FUNC_END;
/****************************************/
L_numerator:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,PARAM(0))
MOV(R0,INDD(R0,1));
FUNC_END;
/****************************************/

L_denominator:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,PARAM(0))
MOV(R0,INDD(R0,2));
FUNC_END;

/****************************************/
L_is_rational:
FUNC_START;
CHECK_ARGS(1);
MOV(R0,TRUE);
MOV(R1,PARAM(0));
CMP(IND(R1),T_INTEGER);
JUMP_EQ(finish_is_number);
CMP(IND(R1),T_FRACTION);
JUMP_EQ(finish_is_rational);
MOV(R0,FALSE);
finish_is_rational:
FUNC_END;
/****************************************/
L_remainder:
FUNC_START;
CHECK_ARGS(2);
CHECK_TYPE(T_INTEGER,PARAM(0));
CHECK_TYPE(T_INTEGER,PARAM(1));
MOV(R0,PARAM(0));
MOV(R1,PARAM(1));
MOV(R0,INDD(R0,1));
MOV(R1,INDD(R1,1));
REM(R0,R1);
PUSH(R0);
CALL(MAKE_SOB_INTEGER);
DROP(1);
FUNC_END;
/****************************************/


L_vec_len:
FUNC_START;
CHECK_ARGS(1);
CHECK_TYPE(T_VECTOR,PARAM(0));
MOV(R0,PARAM(0));
MOV(R0,INDD(R0,1));
PUSH(R0);
CALL(MAKE_SOB_INTEGER);
DROP(1);
FUNC_END;
/****************************************/
L_string_len:
FUNC_START;
CHECK_ARGS(1);
CHECK_TYPE(T_STRING,PARAM(0));
MOV(R0,PARAM(0));
MOV(R0,INDD(R0,1));
PUSH(R0);
CALL(MAKE_SOB_INTEGER);
DROP(1);
FUNC_END;
/****************************************/
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
/****************************************/

L_string_ref:
FUNC_START;
CHECK_ARGS(2);
CHECK_TYPE(T_STRING,PARAM(0));
CHECK_TYPE(T_INTEGER,PARAM(1));
MOV(R1,PARAM(0));
MOV(R2,INDD(R1,1));
MOV(R3,PARAM(1));
MOV(R3,INDD(R3,1));
CMP(R3,R2);
JUMP_GE(ERROR_TYPE_MIS_MATCH);
MOV(R0,INDD(R1,2+R3));
PUSH(R0);
CALL(MAKE_SOB_CHAR);
DROP(1);
FUNC_END;
/****************************************/
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
/****************************************/
/*
*  variadic plus function, foo foo
*/





/****************************************/

L_v_plus:
FUNC_START;
MOV(R1,0);
MOV(R2,1);
MOV(R5,0);
L_plus_f_loop:
CMP(R5,NUMBER);
JUMP_EQ(L_plus_fracs_finish);
MOV(R6,PARAM(R5));
CMP(IND(R6),T_INTEGER);
JUMP_EQ(L_plus_fracs_int_case);
MOV(R3,INDD(R6,1));
MOV(R4,INDD(R6,2));
L_plus_updated:
MUL(R1,R4);
MUL(R4,R2);
MUL(R3,R2);
ADD(R1,R3);
MOV(R2,R4);
INCR(R5);
JUMP(L_plus_f_loop);
L_plus_fracs_int_case:
MOV(R3,INDD(R6,1));
MOV(R4,IMM(1));
JUMP(L_plus_updated);

L_plus_fracs_finish:
PUSH(R2);
PUSH(R1);
CALL(MAKE_SOB_FRACTION);
DROP(2);

FUNC_END;
/****************************************/

L_v_mult:
FUNC_START;
MOV(R1,1);
MOV(R2,1);
MOV(R5,0);
L_mul_f_loop:
CMP(R5,NUMBER);
JUMP_EQ(L_mul_fracs_finish);
MOV(R6,PARAM(R5));
CMP(IND(R6),T_INTEGER);
JUMP_EQ(L_mul_fracs_int_case);
MOV(R3,INDD(R6,1));
MOV(R4,INDD(R6,2));
L_mul_updated:
MUL(R1,R3);
MUL(R2,R4);
INCR(R5);
JUMP(L_mul_f_loop);
L_mul_fracs_int_case:
MOV(R3,INDD(R6,1));
MOV(R4,IMM(1));
JUMP(L_mul_updated);

L_mul_fracs_finish:
PUSH(R2);
PUSH(R1);
CALL(MAKE_SOB_FRACTION);
DROP(2);

FUNC_END;

/****************************************/
L_v_div:
FUNC_START;
MOV(R5,PARAM(0));
MOV(R1,INDD(R5,1));
MOV(R2,INDD(R5,2));
CMP(NUMBER,1);
JUMP_EQ(L_div_fracs_edge);
MOV(R5,1);
L_div_f_loop:
CMP(R5,NUMBER);
JUMP_EQ(L_div_fracs_finish);
MOV(R6,PARAM(R5));
CMP(IND(R6),T_INTEGER);
JUMP_EQ(L_div_fracs_int_case);
MOV(R3,INDD(R6,1));
MOV(R4,INDD(R6,2));
L_div_updated:
MUL(R1,R4);
MUL(R2,R3);
INCR(R5);
JUMP(L_div_f_loop);

L_div_fracs_int_case:
MOV(R3,INDD(R6,1));
MOV(R4,IMM(1));
JUMP(L_div_updated);

L_div_fracs_finish:
PUSH(R2);
PUSH(R1);
CALL(MAKE_SOB_FRACTION);
DROP(2);

FUNC_END;


L_div_fracs_edge:
CMP(IND(R5),T_FRACTION);
JUMP_NE(L_div_fracs_edge_integer);
MOV(R1,INDD(R5,2));
MOV(R2,INDD(R5,1));
JUMP(L_div_fracs_finish);
L_div_fracs_edge_integer:
MOV(R1,IMM(1));
MOV(R2,INDD(R5,1));
JUMP(L_div_fracs_finish);




/****************************************/
L_v_minus:
FUNC_START;
MOV(R5,PARAM(0));
MOV(R1,INDD(R5,1));
MOV(R2,INDD(R5,2));
CMP(NUMBER,1);
JUMP_EQ(L_sub_edge);
MOV(R5,1);
L_sub_f_loop:
CMP(R5,NUMBER);
JUMP_EQ(L_sub_fracs_finish);
MOV(R6,PARAM(R5));
CMP(IND(R6),T_INTEGER);
JUMP_EQ(L_sub_fracs_int_case);
MOV(R3,INDD(R6,1));
MOV(R4,INDD(R6,2));
L_sub_updated:
MUL(R1,R4);
MUL(R4,R2);
MUL(R3,R2);
SUB(R1,R3);
MOV(R2,R4);
INCR(R5);
JUMP(L_sub_f_loop);
L_sub_fracs_int_case:
MOV(R3,INDD(R6,1));
MOV(R4,IMM(1));
JUMP(L_sub_updated);

L_sub_fracs_finish:
PUSH(R2);
PUSH(R1);
CALL(MAKE_SOB_FRACTION);
DROP(2);
FUNC_END;

L_sub_edge:
CMP(IND(R5),T_FRACTION);
JUMP_NE(L_sub_edge_integer);
MUL(R1,-1);
JUMP(L_sub_fracs_finish);
L_sub_edge_integer:
MUL(R1,-1);
MOV(R2,IMM(1));
JUMP(L_sub_fracs_finish);
/*variadic minus function, goo goo 
*/


/************************************************/
COMPARE_NUMS:
FUNC_START;
PUSH(R1);
PUSH(R2);
PUSH(R3);
PUSH(R4);
PUSH(R5);

MOV(R1,FPARG(0));
MOV(R2,FPARG(1));
MOV(R3,INDD(R1,1));
MOV(R4,INDD(R2,1));
CMP(IND(R1),T_INTEGER);
JUMP_EQ(compare_nums_fint);
MOV(R1,INDD(R1,2));
ret_fint:
CMP(IND(R2),T_INTEGER);
JUMP_EQ(compare_nums_sint);
MOV(R2,(INDD(R2,2)));
ret_sint:
MUL(R3,R2); //a*d
MUL(R1,R4);
CMP(R1,R3);
JUMP_EQ(compare_nums_eq);
JUMP_LT(compare_nums_lt);
//else gt
MOV(R0,1);
JUMP(compare_nums_finish);

compare_nums_eq:
MOV(R0,0);
JUMP(compare_nums_finish);

compare_nums_lt:
MOV(R0,-1);
JUMP(compare_nums_finish);

compare_nums_fint:
MOV(R1,1);
JUMP(ret_fint);

compare_nums_sint:
MOV(R2,1);
JUMP(ret_sint);

compare_nums_finish:
POP(R5);
POP(R4);
POP(R3);
POP(R2);
POP(R1);
FUNC_END;
/*************************************************/
L_v_lt:
FUNC_START;
MOV(R1,NUMBER);
MOV(R2,1);
MOV(R5,TRUE);
L_v_lt_loop:
  CMP(R1,R2);
  JUMP_EQ(L_v_lt_finish);
  MOV(R3,PARAM(R2-1));
  MOV(R4,PARAM(R2));
  PUSH(R3);
  PUSH(R4);
  CALL(COMPARE_NUMS);
  DROP(2);
  CMP(R0,-1);
  JUMP_NE(L_v_lt_false);
  INCR(R2);
  JUMP(L_v_lt_loop);
L_v_lt_false:
MOV(R5,FALSE);
L_v_lt_finish:
MOV(R0,R5);
FUNC_END;
/****************************************************/
/****************************************************/

L_v_gt:
FUNC_START;
MOV(R1,NUMBER);
MOV(R2,1);
MOV(R5,TRUE);
L_v_gt_loop:
  CMP(R1,R2);
  JUMP_EQ(L_v_gt_finish);
  MOV(R3,PARAM(R2-1));
  MOV(R4,PARAM(R2));
  PUSH(R3);
  PUSH(R4);
  CALL(COMPARE_NUMS);
  DROP(2);
  CMP(R0,1);
  JUMP_NE(L_v_gt_false);
  INCR(R2);
  JUMP(L_v_gt_loop);
L_v_gt_false:
MOV(R5,FALSE);
L_v_gt_finish:
MOV(R0,R5);
FUNC_END;
/****************************************/
L_v_eq:
FUNC_START;
MOV(R1,NUMBER);
MOV(R2,1);
MOV(R5,TRUE);
L_v_eq_loop:
  CMP(R1,R2);
  JUMP_EQ(L_v_eq_finish);
  MOV(R3,PARAM(R2-1));
  MOV(R4,PARAM(R2));
  PUSH(R3);
  PUSH(R4);
  CALL(COMPARE_NUMS);
  DROP(2);
  CMP(R0,0);
  JUMP_NE(L_v_eq_false);
  INCR(R2);
  JUMP(L_v_eq_loop);
L_v_eq_false:
MOV(R5,FALSE);
L_v_eq_finish:
MOV(R0,R5);
FUNC_END;
/****************************************/



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
/****************************************/
/*make-string given a char and a number, it will make a string hopefully*/
L_make_string:
FUNC_START;
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

/****************************************/
L_make_vector:
FUNC_START;
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
/****************************************/
L_apply:
FUNC_START;
CHECK_ARGS(2);
CHECK_TYPE(T_CLOSURE,PARAM(0));

//ASSERT_LIST(PARAM(1));
MOV(R1,0); //counter for number of arguments
MOV(R2,FPARG(-1)); //should hold the return address
MOV(R8,FPARG(-2)); //should hold the old fp
//we want r3 to hold a pointer to the last argument of apply
MOV(R3,FP);
SUB(R3,IMM(4));
SUB(R3,NUMBER);
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
//Now we must update the SP, else we'll have BUGS
MOV(SP,R3);
MOV(FP,R8);

JUMPA(INDD(R4,2)); //fml

FUNC_END;
/****************************************/
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

/****************************************/
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
/****************************************/
L_symbol_to_string:
FUNC_START;
CHECK_ARGS(1);
CHECK_TYPE(T_SYMBOL,PARAM(0));
MOV(R0,PARAM(0));
MOV(R0,INDD(R0,1));

FUNC_END;
/****************************************/
L_char_to_integer:
FUNC_START;
CHECK_ARGS(1);
CHECK_TYPE(T_CHAR, PARAM(0));
MOV(R0, PARAM(0));
MOV(R0, INDD(R0,1));
PUSH(R0);
CALL(MAKE_SOB_INTEGER);
DROP(1);
FUNC_END;
/****************************************/

L_integer_to_char:
FUNC_START;
CHECK_ARGS(1);
CHECK_TYPE(T_INTEGER, PARAM(0));
MOV(R0, PARAM(0));
MOV(R0, INDD(R0,1));
PUSH(R0);
CALL(MAKE_SOB_CHAR);
DROP(1);
FUNC_END;
/****************************************/
L_set_car:
FUNC_START;
CHECK_ARGS(2);
MOV(R0,PARAM(0)); //the pair
MOV(INDD(R0,1),PARAM(1));
MOV(R0,VOID);
FUNC_END;
/****************************************/
L_set_cdr: 
FUNC_START;
CHECK_ARGS(2);
MOV(R0,PARAM(0)); //the pair
MOV(INDD(R0,2),PARAM(1));
MOV(R0,VOID);
FUNC_END;
/****************************************/
L_string_set:
FUNC_START;
CHECK_ARGS(3);
MOV(R0,PARAM(0)); // the string
MOV(R1,PARAM(1));
MOV(R1,INDD(R1,1));
ADD(R1,2);
MOV(R2,PARAM(2));
MOV(INDD(R0,R1),INDD(R2,1));
MOV(R0,VOID);
FUNC_END;
/****************************************/
L_vector_set:
FUNC_START;
CHECK_ARGS(3);
MOV(R0,PARAM(0)); // the string
MOV(R1,PARAM(1));
MOV(R1,INDD(R1,1));
ADD(R1,2);
MOV(INDD(R0,R1),PARAM(2));
MOV(R0,VOID);
FUNC_END;
/****************************************/

ERROR_TYPE_MIS_MATCH:
printf("vooom ooom \n");
ERROR_NUMBER_OF_ARGS:
printf("errrrorrrr \n");

  exit(12);
