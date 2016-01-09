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
label_name:
 
  
L_is_zero:
FUNC_START;
CHECK_ARGS(1);
CHECK_TYPE(T_INTEGER,PARAM(0));
MOV(R0,PARAM(0));
return_bool(INDD(R0,1), IMM(0),finish_is_zero);
FUNC_END

L_is_null:
FUNC_START;
CHECK_ARGS(1);
return_bool(PARAM(0),NIL,finish_is_nil);
FUNC_END 

ERROR_NUMBER_OF_ARGS:
printf("errrrorrrr \n");
ERROR_TYPE_MIS_MATCH:
printf("vooom ooom \n");
  exit(12);

 
