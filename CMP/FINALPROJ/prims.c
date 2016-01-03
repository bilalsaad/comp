/* This is an omplementation of the car function in scheme */

#define check_type(obj,type,error) \
  PUSH(obj);\
  CALL(type); \
  CMP(R0,0); \
  JMP_EQ(error); \
  DROP(1) 

CAR:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R0, FPARG(2));
  check_type(R0,IS_SOB_PAIR,NON_LIST_ARG_CAR);
  MOV(R0,FPARH(2));
  MOV(R0,INDD(R0,1));
  POP(FP);
  RETURN;

CDR:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R0, FPARG(2));
  check_type(R0,IS_SOB_PAIR,NON_LIST_ARG_CAR);
  MOV(R0,FPARH(2));
  MOV(R0,INDD(R0,2));
  POP(FP);
  RETURN;

PLUS:
  PUSH(FP);
  MOV(FP,SP);
  MOV(R0,FPARG(2));
  ADD(R0,FPARG(3));
  POP(FP);
  RETURN;


NON_LIST_ARG_CAR:
  exit(12);
