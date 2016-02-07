








PRINT_EXPR:
PUSH(FP);
MOV(FP,SP);
CMP(FPARG(0),VOID);
JUMP_EQ(print_expr_finish);
PUSH(FPARG(0));
CALL(WRITE_SOB);
DROP(1);
PUSH(IMM('\n'));
CALL(PUTCHAR);
DROP(1);
print_expr_finish:
POP(FP);
RETURN;
