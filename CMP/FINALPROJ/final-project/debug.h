/* debug_macros.h
 * GDB-Like information for debugging the compiler.
 *
 * Author: Ben Eyal
 */

#define TRANS(x, res) \
   (((x) == T_VOID)           ? snprintf(res, 16, "%s", "T_VOID")                     \
  : ((x) == T_NIL)            ? snprintf(res, 16, "%s", "T_NIL")                      \
  : ((x) == T_BOOL)           ? snprintf(res, 16, "%s", "T_BOOL")                     \
  : ((x) == T_CHAR)           ? snprintf(res, 16, "%s", "T_CHAR")                     \
  : ((x) == T_INTEGER)        ? snprintf(res, 16, "%s", "T_INTEGER")                  \
  : ((x) == T_STRING)         ? snprintf(res, 16, "%s", "T_STRING")                   \
  : ((x) == T_SYMBOL)         ? snprintf(res, 16, "%s", "T_SYMBOL")                   \
  : ((x) == T_PAIR)           ? snprintf(res, 16, "%s", "T_PAIR")                     \
  : ((x) == T_VECTOR)         ? snprintf(res, 16, "%s", "T_VECTOR")                   \
  : ((x) == T_CLOSURE)        ? snprintf(res, 16, "%s", "T_CLOSURE")                   : snprintf(res, 16, "%ld", x))   /*\
  : ((x) == T_UNDEFINED)      ? snprintf(res, 16, "%s", "T_UNDEFINED")                \
  : ((x) == PRIM_CONS)        ? snprintf(res, 16, "%s", "PRIM_CONS")                  \
  : ((x) == PRIM_BOOLEAN)     ? snprintf(res, 16, "%s", "PRIM_BOOLEAN")               \
  : ((x) == PRIM_CAR)         ? snprintf(res, 16, "%s", "PRIM_CAR")                   \
  : ((x) == PRIM_CDR)         ? snprintf(res, 16, "%s", "PRIM_CDR")                   \
  : ((x) == PRIM_ADD)         ? snprintf(res, 16, "%s", "PRIM_ADD")                   \
  : ((x) == PRIM_MUL)         ? snprintf(res, 16, "%s", "PRIM_MUL")                   \
  : ((x) == PRIM_SUB)         ? snprintf(res, 16, "%s", "PRIM_SUB")                   \
  : ((x) == PRIM_DIV)         ? snprintf(res, 16, "%s", "PRIM_DIV")                   \
  : ((x) == PRIM_MOD)         ? snprintf(res, 16, "%s", "PRIM_MOD")                   \
  : ((x) == PRIM_LT)          ? snprintf(res, 16, "%s", "PRIM_LT")                    \
  : ((x) == PRIM_GT)          ? snprintf(res, 16, "%s", "PRIM_GT")                    \
  : ((x) == PRIM_EQ)          ? snprintf(res, 16, "%s", "PRIM_EQ")                    \
  : ((x) == PRIM_ZERO)        ? snprintf(res, 16, "%s", "PRIM_ZERO")                  \
  : ((x) == PRIM_INTEGER)     ? snprintf(res, 16, "%s", "PRIM_INTEGER")               \
  : ((x) == PRIM_NUMBER)      ? snprintf(res, 16, "%s", "PRIM_NUMBER")                \
  : ((x) == PRIM_CHAR)        ? snprintf(res, 16, "%s", "PRIM_CHAR")                  \
  : ((x) == PRIM_PROCEDURE)   ? snprintf(res, 16, "%s", "PRIM_PROCEDURE")             \
  : ((x) == PRIM_PAIR)        ? snprintf(res, 16, "%s", "PRIM_PAIR")                  \
  : ((x) == PRIM_VECTOR)      ? snprintf(res, 16, "%s", "PRIM_VECTOR")                \
  : ((x) == PRIM_STRING)      ? snprintf(res, 16, "%s", "PRIM_STRING")                \
  : ((x) == PRIM_SYMBOL)      ? snprintf(res, 16, "%s", "PRIM_SYMBOL")                \
  : ((x) == PRIM_NULL)        ? snprintf(res, 16, "%s", "PRIM_NULL")                  \
  : ((x) == PRIM_VECLEN)      ? snprintf(res, 16, "%s", "PRIM_VECLEN")                \
  : ((x) == PRIM_STRLEN)      ? snprintf(res, 16, "%s", "PRIM_STRLEN")                \
  : ((x) == PRIM_STRLEN)      ? snprintf(res, 16, "%s", "PRIM_STRLEN")                \
  : ((x) == PRIM_SETCAR)      ? snprintf(res, 16, "%s", "PRIM_SETCAR")                \
  : ((x) == PRIM_SETCDR)      ? snprintf(res, 16, "%s", "PRIM_SETCDR")                \
  : ((x) == PRIM_INTTOCHAR)   ? snprintf(res, 16, "%s", "PRIM_INTTOCHAR")             \
  : ((x) == PRIM_CHARTOINT)   ? snprintf(res, 16, "%s", "PRIM_CHARTOINT")             \
  : ((x) == PRIM_STRSET)      ? snprintf(res, 16, "%s", "PRIM_STRSET")                \
  : ((x) == PRIM_VECSET)      ? snprintf(res, 16, "%s", "PRIM_VECSET")                \
  : ((x) == PRIM_STRREF)      ? snprintf(res, 16, "%s", "PRIM_STRREF")                \
  : ((x) == PRIM_VECREF)      ? snprintf(res, 16, "%s", "PRIM_VECREF")                \
  : ((x) == PRIM_MKSTR)       ? snprintf(res, 16, "%s", "PRIM_MKSTR")                 \
  : ((x) == PRIM_MKVEC)       ? snprintf(res, 16, "%s", "PRIM_MKVEC")                 \
  : ((x) == PRIM_SYMTOSTR)    ? snprintf(res, 16, "%s", "PRIM_SYMTOSTR")              \
  : ((x) == PRIM_STRTOSYM)    ? snprintf(res, 16, "%s", "PRIM_STRTOSYM")              \
  : ((x) == PRIM_ADDREQ)      ? snprintf(res, 16, "%s", "PRIM_ADDREQ")                \
  : ((x) == PRIM_APPLY)       ? snprintf(res, 16, "%s", "PRIM_APPLY")                 \
  : ((x) == PRIM_MAP)         ? snprintf(res, 16, "%s", "PRIM_MAP")                   \
  : ((x) == 13371337)         ? snprintf(res, 16, "%s", "** MOVED **")                \
  :                             snprintf(res, 16, "%ld", (x))) */

#if DO_SHOW == 1 || DO_SHOW == 4
#define INFO {                                                                      \
  char type1[16], type2[16], type3[16], type4[16];                                  \
  printf("\n");                                                                     \
  printf("----------------------------\n");                                         \
  printf("Register Info:\n");                                                       \
  printf("----------------------------\n");                                         \
  printf("FP = %-10ld SP = %ld\n\n", FP, SP);                                       \
  TRANS(R0, type1); TRANS(R1, type2);                                               \
  printf("R0 = %-10s R1 = %s\n", type1, type2);                                     \
  TRANS(R2, type1); TRANS(R3, type2);                                               \
  printf("R2 = %-10s R3 = %s\n", type1, type2);                                     \
  TRANS(R4, type1); TRANS(R5, type2);                                               \
  printf("R4 = %-10s R5 = %s\n\n", type1, type2);                                   \
  printf("----------------------------\n");                                         \
  printf("Stack Info:\n");                                                          \
  printf("----------------------------\n");                                         \
  int i;                                                                            \
  for (i = SP - 1; i >= 0; i--) {                                                   \
    TRANS(STACK(i), type1);                                                         \
    printf("STACK[%2d] = %s\n", i, type1);                                          \
  }                                                                                 \
  printf("\n");                                                                     \
  printf("----------------------------\n");                                         \
  printf("Memory Info:\n");                                                         \
  printf("----------------------------\n");                                         \
  for (i = 0; i <= IND(0); i += 4) {                                                \
    TRANS(IND(i), type1); TRANS(IND(i + 1), type2);                                 \
    TRANS(IND(i + 2) ,type3); TRANS(IND(i + 3), type4);                             \
    printf("MEM[%4d] = %-15s MEM[%4d] = %-15s MEM[%4d] = %-15s MEM[%4d] = %-15s\n", \
        i, type1,                                                                   \
        i + 1, type2,                                                               \
        i + 2, type3,                                                               \
        i + 3, type4);                                                              \
  }                                                                                 \
  printf("\n");                                                                     \
}

#define STK {                                                                       \
  printf("----------------------------\n");                                         \
  printf("Stack Info:\n");                                                          \
  printf("----------------------------\n");                                         \
  char type1[16];                                                                   \
  int i;                                                                            \
  for (i = SP - 1; i >= 0; i--) {                                                   \
    TRANS(STACK(i), type1);                                                         \
    printf("STACK[%2d] = %s\n", i, type1);                                          \
  }                                                                                 \
  printf("\n");                                                                     \
}

#define REG {                                                                       \
  char type1[16], type2[16];                                                        \
  printf("----------------------------\n");                                         \
  printf("Register Info:\n");                                                       \
  printf("----------------------------\n");                                         \
  printf("FP = %-10ld SP = %ld\n\n", FP, SP);                                       \
  TRANS(R0, type1); TRANS(R1, type2);                                               \
  printf("R0 = %-10s R1 = %s\n", type1, type2);                                     \
  TRANS(R2, type1); TRANS(R3, type2);                                               \
  printf("R2 = %-10s R3 = %s\n", type1, type2);                                     \
  TRANS(R4, type1); TRANS(R5, type2);                                               \
  printf("R4 = %-10s R5 = %s\n\n", type1, type2);                                   \
}

#define BP { INFO; getchar(); }
#else
#define INFO {}
#define STK {}
#define REG {}
#define BP {}
#endif
