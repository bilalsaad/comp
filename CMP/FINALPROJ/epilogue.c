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
