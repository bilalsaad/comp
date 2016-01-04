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
