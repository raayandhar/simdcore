; We wait for three header bytes on the UART: addr_hi (upper 8 bit), addr_lo (lower 8 bits), count (number of bytes that follow)
; if count == 0 then we should go to the address that boots the program, otherwise, keep reading

; I guess just assume this is where we are after power up, just move some basics into registers
RESET:
  mov  s5, 0x80; ; chat recomends I use 0x80 as the base address of UART and 0x81 as the status register (???), we can always change it later
  mov  s29, 0x0001; 
  mov  s30, 0x0100;
  mov  s31, 0xFFFF;
HEADER:
READ_HI:
  mov  s0, 0;
  inl  s0;
  test s0; 0;
  jeq  READ_HI;
  mov  s1, s0;
READ_LO:
  mov  s0, 0;
  inl  s0;
  test s0, 0;
  jeq  READ_LO;
  mov  s2, s0;
READ_CNT:
  mov  s0, 0;
  inl  s0;
  test s0, 0;
  jeq  READ_CNT;
  mov  s3, s0;
  mul  s4, s1, s30;
  add  s4, s4, s2;
  test s3, 0;
  jeq  RUN_PROG;
  mov  s7, s3;
DATA_LOOP:
  test s7, 0;
  jeq  HEADER;
READ_DATA:
  mov  s0, 0;
  inl  s0;
  test s0, 0;
  jeq  READ_DATA;
  mov  [s4+0], s0;
  add  s4, s4, s29;
  add  s7, s7, s31;
  j    DATA_LOOP;
RUN_PROG:
  jr   s4;