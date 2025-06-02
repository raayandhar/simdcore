.org 0
  mov s0, DATA ;
PRINT:
  mov s1, [s0 + 0] ;
  cmp s1, 0 ;
  jeq END ;
  outh s1 ;
  outl s1 ;
  mov s1, 2 ;
  add s0, s0, s1 ;
  j PRINT ;
END:
  j 0x100 ; WOZMON
DATA:
  .db "DEMO LOADED\n"
  .db "AVAILABLE PROGRAMS:\n"
  .db "  0100 WOZMON\n  1000 UPPERCASE\nSTARTING WOZMON...\n"

