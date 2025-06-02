;; wozmon.s - a "bootloader" and memory editor
;; heavily inspired by the original wozmon

  mov s10, 0xff00 ; offset of first address
  mov s30, 0x3e20 ; "> "
  outh s30 ;
  outl s30 ;
INPUT:
;; s0 - offset into buffer
;; s1 - input byte
;; s2 - nibble selector
;; s3 - in progress word
  mov s0, 0 ; counter
  mov s30, 0 ;
  mov [s10 + 0], s30 ; reset
  mov [s10 + 2], s30 ;
INPUT_CONT:
  mov s1, 0 ; input
  mov s2, 0 ; which nibble (0, 1, 2, 3)
  mov s3, 0 ; in progress byte value
INPUT_LOOP:
  inl s1 ;
  jeq INPUT_LOOP ;
  cmp s1, 0xa ; newline
  jeq GO ; process command
  cmp s1, 0x3a ; ':'
  jeq EDIT ;
  cmp s1, 0x52 ; 'R'
  jne DONT_R ;
  mov s30, [s10 + 0] ;
  jr s30 ;
DONT_R:
  cmp s1, 0x2e ; '.'
  jne OK ;
DOT:
  inl s1 ;
  jeq DOT ;
OK:
  cmp s1, 0x39 ; '9'
  jgt HEX ;
  mov s30, 0xffd0 ; -'0'
  add s1, s1, s30 ;
  j WRITE ;
HEX:
  mov s30, 0xffc9 ; -'A'
  add s1, s1, s30 ;
WRITE:
  cmp s2, 0 ;
  jeq WRITE_0 ;
  cmp s2, 2 ;
  jlt WRITE_1 ;
  jeq WRITE_2 ;
  add s3, s3, s1 ;
  add s30, s10, s0 ; compute offset
  mov [s30 + 0], s3 ;
  mov s30, 2 ;
  add s0, s0, s30 ;
  j INPUT_CONT ;
WRITE_0:
  mov s30, 0x1000 ; 2 ** 12
  mul s3, s1, s30 ; << 12
  mov s2, 1 ;
  j INPUT_LOOP ;
WRITE_1:
  mov s30, 0x100 ; 2 ** 8
  mul s31, s1, s30 ; << 8
  add s3, s3, s31 ;
  mov s2, 2 ;
  j INPUT_LOOP ;
WRITE_2:
  mov s30, 0x10 ; 2 ** 4
  mul s31, s1, s30 ; << 4
  add s3, s3, s31 ;
  mov s2, 3 ;
  j INPUT_LOOP ;

GO:
;; s0 - data to print
;; s1 - mode (0 is print address, 1 is dereferencing)
;; s2 - offset to inspect
  mov s0, [s10 + 0] ; first we print address
  mov s1, 0 ;
PRINT:
  mov s20, 0 ; nibble index
GO_LOOP:
  cmp s20, 0 ;
  jeq O0 ;
  cmp s20, 2 ;
  jlt O1 ;
  jeq O2 ;
  cmp s20, 4 ;
  jeq PRINT_DONE ;
  mov s31, s0 ;
  j O ;
O0:
  mov s30, 0x1000 ; 2 ** 12
  div s31, s0, s30 ; first nibble
  j O ;
O1:
  mov s30, 0x100 ; 2 ** 8
  div s31, s0, s30 ; second nibble
  j O ;
O2:
  mov s30, 0x10 ; 2 ** 4
  div s31, s0, s30 ; third nibble
  j O ;
O:
  mov s30, 0xF ;
  and s31, s31, s30 ;
  cmp s31, 0x9 ;
  jgt WALPH ;
  mov s30, 0x30 ; '0'
  j W ;
WALPH:
  mov s30, 0x37 ; 'A' - 0xa
W:
  add s31, s31, s30 ;
  outl s31 ;
  mov s30, 1 ;
  add s20, s20, s30 ;
  j GO_LOOP ;
PRINT_DONE:
  cmp s1, 0 ;
  jne DEREF ;
  mov s30, 0x3a20 ; ": "
  outh s30 ;
  outl s30 ;
  mov s1, 1 ;
  mov s2, s0 ;
  mov s0, [s2 + 0] ;
  j PRINT ;
DEREF:
  mov s30, [s10 + 2] ;
  cmp s30, 0 ; if dst is null, we don't print a range
  jeq END ;
  cmp s2, s30 ;
  jge END ;
  mov s31, 2 ;
  add s2, s2, s31 ;
  mov s0, [s2 + 0] ;
  mov s30, 0x20 ;
  outl s30 ;
  j PRINT ;
END:
  mov s30, 0x0a ; "\n"
  outl s30 ;
  j 0 ;

  mov s30, 0x45 ;

EDIT:
;; s0 - current address to write to
;; s1 - nibble idx
;; s2 - nibble
;; s3 - word to write
  mov s0, [s10 + 0] ;
EDIT_LOOP:
  mov s1, 0 ;
  mov s2, 0 ;
  mov s3, 0 ;
ECONT:
  inl s2 ;
  jeq ECONT ;
  cmp s2, 0xa ; newline
  jeq 0 ;
  cmp s2, 0x39 ; '9'
  jgt EH ;
  mov s31, 0xffd0 ; -'0'
  j EW ;
EH:
  mov s31, 0xffc9 ; 0xA-'A'
EW:
  add s2, s2, s31 ;
  cmp s1, 0 ;
  jeq EW0 ;
  cmp s1, 2 ;
  jlt EW1 ;
  jeq EW2 ;
  add s3, s3, s2 ;
  mov [s0 + 0], s3 ;
  mov s31, 2 ;
  add s0, s0, s31 ;
  j EDIT_LOOP ;
EW0:
  mov s30, 0x1000 ;
  mul s3, s2, s30 ;
  mov s1, 1 ;
  j ECONT ;
EW1:
  mov s30, 0x100 ;
  mul s31, s2, s30 ;
  add s3, s3, s31 ;
  mov s1, 2 ;
  j ECONT ;
EW2:
  mov s30, 0x10 ;
  mul s31, s2, s30 ;
  add s3, s3, s31 ;
  mov s1, 3 ;
  j ECONT ;
DUMB:
  outl s30 ;
  j DUMB ;

