;; loopa.s - Simple jump test, prints 'A' in a loop
  mov s0, 0x41 ; 'A'
S:
  outl s0 ;
  j S ;
