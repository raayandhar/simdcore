;; echo.s - Echoes all input directly to output
  mov s0, 0x0 ;
READ:
  inl s0 ;
  jeq READ ; ZF is set if there was no input ready
  outl s0 ;
  j READ ;
