  mov s0, 0x0 ;
READ:
  inl s0 ;
  jeq READ ;
  outl s0 ;
  j READ ;
