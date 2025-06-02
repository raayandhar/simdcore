;; uppercase.s - converts strings to uppercase (SIMD demo)
START:
  mov s30, 0x3e20 ; "> "
  outh s30 ;
  outl s30 ;
  mov s0, 0x1000 ; buffer address
  mov s1, 0 ; input byte
READC:
  inl s1 ;
  jeq READC ; wait for input
  mov [s0 + 0], s1 ;
  mov s30, 2 ;
  add s0, s0, s30 ;
  cmp s1, 0xa ; newline
  jeq GO ;
  j READC ;
GO:
  mov s1, 0x1000 ; current address
LOOP:
  cmp s0, s1 ; are we done?
  jeq START ;
  mov v0, [s1 + 0] ; read 8 bytes
  mov s2, 0x60 ; 'a'-1
  mov v1, s2 ; broadcast
  mov s2, 0x7b ; 'z'+1
  mov v2, s2 ;
  vgt v1, v0, v1 ; v1 = v0 >= 'a'
  vgt v2, v2, v0 ; v2 = v0 <= 'z'
  and v1, v1, v2 ; v1 in [a-z]
  mov s2, 0xffe0 ; -0x20
  mov v2, s2 ;
  and v1, v1, v2 ;
  add v0, v0, v1 ;
  mov [s1 + 0], v0 ;
  mov s3, 0 ; byte counter
OUT:
  mov s2, [s1 + 0] ;
  outl s2 ;
  mov s2, 1 ;
  add s1, s1, s2 ;
  add s3, s3, s2 ;
  cmp s0, s1 ;
  jeq START ;
  cmp s3, 8 ;
  jne OUT ;
  j LOOP ;
