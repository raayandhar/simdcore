mov s0, 0x0 ;
mov s1, 0x80 ;
mov s2, 0 ; 

LOOP:
  inl s6 ; 

  test s6, 0x4F ;    
  jeq READ  ;

  test s6, 0x30 ;    
  jeq STORE ;

  test s6, 0x40 ;    
  jeq RUN ;

  j NEXT ;       

READ:
  inl s7 ;            
  mov s8, [s7+0] ;  
  
  mov [s1+0], s7 ;   
  mov [s1+2], s8 ;  
  j NEXT ;

STORE:
  inl s7 ;            
  inl s8 ;           
  mov [s7+0], s8 ;  
  
  mov [s1+0], s7 ;  
  mov [s1+2], s8 ; 
  j NEXT ;

RUN:
  inl s9 ; 
  jr s9 ;

NEXT:
  j LOOP ;
 
