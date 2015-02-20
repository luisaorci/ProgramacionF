  ! Volumen.f90: Calculates a spherical cap
  !------------------------------------

Program Volumen
  Implicit None
  ! Declarar mis variables, NO CALCULAR NADA
  Real *8 :: radio, circum, PI, vol, h
  Integer :: model_n = 2
  
  
  ! EPEZAR A realizar calculos y demas
  PI= 4.0 * atan(1.0)
  print *, 'Dame un radio'
  read *, radio
  print *, 'Dame una altura'
  read *, h
  circum= 2.0 * PI * radio
  vol= (PI*(h*h))*(radio-(h/3))
  print * , 'Program number = ' , model_n
  print *, 'Radio =' , radio
  print *, 'Altura=' , h
  print *, 'Circunferencia =' , circum
  print *, 'Volumen =' , vol
  
  
End 
