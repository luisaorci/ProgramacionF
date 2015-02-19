! Math . f90: demo some Fortran math functions
! --------------------------------------------

Program Math_test             ! Begin main program
  Real *8 :: x=-1.0, y=2.0, z=0    ! Declare variables x, y, z
  v = SQRT (x)                ! Call the Square root  function
  w = ASIN (y)                ! Call the Arcsine function
  f = LOG10 (z)               ! Call the Common logarithm function
  print *, v, w, f            ! Print x, y, z
 
End Program Math_test         ! End main program



