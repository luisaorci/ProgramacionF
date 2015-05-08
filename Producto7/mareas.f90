program Mareas

implicit none

real, dimension (7674) :: altura

integer :: i 

real :: Datos1, MaxDia1, MaxDia2, MaxDia3
real :: Maxima1, Maxima2, Maxima3, Maxima4, Maxima5
real :: Tiempo11, Tiempo12, Tiempo13, Tiempo14, Tiempo15
real :: Datos2, MinDia1, MinDia2, MinDia3
real :: Minima1, Minima2, Minima3, Minima4, Minima5
real :: Tiempo21, Tiempo22, Tiempo23, Tiempo24, Tiempo25



open (1, file = "Mareas.csv")

do i = 1, 7674
read (1, *) altura(i)
end do
close (1)

! MES UNO

Maxima1 = 0
do i = 1, 1344
Datos1 = Maxima1 - altura(i)
if (Datos1<0) then 
Maxima1 = altura(i)

Tiempo11 = i/48.0
end if
end do

Minima1 = 0
do i = 1, 1344
Datos2 = Minima1 - Altura(i)
if (Datos2>0) then
Minima1 = altura(i)

Tiempo21 = i/48.0
end if
end do

!MES DOS

Maxima2 = 0
do i = 1345, 2689
Datos1 = Maxima2 - altura(i)
if (Datos1<0) then 
Maxima2 = Altura(i)

Tiempo12 = i/48.0
end if 
end do

Minima2 = 0
do i = 1345, 2689
Datos2 = Minima2 - altura(i)
if (Datos2>0) then
Minima2 = altura(i)

Tiempo22 = i/48.0
end if
end do

!MES TRES

Maxima3 = 0
do i = 2690, 4034
Datos1 = Maxima3 - altura(i)
if (Datos1<0) then
Maxima3 = altura(i)

Tiempo13 = i/48.0
end if
end do

Minima3 = 0
do i = 2690, 4034
Datos2 = Minima3 - altura(i)
if (Datos2>0) then
Minima3 = altura(i)

Tiempo23 = i/48.0
end if
end do


!MES CUATRO

Maxima4 = 0
do i = 4035, 5379
Datos1 = Maxima4 - altura(i)
if (Datos1<0) then
Maxima4 = altura(i)

Tiempo14 = i/48.0
end if
end do

Minima4 = 0
do i = 4035, 5379
Datos2 = Minima4 - altura(i)
if (Datos2>0) then
Minima4 = altura(i)

Tiempo24 = i/48.0
end if
end do

!MES CINCO

Maxima5 = 0
do i = 5380, 6724
Datos1 = Maxima5 - altura(i)
if (Datos1<0) then
Maxima5 = altura(i)

Tiempo15 = i/48.0
end if
end do

Minima5 = 0
do i = 5380, 6724
Datos2 = Minima5 - altura(i)
if (Datos2>0) then
Minima5 = altura(i)

Tiempo25 = i/48.0
end if
end do 

! --------------------------

MaxDia1 = 0 
do i = 1, 48
Datos1 = MaxDia1 - altura(i)
if(Datos1<0) then 
MaxDia1 = altura(i)
end if 
end do

MinDia1 = 0
do i = 1, 48
Datos2 = MinDia1 - altura(i)
if (Datos2>0) then
MinDia1 = altura(i)
end if
end do

! --------------------------

MaxDia2 = 0
do i = 49, 97
Datos1 = MaxDia2 - altura(i)
if (Datos2<0) then
MaxDia2 = altura(i)
end if
end do

MinDia2 = 0 
do i = 49, 97
Datos2 = MinDia2 - altura(i)
if (Datos2>0) then
MinDia2 = altura(i)
end if 
end do

! ----------------------------

MaxDia3 = 0
do i = 98, 146
Datos1 = MaxDia3 - altura(i)
if (Datos1<0) then
MaxDia3 = altura(i)
end if
end do

MinDia3 = 0
do i = 98, 146
Datos2 = MinDia3 - altura(i)
if (Datos2>0) then 
MinDia3 = altura(i)
end if 
end do 


Print * , ' ========================================================== '
Print * , 'ALTURA MAXIMA DE LAS MAREA'
Print * , ' ========================================================== '
Print * , 'Día 1:' , MaxDia1
Print * , 'Día 2:' , MaxDia2
Print * , 'Día 3:' , MaxDia3
Print * , ' ========================================================== '
Print * , 'Mes uno:' , Maxima1 , 'En el día:' , Tiempo11
Print * , 'Mes dos:' , Maxima2 , 'En el día:' , Tiempo12
Print * , 'Mes tres:' , Maxima3 , 'En el día:' , Tiempo13
Print * , 'Mes cuatro:' , Maxima4 , 'En el día:' , Tiempo14
Print * , 'Mes cinco:' , Maxima5 , 'En el día:' , Tiempo15
Print * , ' ========================================================== '
Print * , 'ALTURA MINIMA DE LA MAREA'
Print * , ' ========================================================== '
Print * , 'Día 1:' , MinDia1
Print * , 'Día 2:' , MinDia2
Print * , 'Día 3:' , MinDia3
Print * , ' ========================================================== '
Print * , 'Mes uno:' , Minima1 , 'En el día:' , Tiempo21
Print * , 'Mes dos:' , Minima2 , 'En el día:' , Tiempo22
Print * , 'Mes tres:' , Minima3 , 'En el día:' , Tiempo23
Print * , 'Mes cuatro:' , Minima4 , 'En el día:' , Tiempo24
Print * , 'Mes cinco:' , Minima5 , 'En el día:' , Tiempo25
Print * , ' ========================================================== '

 


end program Mareas




























