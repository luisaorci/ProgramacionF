module constantes1

implicit none

   real , parameter :: pi = 3.14159265359
   real , parameter :: radianes = (pi)/180

   integer , parameter :: puntos = 5000, muestreo = 1000

   real , parameter :: densidad = 1.18, gravedad = 9.81
   real , parameter :: CoefEsfera = 0.47

end module constantes1

program proyectil

use constantes1

implicit none

real :: xInicial, yInicial, velocidadInicial, anguloInicial 
real :: xMax, yMax, tiempoFinal, xMaxFric, yMaxFric, tiempoFinalFric
real :: xmaximasifric, ymaximasifric, tiempofinalsifric
real :: errorx, errory

write (*,*) 'Introduzca la posicion inicial de tiro en "x" y "y"'
read * , xInicial
read * , yInicial
write (*,*) 'Introduzca la velocidad inicial del proyectil en m/s'
read * , velocidadInicial

write (*,*) 'Introduzca el angulo inicial en grados'
read * , anguloInicial


 
call no_friccion (xInicial, yInicial, velocidadInicial, anguloInicial, xMax, yMax, tiempoFinal)
call si_friccion (xInicial, yInicial, velocidadInicial, anguloInicial, xmaximasifric, ymaximasifric, tiempofinalsifric)

errorx = xMax-xmaximasifric
errory = yMax-ymaximasifric


Print * , "------------------------------------------------------------"
Print * , "SIN FRICCION"

Print * , "Posicion inicial del tiro en x=", xInicial, "y=", yInicial
Print * , "Con velocidad inicial de:", velocidadInicial , "m/s"
Print * , "Y un angulo inicial de:", anguloInicial , "radianes"
Print * , "Distancia horizontal maxima =", xMax, "m, y vertical = ", yMax,"m"
Print * , "Tiempo de vuelo de ", tiempoFinal, "s"

Print * , "------------------------------------------------------------"
Print * , "CON FRICCION"
Print * , "Posicion inicial del tiro en x=", xInicial, "y=", yInicial
Print * , "Con velocidad inicial de:", velocidadInicial , "m/s"
Print * , "Y un angulo inicial de:", anguloInicial , "radianes"
Print * , "Distancia horizontal maxima =", xmaximasifric, "m, y vertical =", ymaximasifric, "m"
Print * , "Tiempo de vuelo de " , tiempofinalsifric, "s"

Print * , "La diferencia de distancia horizontal entre el tiro sin friccion y el tiro con friccion es de" , errorx, "m"
Print * , "La diferencia de distancia vertical entre el tiro sin friccion y el tiro con friccion es de", errory, "m"
endprogram proyectil

















subroutine no_friccion (xInicial, yInicial, velocidadInicial, anguloInicial, xMax, yMax, tiempoVuelo)

use constantes1
implicit none

integer :: i
Real :: xInicial, yInicial, velocidadInicial, anguloInicial
Real :: xMax, yMax, tiempoVuelo, incrementoTiempo, tiempo
real :: xact, yact, vx, vy
anguloInicial = anguloInicial*radianes

xMax = xInicial + ((velocidadInicial*velocidadInicial+sin(2*anguloInicial))/(gravedad))
yMax = yInicial + (((velocidadInicial*velocidadInicial)*(sin(anguloInicial)*sin(anguloInicial)))/(2*gravedad))
tiempoVuelo = (2*velocidadInicial*sin(anguloInicial))/(gravedad)
incrementoTiempo = tiempoVuelo/muestreo

open (1, FILE = "nofriccion.dat")

!Calcular las velocidad
   vx=(velocidadInicial)*(cos(anguloInicial))
   if (vx < 0) then
      vx = -1*vx
   end if
   vy=(velocidadInicial)*(sin(anguloInicial))


tiempo = 0.0
do i = 1, muestreo

    xact = xInicial + (vx*tiempo)
    yact = yInicial + (vy*tiempo) - (0.5*gravedad*tiempo*tiempo)

    write(1,*) xact, yact
	
    tiempo = tiempo + incrementoTiempo

end do


endsubroutine








