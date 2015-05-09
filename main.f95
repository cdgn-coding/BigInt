!Proyecto numero 1
!Rafael Tellez (12-11397)
!Carlos David Nexans (-)

program main



use libreriaBigInt


implicit none
!Declaracion de constantes
integer, parameter :: entrada=10, salida=11, maxlen=120
!Declaración de variables
type(BigInt) :: num1, num2
integer :: ierr, cantoper, c
character(len=maxlen) :: archivo, operacion



!Pedir el nombre del archivo, abrirlo y revisar su correcta apertura
print*, "Introduzca el nombre a leer: "
read*, archivo
open(unit=entrada, file=archivo, action="read", status="old", iostat=ierr)
if(ierr /= 0) then
  print*, "Error al abrir el archivo, finalizando programa."
  !pause
  stop
endif


!Abro el archivo donde escribiré y reviso su correcta apertura
open(unit=salida, file="resultados.txt", action="write", status="unknown", iostat=ierr)
if(ierr /= 0) then
  print*, "Error al abrir el archivo, finalizano programa."
  !pause
  stop
endif


!Leo cuantas operaciones hay que realizar para establecer el tope de mi ciclo
read(entrada, *) cantoper



end program main
