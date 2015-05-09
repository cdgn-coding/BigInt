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
integer :: ierr, cantoper, i
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

do i=1,cantoper
	
	read(entrada,*) operacion
	read(entrada,*) num1%Ndig
	allocate( num1%Digs( num1%nDig ) )
	read(entrada,*) num1%Digs
	
	
	read(entrada,*) num2%nDig
	allocate( num2%Digs( num2%nDig ) )
	read(entrada,*) num2%Digs

    !Asigno la operacion a realizarse dependiendo de lo que diga la cadena de texto
    if (operacion=="suma") then
	!write(salida,*) operacion
      !write(salida,*) num1%Digs
      !write(salida,*) num2%Digs
      !call suma(num1, num2, s)
      !write(salida,*) s
      !write(salida,*)
      
    elseif (operacion=="resta") then
	!write(salida,*) operacion  
      !write(salida,*) num1%Digs
      !write(salida,*) num2%Digs
      !call resta(num1, num2, s)
      !write(salida,*) s
      !write(salida,*)
      
    elseif (operacion == "compara") then
	!write(salida,*) operacion    
      !write(salida,*) num1%Digs
      !write(salida,*) num2%Digs
      !write(salida,*) compara(num1,num2)
	!write(salida,*)
    else 
      print*, "Operación invalida, saltando iteración"
      cycle
    endif
enddo


end program main
