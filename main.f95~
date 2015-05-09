!Proyecto numero 1
!Rafael Tellez (12-11397)
!Carlos David Nexans (-)

!LIIISTO PERRITOOOOO YAYAYAJUUUUUUU!!!! Ctrl+C Ctrl+V V V V V V V V

program main
use libreriaBigInt
implicit none
!Declaración de variables
integer, dimension(:), allocatable :: e1,e2
integer :: nDig1,nDig2,ierr,cantoper,i,c,compara
character(len=120) :: archivo, operacion

!Pedir el nombre del archivo, abrirlo y revisar su correcta apertura
print*, "Introduzca el nombre del archivo con los enteros"
read*, archivo

open(unit=10,file=archivo,action="r",status="old",iostat=ierr)
if(ierr /= 0) then
  print*, "Error al abrir el archivo, finalizano programa."
  pause
  stop
endif
!Abro el archivo donde escribiré y reviso su correcta apertura
open(unit=11,file="resultados.txt",action="w",status="unknown",iostat=ierr)
if(ierr /= 0) then
  print*, "Error al abrir el archivo, finalizano programa."
  pause
  stop
endif


!Leo cuantas operaciones hay que realizar para establecer el tope de mi ciclo
read(10,*)cantoper
do i=1,cantoper
	!Leo el archivo para asignar
	read(10,*)operacion
	read(10,*)nDig1
	!Establezco la dimension de e1 para poder almacenarlo
	allocate(e1(nDig1))
	read(10,*)e1
	!Leo la cantidad de digitos del segundo numero, redimensiono e2 y lo almaceno
	read(10,*)nDig2
	allocate(e2(nDig2))
	read(10,*)e2
    print*, e1
	print*, e2

    !Asigno la operacion a realizarse dependiendo de lo que diga la cadena de texto
    if (operacion=="suma") then

      write(11,*)"suma"
      write(11,*)e1
      write(11,*)e2
      write(11,*)""
      write(11,*)" "
    elseif (operacion=="resta") then
    
      write(11,*)"resta"
      write(11,*)e1
      write(11,*)e2
      write(11,*)""
      write(11,*)" "
    elseif (operacion == "compara") then
    
      write(11,*)"compara"
      write(11,*)e1
      write(11,*)e2
      write(11,*)""
      write(11,*)compara(e1,e2)
      write(11,*)" "
    else 
      print*, "operación invalida, saltando iteración"
      cycle
    endif
enddo

endprogram main
