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
integer :: ierr, cantoper, i, digitos
character(len=maxlen) :: archivo, operacion, str



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
	read(entrada,*) digitos
	read(entrada,*) str
	call str2BigInt( str, digitos, num1 )
	
	
	read(entrada,*) digitos
	read(entrada,*) str
	call str2BigInt( str, digitos, num2 )

    !Asigno la operacion a realizarse dependiendo de lo que diga la cadena de texto
    if (operacion=="suma") then
    	call BigInt2str( str, num1 )
    	print*, str
    	call BigInt2str( str, num2 )
    	print*, str
    	
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
    endif
    !if( allocated(num1%Digs) ) deallocate(num1%Digs)
    !if( allocated(num2%Digs) ) deallocate(num2%Digs)
enddo


contains

	subroutine str2BigInt(str, nDig, num)
		integer, intent(in):: nDig
		character (len=*), intent (in) :: str
		integer :: i
		type(BigInt), intent(out) :: num
		if( allocated(num%Digs) ) deallocate(num%Digs)
		num%nDig = nDig
		allocate( num%Digs(num%nDig) )
		num%nDig = nDig
		do i=1, nDig
			num%Digs(nDig-i+1) = ichar( str(i:1) )
		end do
		
	end subroutine str2BigInt
	
	subroutine BigInt2str(str, num)
		character (len=maxlen), intent (out) :: str
		integer :: i, nDig
		type(BigInt), intent(in) :: num
		nDig = num%nDig
		str = ""
		do i=1, nDig
			str = trim(str) // char( num%Digs(nDig-i+1) )
		end do
		
	end subroutine BigInt2str


end program main
