!Proyecto numero 1
!Rafael Tellez (12-11397)
!Carlos David Nexans (-)

program main



use libreriaBigInt


implicit none
!Declaracion de constantes
integer, parameter :: entrada=10, salida=11, maxlen=120
!Declaración de variables
type(BigInt) :: num1, num2, resultado
integer :: ierr, cantoper, i, digitos
character(len=maxlen) :: archivo, operacion
character (len=BI__MAXDIGS) :: str



!Pedir el nombre del archivo, abrirlo y revisar su correcta apertura
print*, "Introduzca el nombre a leer: "
read*, archivo
open(unit=entrada, file=archivo, action="read", status="old", iostat=ierr)
if(ierr /= 0) then
  print*, "Error al abrir el archivo, finalizando programa."
  stop
endif


!Abro el archivo donde escribiré y reviso su correcta apertura
open(unit=salida, file="resultados.txt", action="write", status="unknown", iostat=ierr)
if(ierr /= 0) then
  print*, "Error al abrir el archivo, finalizano programa."
  stop
endif


!Leo cuantas operaciones hay que realizar para establecer el tope de mi ciclo
read(entrada, *) cantoper

do i=1,cantoper
	
	read(entrada,*) operacion
	read(entrada,*) digitos
	read(entrada,*) str
	call str2BigInt( trim(str), digitos, num1 )
	
	
	read(entrada,*) digitos
	read(entrada,*) str
	call str2BigInt( trim(str), digitos, num2 )
    !Asigno la operacion a realizarse dependiendo de lo que diga la cadena de texto
    
    if (operacion=="suma") then

		write(salida,*) trim(operacion)
	   
		call BigInt2str( str, num1 ) 
		write(salida,*) trim(str)
      
		call BigInt2str( str, num2 )
		write(salida,*) trim(str)
      
		call suma(num1, num2, resultado)
		call BigInt2str( str, resultado )
		write(salida,*) trim(str)
      
    elseif (operacion=="resta") then
    
		!write(salida,*) trim(operacion)
		!call BigInt2str( str, num1 ) 
		!write(salida,*) trim(str)
      
		!call BigInt2str( str, num2 )
		!write(salida,*) trim(str)
      
		!call resta(num1, num2, resultado)
		!call BigInt2str( str, resultado )
		!write(salida,*) trim(str)
      
      
      
    elseif (operacion == "compara") then
		write(salida,*) trim(operacion)
	   
		call BigInt2str( str, num1 ) 
		write(salida,*) trim(str)
      
		call BigInt2str( str, num2 )
		write(salida,*) trim(str)
      
		write(str, '(i2)') compara(num1,num2)
		write(salida,*) trim(str)

    else 
		print*, "Operación invalida, saltando iteración"
    endif

    if( allocated(num1%Digs) ) then
        deallocate(num1%Digs)
    end if
    if( allocated(num2%Digs) ) then
        deallocate(num2%Digs)
    end if

enddo

contains

	subroutine str2BigInt(str, nDig, num)
		integer, intent(in):: nDig
		character (len=*), intent (in) :: str
		integer :: i, j
		type(BigInt), intent(out) :: num
		num%nDig = nDig
		allocate( num%Digs(num%nDig) )
		num%nDig = nDig
		

		do i=1, nDig
			num%Digs(nDig - i + 1) = ichar( str(i:1) )-48
		end do
		
	end subroutine str2BigInt
	
	subroutine BigInt2str(str, num)
		character (len=BI__MAXDIGS), intent (out) :: str
		integer :: i, nDig
		character (len=1) :: digito
		type(BigInt), intent(in) :: num
		nDig = num%nDig
		str = ""
		do i=1, nDig
			write(digito, '(i1)') num%Digs(nDig-i+1)
			str = trim(str) // digito
		end do
		
	end subroutine BigInt2str


end program main
