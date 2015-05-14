!prueba
module libreriaBigInt
implicit none

!Definicion de constantes de la libreria

integer, parameter :: BI__MAXDIGS = 500
!Numero maximo de digitos en un tipo BigInt

integer*2, parameter :: BI__IGUAL = 0, BI__MENOR = -1, BI__MAYOR = 1
!Corresponde un valor numerico a una comparacion de dos tipos BigInt

!Definicion del tipo de dato BigInt
type BigInt
	integer :: nDig !Contiene la cantidad de digitos del arreglo de digitos
	integer*2, dimension(:), allocatable :: Digs
	!Es un arreglo de tipo integer de dos bytes (short int en C) que contiene los digitos
end type BigInt



contains
!Defino la funcion compara, que toma e1 y e2 y devuelve un c
	function compara(x,y) result(c)
	implicit none

	type(BigInt), intent(in) :: x, y
	integer*2 :: c
	integer :: i

	!En caso que el numero de cifras sea diferente el problema es trivial, no hay mucho que hacer
	if( x%nDig < y%nDig ) then
		c = BI__MENOR
		return
	elseif( x%nDig > y%nDig ) then
		c = BI__MAYOR
		return
	else
		!Comparamos las cifras de mayor a menor significancia
		do i = x%nDig, 1, -1
			if ( x%Digs(i)>y%Digs(i) ) then
				c = BI__MAYOR
				return
			elseif ( x%Digs(i)<y%Digs(i) ) then
				c = BI__MENOR
				return
			else
				cycle
			end if
		end do
	  
	end if

	c = BI__IGUAL
	end function compara


	subroutine suma(x, y, z)
		type(BigInt), intent(in) :: x, y
		type(BigInt), intent(out) :: z
		integer :: i
		integer*2 :: a, b, resto
		
		if ( x%nDig > y%nDig ) then
			z%nDig = x%nDig
		else
			z%nDig = y%nDig
		end if
		!Se toma el numero de digitos mas grande de los dos, o el numero de
		!digitos del segundo, en caso que sean iguales; asi se reserva memoria
		
		
		if ( allocated(z%Digs) ) then
			deallocate(z%Digs)
		end if
		allocate( z%Digs(z%nDig) )
		
		z%Digs = 0
		!Iniciamos todos los digitos en ero
		
		!Se suma desde la cifra menor significativa a la mayor
		do i=1, z%nDig
			!Si no hay digitos disponibles en algun sumando, se conviene que es cero
			if( i <= x%nDig ) then
				a = x%Digs(i)
			else
				a = 0
			end if
			
			if( i <= y%nDig ) then
				b = y%Digs(i)
			else
				b = 0
			end if
			
			!En efecto a+b = 10q + resto, dado que 0<=a+b<=18, se tiene q=0 o bien q=1
			resto = mod(a+b, 10)
			z%Digs(i) = resto
			
			!En caso que no haya espacio para la cifra siguiente, se inicializa 
			!z de nuevo con un espacio extra para guardar tal cifra
			if ( (resto<a+b) .and. (i==z%nDig) ) then
				z = inicializar( z, z%nDig+1 )
			end if
			
			!En caso que resto<a+b, quiere decir que q=1, o sea: 10<=a+b<=18
			if( resto < a+b ) then
				!De tal manera que si sucede, sumamos 1 al siguiente espacio
				z%Digs(i+1) = z%Digs(i+1) + 1
			end if

			
		end do
		!Este seria el algoritmo intuitivo
		
	end subroutine suma

	!Esta funcion no se pide en el laboratorio, pero nos sirve para hacer un codigo mas legible
	!en las otras subrutinas como suma y resta. Inicializa un BigInt x con tamaño tam y ceros,
	!luego de ello, introduce los digitos de c en x
	function inicializar(x, tam) result(c)
		type(BigInt), intent(in) :: x
		integer, intent(in):: tam
		type(BigInt) :: c
		integer :: i
		
		c%nDig = tam
		allocate(c%Digs(tam))
		c%Digs = 0
		do i=1, x%nDig
			c%Digs(i) = x%Digs(i)
		end do
		
	end function inicializar


end module libreriaBigInt
