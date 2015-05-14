!prueba
module libreriaBigInt
implicit none

integer*4, parameter :: BI__IGUAL = 0, BI__MENOR = -1, BI__MAYOR = 1
! el prefijo BI es para denotar que los parametros pertenecen a este modulo para que no choquen 
! en algun otro proyecto que se haga

type BigInt
	integer :: nDig !Contiene la cantidad de digitos del arreglo de digitos
	integer*2, dimension(:), allocatable :: Digs
	!Es un arreglo de tipo integer de dos bytes (short int en C) que contiene los digitos!
end type BigInt



contains
!Defino la funcion compara, que toma e1 y e2 y devuelve un c
	function compara(x,y) result(c)
	implicit none

	type(BigInt), intent(in) :: x, y
	integer*2 :: c
	integer :: i

	if( x%nDig < y%nDig ) then
		c = BI__MENOR
		return
	elseif( x%nDig > y%nDig ) then
		c = BI__MAYOR
		return
	else
	
		do i = 1, x%nDig
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
		
		
		if ( allocated(z%Digs) ) then
			deallocate(z%Digs)
		end if
		allocate( z%Digs(z%nDig) )
		
		z%Digs = 0
		
		do i=1, z%nDig
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
			
			resto = mod(a+b, 10)
			z%Digs(i) = resto
			
			if ( (resto<a+b) .and. (i==z%nDig) ) then
				z = inicializar( z, z%nDig+1 )
			end if
			
			if( resto < a+b ) then
				z%Digs(i+1) = z%Digs(i+1) + 1
			end if

			
		end do
		
		
	end subroutine suma

	! esta funcion no se pide como tal en el proyecto, pero nos sirve para las subrutinas
	!de manera de que no se repita codigo, sirve para inicializar otro BigInt de otro tamaño
	! pero con los mismos valores que c%Dig, el resto siendo 0
	! ya sea para inicializar otro mas pequeño o mas grande
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
