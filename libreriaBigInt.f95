!prueba
module libreriaBigInt
implicit none

integer*4, parameter :: BI__IGUAL = 0, BI__MENOR = -1, BI__MAYOR = 1
! el prefijo BI es para denotar que los parametros pertenecen a este modulo para que no choquen 
! en algun otro proyecto que se haga

type BigInt
	integer :: nDig !Contiene la cantidad de digitos del arreglo de digitos
	integer*4, dimension(:), allocatable :: Digs
	!Es un arreglo de tipo integer de dos bytes (short int en C) que contiene los digitos!
end type BigInt



contains
!Defino la funcion compara, que toma e1 y e2 y devuelve un c
	function compara(x,y) result(c)
	implicit none

	type(BigInt), intent(in) :: x, y
	integer*4 :: c
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






end module libreriaBigInt
