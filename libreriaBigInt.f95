module libreriaBigInt
implicit none

type BigInt
	integer :: nDig !Contiene la cantidad de digitos del arreglo de digitos
	integer*2, dimension(:), allocatable :: Digs
	!Es un arreglo de tipo integer de dos bytes (short int en C) que contiene los digitos
end type BigInt



contains
!Defino la funcion compara, que toma e1 y e2 y devuelve un c
	function compara(x,y) result(c)
	implicit none

	integer, dimension(:),intent(in) :: x
	integer, dimension(:),intent(in) :: y
	integer :: c, i
	logical :: igualdad = .true.

	if(size(x)<size(y)) then
	   c=-1
	elseif(size(x)>size(y)) then
	   c=1
	else
	  do i = 1,size(x)
		if (x(i)>y(i)) then
		  c=1
		  igualdad = .false.
		  exit
		elseif (x(i)<y(i)) then
		  c=-1
		  igualdad = .false.
		  exit
		else
		  cycle
		endif
	  enddo
	endif

	if(igualdad) c=0
	end function compara


end module libreriaBigInt
