module libreriaBigInt
implicit none

!Declaro las variables a usar
integer :: c,i
logical :: igualdad = .true.
integer, dimension(:),intent(in) :: x,y

contains
!Defino la funcion compara, que toma e1 y e2 y devuelve un c
function compara(x,y) result(c)
implicit none

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
  enddo

if(igualdad) c=0
end function compara


end module libreriaBigInt