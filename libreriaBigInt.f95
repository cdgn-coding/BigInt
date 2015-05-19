!Proyecto numero 1, Computo científico II
!Carlos David Nexans (13-10591)
!Rafael Tellez (12-11397)
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
		implicit none !Dont repeat yourself (?)
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
			a = a + z%Digs(i)
			!En efecto a+b = 10q + resto, dado que 0<=a+b<=18, se tiene q=0 o bien q=1
			resto = mod(a+b, 10)
			z%Digs(i) = resto
			
			!En caso que no haya espacio para la cifra siguiente, se inicializa 
			!z de nuevo con un espacio extra para guardar tal cifra
			if ( (resto<a+b) .and. (i==z%nDig) ) then
				z = inicializar( z, z%nDig+1 )
			end if
			
			!En caso que resto<a+b, quiere decir que q=1, o sea: 10<=a+b<=18
			if( resto<a+b ) then
				!De tal manera que si sucede, sumamos 1 al siguiente espacio
				z%Digs(i+1) = 1
			end if

			
		end do
		!Este seria el algoritmo intuitivo
		
	end subroutine suma
	
	
	
	
	subroutine resta(x, y, z)
		implicit none
		type(BigInt), intent(in) :: x, y
		type(BigInt), intent(out) :: z
		type(BigInt) :: aux ! Aux sera el numero menor entre x e y
		integer :: i, j
		integer*2 :: a, foo !foo guardara cual es mayor o menor, a sera un auxiliar	
		
		foo = compara(x, y)
		!En z, momentaneamente guardaremos el numero mayor
		
		!if (allocated(z%Digs)) then
		!	deallocate(z%Digs) !Por si esta allocado lo desallocamos ;D
		!end if ! ATENCION : <------ Aparentemente en visual studio da problemas si no se limpia la solucion
		!Antes de buildear el codigo. PENDIENTE. Los allocatables en estructuras no son estandar
		!Aparentemente el libro "aprenda fortran como si estuviera en primero" no quiso ser polemico
		!Ni hacer mucho incapie en este tipo de problemas de portbilidad
		!En el caso del gnu fortran compiler esto es OK
		
		if ( foo == 1 ) then !Si x>y
			aux%nDig = y%nDig
			allocate( aux%Digs(aux%nDig) )
			aux%Digs = y%Digs

			z%nDig = x%nDig
			allocate( z%Digs(z%nDig) )
			z%Digs = x%Digs

		else !Si x<=y
			aux%nDig = x%nDig
			allocate( aux%Digs(aux%nDig) )
			aux%Digs = x%Digs
			
			z%nDig = y%nDig
			allocate( z%Digs(z%nDig) )
			z%Digs = y%Digs
		end if
		
		do i=1, z%nDig
			if( i <= aux%nDig ) then
				a = aux%Digs(i)
			else
				a = 0
			end if !Tomamos la cifra del menor que corresponde alinear el mayor - el menor
			
			if ( z%Digs(i) - a >= 0 ) then !Si la resta es propia de los positivos
				z%Digs(i) = z%Digs(i) - a !restmos cifras y listo
			else
				!De lo contrario usamos el corolario de todas las maestras de primaria
				!"Le pedimos prestado 1 al siguiente", no nos preocupamos por no tener siguiente
				!Porque si tienen la misma cantidad de cifras y el menor tiene una cifra de mayor significado
				!Mas grande, ... entonces no seria el menor
				
				z%Digs(i) = 10 + z%Digs(i) - a
				!Que pasa si el siguiente es cero?
				!Tenemos que buscar hasta que encontremos una cifra mayor o igual que uno para restarle 1
				j = i+1
				do while( z%Digs(j) == 0 ) 
					z%Digs(j) = 9
					j=j+1
				end do
				!Al final del ciclo esta la cifra que es mayor o igual a 1
				
				z%Digs(j) = z%Digs(j) - 1
			
			end if
		
		
		end do
		
		!Ajustamos las cifras eliminando los ceros a la izquierda
		j = 0
		do while( z%Digs( z%nDig - j ) == 0 ) !Recorremos el vector al reves
			j = j + 1
		end do
		
		z = inicializar( z, z%nDig-j )
		
		deallocate( aux%Digs ) !Desreservamos este espacio de memoria que ya no utilizaremos
		!Aparentemente los compiladores de fortran no tienen como estandar hacerlo automaticamente

		
		
	end subroutine resta
	
	
	
	
	
	!Esta funcion no se pide en el laboratorio, pero nos sirve para hacer un codigo mas legible
	!en las otras subrutinas como suma y resta. Inicializa un BigInt x con tamaño tam y ceros,
	!luego de ello, introduce los digitos de c en x
	!Mas que todo para no repetir el mismo codigo varias veces ...
	function inicializar(x, tam) result(c)
		implicit none
		type(BigInt), intent(in) :: x
		integer, intent(in):: tam
		type(BigInt) :: c
		integer :: i
		
		c%nDig = tam
		allocate(c%Digs(tam))
		c%Digs = 0
		do i=1, x%nDig
			if (i>tam) then
				exit
			end if
			c%Digs(i) = x%Digs(i)
		end do
		
	end function inicializar
	
	
	!Estas subrutinas no fueron pedidas en el laboratorio pero nos sirven para no repetir codigo
	!Aparte que puede aportar algo a la libreria en si.. Mas que dejarlo en el main, hemos
	!preferido dejarlo aca
	
	!Esta subrutina toma cada caracter de la cadena leida y toma el codigo ascii para
	!calcular la cifra a la que corresponde
	subroutine str2BigInt(str, nDig, num)
		integer, intent(in):: nDig
		character (len=*), intent (in) :: str
		integer :: i
		type(BigInt), intent(out) :: num
		num%nDig = nDig
		allocate( num%Digs(num%nDig) )
		num%nDig = nDig
		

		do i=1, nDig
			num%Digs(nDig - i + 1) = ichar( str(i:1) )-48
		end do
		
	end subroutine str2BigInt
	
	!En este escribimos sobre la cadena digito el numero que corresponde y lo vamos concatenando
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


end module libreriaBigInt
