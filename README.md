#BigInt

A short module in Fortran 95 which deals with integers that can't be stored in the traditional way.

## API documentation

**Type BigInt**

Integers are stored with the BigInt type that is composed with

* nDigs: the number of digits of the number
* Digs: the vector containing the digits

**Constants**

* BI__MAXDIGS = 500. The maximum number of digits for the type BigInt
* BI__IGUAL = 0
* BI__MENOR = -1
* BI__MAYOR = 1

**compara(x : BigInt, y : BigInt)**

Returns:
* BI_IGUAL if x == y
* BI_MENOR if x < y
* BI_MAYOR if x > y

**suma(x : BigInt, y : BigInt, y : BigInt)**

Returns z = x + y by reference

**resta(x : BigInt, y : BigInt, y : BigInt)**

Returns z = x - y by reference

##Compilation

**Using the php script**

If you have php and php-cli installed in GNU Linux

1. Open terminal/cmd and go to project directory: cd path/to/bigint/
2. Type php f95-compile.php and follow the instructions

**Using Silverfrost**

Just create a new project from this source
