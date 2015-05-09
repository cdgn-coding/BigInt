<?php

/* Este script se encarga de compilar el codigo ya sea por partes o completamente
   De tal manera que no se tenga que ejecutar una linea larga llamando a f95 de manera repetida
   Si no existen los directorios los crea con permisos $permisos
   $dir_obj : aca se guardan los .o y los .mod generados
   $dir_bin : aca se crea el ejecutable
   $nom_ejecutable : nombre del ejecutable
   
   Se ejecuta en el terminal como: php f95-compiile.php
   */

$dir_obj = './obj';
$dir_bin = './bin';
$permisos = 0722;
$nom_ejecutable = "release.out";

print("1. Compilar main.f95\n");
print("2. Compilar libreriaBigInt.f95\n");
print("3. Linkear y crear ejecutable\n");
print("Hacer opcion: ");
fscanf(STDIN, "%d\n", $opcion);
($opcion>=1 && $opcion<=3) or die("La opcion es inválida\n");
if( !is_dir($dir_obj) ) mkdir($dir_obj, $permisos);

switch($opcion)
{
	case 1:
		$s = "f95 -c ../main.f95 -fintrinsic-modules-path ./obj -o ./obj/main.o";
		$r = shell_exec($s);
		print($r);
	break;
	
	case 2:
		$s = "f95 -c ../libreriaBigInt.f95 -o ./obj/libreriaBigInt.o -J./obj";
		$r = shell_exec($s);
		print($r);
	break;
	
	case 3:
		if( !is_dir($dir_bin) ) mkdir($dir_bin, $permisos);
		$s = "f95 ./obj/libreriaBigInt.o ./obj/main.o -o ./bin/$nom_ejecutable";
		$r = shell_exec($s);
		print($r);
	break;
}


?>
