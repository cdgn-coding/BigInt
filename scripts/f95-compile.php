<?php
$dir_obj = './obj';
$dir_bin = './bin';

print("1. Compilar main.f95\n");
print("2. Compilar libreriaBigInt.f95\n");
print("3. Linkear y crear ejecutable\n");
print("Hacer opcion: ");
fscanf(STDIN, "%d\n", $opcion);
($opcion>=1 && $opcion<=3) or die("La opcion es inválida\n");


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
		is_dir($dir_bin) or die("No existe el directorio obj en la carpeta raiz\n");
		$s = "f95 ./obj/libreriaBigInt.o ./obj/main.o -o ./bin/release.out";
		$r = shell_exec($s);
		print($r);
	break;
}


?>
