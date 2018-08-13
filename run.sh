dune build app/compiler.exe
_build/default/app/compiler.exe $1 > assembly.s
gcc assembly.s -o out.exe
./out.exe
echo $?

