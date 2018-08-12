_build/default/app/compiler.exe $1 > /tmp/assembly.s
gcc assembly.s -o /tmp/mycc-out.exe
/tmp/mycc-out.exe
echo $?
rm /tmp/assembly.s
rm /tmp/mycc-out.exe