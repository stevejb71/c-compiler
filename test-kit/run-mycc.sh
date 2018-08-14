filename=$1
test_id=$2

_build/default/app/compiler.exe $filename > /tmp/assembly-$test_id.s
gcc /tmp/assembly-$test_id.s -o /tmp/mycc-out-$test_id.exe
/tmp/mycc-out-$test_id.exe 
echo $? > /tmp/mycc-out-$test_id.txt
cat /tmp/mycc-out-$test_id.txt
rm /tmp/assembly-$test_id.s
rm /tmp/mycc-out-$test_id.exe
rm /tmp/mycc-out-$test_id.txt