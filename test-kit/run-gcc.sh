filename=$1
test_id=$2

gcc $filename -o /tmp/gcc-out-$test_id.exe
/tmp/gcc-out-$test_id.exe 
echo $? > /tmp/gcc-out-$test_id.txt
cat /tmp/gcc-out-$test_id.txt
rm /tmp/gcc-out-$test_id.exe
rm /tmp/gcc-out-$test_id.txt