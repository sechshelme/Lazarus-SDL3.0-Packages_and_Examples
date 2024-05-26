echo 
echo ======== Linux ========
echo 
rm main
gcc -o main main.cpp 
./main
echo 
echo ======== Windows ========
echo 
rm main.exe
x86_64-w64-mingw32-gcc main.cpp -o main.exe
wine main.exe




