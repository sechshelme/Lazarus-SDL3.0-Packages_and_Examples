echo 
echo ======== Linux ========
echo 
rm main
gcc -o main main.c 
LANG="de_CH.UTF8"  ./main
LANG="de_DE.UTF8"  ./main
LANG="en_US.UTF8"  ./main
LANG="en_GB.UTF8"  ./main
echo 
echo ======== Windows ========
echo 
rm main.exe
x86_64-w64-mingw32-gcc main.c -o main.exe
LANG="de_CH.UTF8" wine main.exe
LANG="de_DE.UTF8" wine main.exe
LANG="en_US.UTF8" wine main.exe
LANG="en_GB.UTF8" wine main.exe




