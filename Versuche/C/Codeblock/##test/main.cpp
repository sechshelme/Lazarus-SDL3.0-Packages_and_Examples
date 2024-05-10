#include <stdio.h>

//#define FOO(x) foo_##x
//#define BAR(x) bar_##x

#define Write(x) puts(##x)

int main(int argc, char *argv[])
{
//#define paster( n ) printf( "token" #n " = %d", token##n )
//int token9 = 9;

//paster(9);

Write("Hello World !");
}
