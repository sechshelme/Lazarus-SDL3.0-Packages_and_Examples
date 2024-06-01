#include <stdio.h>

struct int24 {
  unsigned int data:24;
};


int main(int argc, char *argv[])
{
  int24 int24s[4];
  printf("%i\n", &int24s[0]);
  printf("%i\n", &int24s[1]);
  printf("%i\n", &int24s[2]);
}
