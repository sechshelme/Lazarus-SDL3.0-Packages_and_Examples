#include <stdio.h>

void test(int a,int b,int c)
{
  printf("addr: %i\n", &a);
  printf("addr: %i\n", &b);
  printf("addr: %i\n", &c);
}

int main(int argc, char *argv[])
{
  test(1,1,1);
  return 0;
}
