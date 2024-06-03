#include <stdio.h>
#include <locale.h>
#include <time.h>

int main(void)
{
  time_t currtime;
  struct tm *timer;
  char buffer[80];

  time( &currtime );
  timer = localtime( &currtime );

  printf("\nsetlocale: %s\n", setlocale(LC_ALL, ""));
  strftime(buffer,80,"%c", timer );
  printf("Float: %f   Date: %s\n\n", 123.456, buffer) ;

  return 0;
}
